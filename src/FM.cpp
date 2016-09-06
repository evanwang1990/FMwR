#include "FM.h"

using namespace Rcpp;
using namespace std;


// [[Rcpp::export]]
List FM(List data_, NumericVector target, List fm_controls, List solver_controls, List validation_controls)
{
  map<string, int> tasks_map;
  tasks_map["CLASSIFICATION"] = 10;
  tasks_map["REGRESSION"    ] = 20;
  tasks_map["RANKING"       ] = 30;

  map<string, int> solvers_map;
  solvers_map["MCMC"] = 100;
  solvers_map["ALS" ] = 200;
  solvers_map["SGD" ] = 300;
  solvers_map["FTRL"] = 400;
  solvers_map["TDAP"] = 500;

  map<string, int> evaluations_map;
  evaluations_map["LL"  ] =  000;
  evaluations_map["AUC" ] =  111;
  evaluations_map["ACC" ] =  222;
  evaluations_map["RMSE"] =  333;
  evaluations_map["MSE" ] =  444;
  evaluations_map["MAE" ] =  555;

  // init Data
  SMatrix<float> m(data_);
  Data data;
  data.add_data(&m);
  DVector<float> tg;
  tg.assign(target);
  data.add_target(&tg);

  // init Model
  Model fm;
  List hyper_params = fm_controls["hyper.params"];
  fm.k0            = (bool)hyper_params["keep.w0"];
  fm.l2_reg0       = (double)hyper_params["L2.w0"];
  fm.k1            = (bool)hyper_params["keep.w1"];
  fm.l1_regw       = (double)hyper_params["L1.w1"];
  fm.l2_regw       = (double)hyper_params["L2.w1"];
  fm.num_factor    = (uint)hyper_params["factor.number"];
  fm.init_mean     = (double)hyper_params["v.init_mean"];
  fm.init_stdev    = (double)hyper_params["v.init_stdev"];
  fm.l1_regv       = (double)hyper_params["L1.v"];
  fm.l2_regv       = (double)hyper_params["L2.v"];
  fm.nthreads      = (int)fm_controls["nthreads"];
  fm.num_attribute = data.num_features;
  fm.SOLVER        = solvers_map[as<string>(fm_controls["solver"])];
  fm.TASK          = tasks_map[as<string>(fm_controls["task"])];
  fm.init();

  // init Metainfo
  DataMetaInfo meta(data.num_features); //TODO: set variables' groups

  // init Learner
  Learner* learner;
  switch (fm.SOLVER) {
    case MCMC: learner = new MCMC_Learner(); break;
    case ALS : learner = new ALS_Learner() ; break;
    case SGD : learner = new SGD_Learner() ; break;
    case TDAP: learner = new TDAP_Learner(); break;
    case FTRL: learner = new FTRL_Learner(); break;
    default  : stop("Unknown solver...");
  }
  learner->meta              = &meta;
  learner->fm                = &fm;
  learner->min_target        = min(target);
  learner->max_target        = max(target);
  learner->nthreads          = (int)solver_controls["nthreads"];
  learner->max_iter          = (int)solver_controls["max_iter"];
  learner->tracker.step_size = (int)validation_controls["step_size"];
  learner->tracker.max_iter  = learner->max_iter;
  learner->type              = evaluations_map[as<string>(solver_controls["evaluation"])]; //TODO:收敛 R中检验回归或分类对应的评价标准
  learner->tracker.type      = evaluations_map[as<string>(solver_controls["evaluation"])];
  learner->conv_condition    = (double)solver_controls["convergence"];
  List solver = solver_controls["solver"];
  switch (fm.SOLVER) {
    case MCMC : {
      ((MCMC_Learner*)learner)->alpha_0   = (double)solver["alpha_0"];
      ((MCMC_Learner*)learner)->gamma_0   = (double)solver["gamma_0"];
      ((MCMC_Learner*)learner)->beta_0    = (double)solver["beta_0"];
      ((MCMC_Learner*)learner)->mu_0      = (double)solver["mu_0"];
      ((MCMC_Learner*)learner)->alpha     = (double)solver["alpha"];
      ((MCMC_Learner*)learner)->w0_mean_0 = (double)solver["w0_mean_0"];
      break;
    }
    case ALS  : {
      ((ALS_Learner*)learner)->alpha_0   = (double)solver["alpha_0"];
      ((ALS_Learner*)learner)->gamma_0   = (double)solver["gamma_0"];
      ((ALS_Learner*)learner)->beta_0    = (double)solver["beta_0"];
      ((ALS_Learner*)learner)->mu_0      = (double)solver["mu_0"];
      ((ALS_Learner*)learner)->alpha     = (double)solver["alpha"];
      ((ALS_Learner*)learner)->w0_mean_0 = (double)solver["w0_mean_0"];
      break;
    }
    case SGD  : {
      ((SGD_Learner*)learner)->learn_rate  = (double)solver["learn_rate"];
      ((SGD_Learner*)learner)->random_step = (int)solver["random_step"];
      break;
    }
    case FTRL : {
      ((FTRL_Learner*)learner)->alpha_w     = (double)solver["alpha_w"];
      ((FTRL_Learner*)learner)->alpha_v     = (double)solver["alpha_v"];
      ((FTRL_Learner*)learner)->beta_w      = (double)solver["beta_w"];
      ((FTRL_Learner*)learner)->beta_v      = (double)solver["beta_v"];
      ((FTRL_Learner*)learner)->random_step = (int)solver["random_step"];
      break;
    }
    case TDAP : {
      ((TDAP_Learner*)learner)->gamma       = (double)solver["gamma"];
      ((TDAP_Learner*)learner)->alpha_w     = (double)solver["alpha_w"];
      ((TDAP_Learner*)learner)->alpha_v     = (double)solver["alpha_v"];
      ((TDAP_Learner*)learner)->random_step = (int)solver["random_step"];
      break;
    }
    default: stop("Unknown solver...");
  }

  learner->init();

  // train model
  learner->learn(data);

  // output
  List res;
  if (learner->tracker.step_size > 0) {
    NumericVector eval_train(learner->tracker.record_cnter);
    for (int ri = 0; ri < learner->tracker.record_cnter; ri++)
    {
      eval_train[ri] = learner->tracker.evaluations_of_train[ri];
    }

    res = List::create(
      _["Model"]       = fm.save_model(),
      _["Convergence"] = learner->convergent,
      _["Validation"]  = List::create(
        _["trace"]            = learner->tracker.save(),
        _["evaluation.train"] = eval_train
      )
    );
  } else {
    res = List::create(_["Model"]       = fm.save_model(),
                       _["Convergence"] = learner->convergent); //TODO:save_model 优化
  }
  res.attr("class") = "FM";
  return res;
}
