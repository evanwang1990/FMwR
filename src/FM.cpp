#include "FM.h"

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
List FM(List data_, NumericVector target, List fm_controls, List solver_controls, List validation_controls)
{
  // init Data
  SMatrix<float> m(data);
  Data data;
  data.add(&m);
  DVector<float> tg;
  tg.assign(target);
  data.add_target(&target);

  // init Model
  Model fm;
  List hyper_params = fm_controls["hyper.params"];
  fm.k0            = (bool)hyper_params["keep.w0"];
  fm.l2_reg0          = (double)hyper_params["L2.w0"];
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
  switch ((String)fm_controls["solver"]) {
    case "SGD" : fm.SOLVER = SGD ; break;
    case "TDAP": fm.SOLVER = TDAP; break;
    case "FTRL": fm.SOLVER = FTRL; break;
    case "MCMC": fm.SOLVER = MCMC; break;
    case "ALS" : fm.SOLVER = ALS ; break;
    default    : stop("Unknown solver...");
  }
  swtich ((String)fm_controls["task"]) {
    case "CLASSIFICATION" : fm.TASK = CLASSIFICATION; break;
    case "REGRESSION"     : fm.TASK = REGRESSION    ; break;
    default               : stop("Unknown task...");
  }
  fm.init();

  // init Metainfo
  DataMetaInfo meta(data.num_features); //TODO; varclus

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
  switch ((String)solver_controls["evaludation"]) { //TODO:收敛 R中检验回归或分类对应的评价标准
    case "AUC"  : learner->type = AUC ; learner->tracker.type = AUC ; break;
    case "ACC"  : learner->type = ACC ; learner->tracker.type = ACC ; break;
    case "LL"   : learner->type = LL  ; learner->tracker.type = LL  ; break;
    case "RMSE" : learner->type = RMSE; learner->tracker.type = RMSE; break;
    case "MAE"  : learner->type = MAE ; learner->tracker.type = MAE ; break;
  }
  List solver = solver_controls["solver"];
  switch (fm->SOLVER) {
    case MCMC :
    case ALS  : {
      learner->alpha_0   = (double)solver["alpha_0"];
      learner->gamma_0   = (double)solver["gamma_0"];
      learner->beta_0    = (double)solver["beta_0"];
      learner->mu_0      = (double)solver["mu_0"];
      learner->alpha     = (double)solver["alpha"];
      learner->w0_mean_0 = (double)solver["w0_mean_0"];
      break;
    }
    case SGD  : {
      learner->learn_rate  = (double)solver["learn_rate"];
      learner->random_step = (int)solver["random_step"];
      break;
    }
    case FTRL : {
      learner->alpha_w     = (double)solver["alpha_w"];
      learner->alpha_v     = (double)solver["alpha_v"];
      learner->beta_w      = (double)solver["beta_w"];
      learner->beta_v      = (double)solver["beta_v"];
      learner->random_step = (int)solver["random_step"];
      break;
    }
    case TDAP : {
      learner->gamma       = (double)solver["gamma"];
      leaner->alpha_w      = (double)solver["alpha_w"];
      learner->alpha_v     = (double)solver["alpha_v"];
      learner->random_step = (int)solver["random_step"];
      break;
    }
    default: stop("Unknown solver...")
  }
  learner.init()

  // train model
  learner.learn(data);

  // output
  if (learner->tracker.step_size > 0) {
    return List::create(
      _["Model"] = fm.save_model(),
      _["Validation"] = List::create(
        _["trace"] = tracker.save(),
        _["eval.train"] = learner->tracker.evaluations_of_train.to_rtype()
      )
    )
  } else {
    return List::create(_["Model"] = fm.save_model()) //TODO:save_model 优化
  }
}
