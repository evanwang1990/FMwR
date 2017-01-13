#include "FM.h"

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
List FM(List data_, IntegerVector normalize, List fm_controls, List solver_controls, List track_controls, List model_list)
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
  List X = data_["features"];
  NumericVector target = as<NumericVector>(data_["labels"]);
  SMatrix<float> m;
  m.assign(X);
  List scales;
  if (normalize[0] > -1) {
    scales = m.scales(normalize);
  }
  scales["model.vars"] = as<CharacterVector>(X.attr("feature_names"));
  Data data;
  data.add_data(&m);
  DVector<float> tg;
  tg.assign(target);
  data.add_target(&tg);

  // init/load Model
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
  List solver      = solver_controls["solver"];
  fm.SOLVER        = solvers_map[as<string>(solver.attr("solver"))];
  fm.TASK          = tasks_map[as<string>(fm_controls["task"])];
  fm.init();

  SEXP is_model = model_list.attr("class");
  if (!Rf_isNull(is_model)) {
    List model = model_list["Model"];
    fm.w0 = (double)(model["w0"]);
    fm.w.assign(as<NumericVector>(model["w"]));
    fm.v.assign(as<NumericMatrix>(model["v"]));
  }

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
  if (!Rf_isNull(is_model)) {
    List scales_old        = model_list["Scales"];
    NumericVector tg_range = as<NumericVector>(scales_old.attr("target.range"));
    learner->min_target        = std::min(learner->min_target, tg_range[0]);
    learner->max_target        = std::max(learner->max_target, tg_range[1]);
  }
  learner->nthreads          = (int)solver_controls["nthreads"];
  learner->max_iter          = (int)solver_controls["max_iter"];
  learner->tracker.step_size = (int)track_controls["step_size"];
  learner->tracker.max_iter  = learner->max_iter;
  learner->type              = evaluations_map[as<string>(track_controls["evaluate.metric"])]; //TODO:收敛 R中检验回归或分类对应的评价标准
  learner->tracker.type      = evaluations_map[as<string>(track_controls["evaluate.metric"])];
  learner->conv_condition    = (double)track_controls["convergence"];
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
  SMatrix<float> m_t;
  if (fm.SOLVER <= ALS) {
    m.transpose(m_t);
    data.add_data(&m_t);
  }
  learner->learn(data);

  // output
  List res;
  List md = fm.save_model();
  md.attr("model.control") = fm_controls;
  md.attr("solver.control") = solver_controls;
  md.attr("track.control") = track_controls;
  md.attr("convergence") = learner->convergent;
  res["Model"] = md;

  scales.attr("target.range") = NumericVector::create(learner->min_target, learner->max_target);
  res["Scales"] = scales;

  if (learner->tracker.step_size > 0) {
    res["Trace"] = learner->tracker.save();
  }

  res.attr("class") = "FM";
  return res;
}


// [[Rcpp::export]]
NumericVector FMPredict(List newdata, bool normalize, List model_list, int max_threads)
{
  // init newdata
  List newdata_ = newdata["features"];
  SMatrix<float> m(newdata_);
  List scales;
  if (normalize) {
    scales = model_list["Scales"];
    m.normalize(scales);
  }
  Data data;
  data.add_data(&m);

  // load model
  Model fm;
  List model = model_list["Model"];
  fm.load_model(model);
  fm.nthreads = max_threads;

  // predict
  DVector<double> pred(data.num_cases);
  pred.init(0.0);
  if (fm.TASK == CLASSIFICATION) {
    fm.predict_prob(data, pred);
  } else {
    fm.predict_batch(data, pred);
    NumericVector tg_range = as<NumericVector>(scales.attr("target.range"));
    #pragma omp parallel for num_threads(max_threads)
    for (uint i = 0; i < data.num_cases; i++) {
      if (pred[i] < tg_range[0])
        pred[i] = tg_range[0];
      else if (pred[i] > tg_range[1])
        pred[i] = tg_range[1];
    }
  }

  return pred.to_rtype();
}


// [[Rcpp::export]]
NumericVector FMTrack(List newdata, List model_list, bool normalize, String type, int max_threads)
{
  // init newdata
  List newdata_ = newdata["features"];
  NumericVector newtg = as< NumericVector >(newdata["labels"]);
  SMatrix<float> m(newdata_);
  List scales = model_list["Scales"];
  if (normalize) {
    m.normalize(scales);
  }
  Data data;
  data.add_data(&m);
  DVector<float> tg;
  tg.assign(newtg);
  data.add_target(&tg);

  // load model
  Model fm;
  List model = model_list["Model"];
  fm.load_model(model);
  fm.nthreads = max_threads;

  // init validator
  map<string, int> evaluations_map;
  evaluations_map["LL"  ] =  000;
  evaluations_map["AUC" ] =  111;
  evaluations_map["ACC" ] =  222;
  evaluations_map["RMSE"] =  333;
  evaluations_map["MSE" ] =  444;
  evaluations_map["MAE" ] =  555;

  Tracker tracker;
  List trace = model_list["Trace"];
  List trace_ = trace["trace"];
  tracker.load(&fm, trace_);
  tracker.type = evaluations_map[type];
  NumericVector tg_range = as<NumericVector>(scales.attr("target.range"));
  tracker.report(&fm, data, tg_range[0], tg_range[1]);

  return tracker.evaluations_of_test.to_rtype();
}
