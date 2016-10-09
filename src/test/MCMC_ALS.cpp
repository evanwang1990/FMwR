#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

/***R/
setwd("/home/jinlingwang/project/github/FMwR/src/test/")

data(iris)
iris1 <- iris
iris$Species <- ifelse(iris$Species == "versicolor", 1, -1)
iris <- as.matrix(iris)
iris[,1:4] <- apply(iris[,1:4], 2, scale)
res <- test(iris[,1:4], iris[,5], 1, 300, 1, 0.01); res[[1]]
 t(res[[1]]$v) %*% res[[1]]$v

summary(pnorm(res[[2]][iris[,5] > 0]))
summary(pnorm(res[[2]][iris[,5] < 0]))

 library(ggplot2)
 iris1$Species <- ifelse(iris1$Species == "versicolor", 1, -1)
 iris1$k <- iris1$Sepal.Width*iris1$Petal.Width
 fit0 <- glm(as.factor(Species) ~ Sepal.Length + Sepal.Width + Petal.Length +Petal.Width, data = iris1, family = binomial(link = "logit"))
 fit1 <- glm(as.factor(Species) ~ Sepal.Length + Sepal.Width + Petal.Length +Petal.Width + Sepal.Width * Petal.Length, data = iris1, family = binomial(link = "logit"))
 fit2 <- glm(as.factor(Species) ~ Sepal.Length + Sepal.Width + Petal.Length +Petal.Width + Petal.Width * Petal.Length, data = iris1, family = binomial(link = "logit"))
 x <- data.frame(y = iris[,5], y_hat = pnorm(res[[2]]), y_hat2 = predict(fit1, iris1, type = "response"))
 ggplot(data = x) +
 geom_density(aes(x = y_hat, color = factor(y))) +
 geom_density(aes(x = y_hat2, color = factor(y+10)))


 airq <- airquality[complete.cases(airquality),]
 airq <- as.matrix(airq)
 airq[,1] <- ifelse(airq[,1] > 60, 1, -1)
 airq[,2:6] <- apply(airq[,2:6], 2, scale)
 res <- test(airq[,2:6], airq[,1], 1, 300, 1, 0.01); res[[1]]
 summary(pnorm(res[[2]][airq[,1] > 0]))
 summary(pnorm(res[[2]][airq[,1] < 0]))
 res <- test(xx[["X"]], xx[["X_t"]], xx[["Y"]], 1, 300, 1, 0.01, fm_controls3, solver_controls3, validation_controls3)
 */


#include "../util/Smatrix.h"
#include "../core/Data.h"
#include "../core/Model.h"
#include "../util/Swrap.h"
#include "../solver/MCMC_ALS_Learner.h"

// [[Rcpp::export]]
List test(List X, List X_t, NumericVector target, int factors, int max_iter, int nthreads, double x, List fm_controls, List solver_controls, List validation_controls)
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
  //List X = data_["X"];
  SMatrix<float> m(X);
  Data data;
  data.add_data(&m);
  //List X_t = data_["X_t"];

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

  SMatrix<float> m_t;
  if (true) {
    m_t.assign(X_t);
    data.add_data(&m_t);
  }

  // init Metainfo
  DataMetaInfo meta(data.num_features);

  // init Learner
  Learner* learner;
  learner = new ALS_Learner();
  learner->meta = &meta;
  learner->fm = &fm;
  learner->nthreads          = (int)solver_controls["nthreads"];
  learner->max_iter          = (int)solver_controls["max_iter"];
  learner->tracker.step_size = (int)validation_controls["step_size"];
  learner->tracker.max_iter  = learner->max_iter;
  learner->type              = evaluations_map[as<string>(solver_controls["evaluate.method"])]; //TODO:收敛 R中检验回归或分类对应的评价标准
  learner->tracker.type      = evaluations_map[as<string>(solver_controls["evaluate.method"])];
  learner->conv_condition    = (double)solver_controls["convergence"];
  List solver = solver_controls["solver"];
  ((ALS_Learner*)learner)->alpha_0   = (double)solver["alpha_0"];
  ((ALS_Learner*)learner)->gamma_0   = (double)solver["gamma_0"];
  ((ALS_Learner*)learner)->beta_0    = (double)solver["beta_0"];
  ((ALS_Learner*)learner)->mu_0      = (double)solver["mu_0"];
  ((ALS_Learner*)learner)->alpha     = (double)solver["alpha"];
  ((ALS_Learner*)learner)->w0_mean_0 = (double)solver["w0_mean_0"];


  learner->init();


  // learn model
  learner->learn(data);

  // predict data
  DVector<double> res(data.num_cases);
  fm.predict_batch(data, res);

  // evaluate 有问题
  //cout<<"error: "<<learner.evaluate(data)<<endl;

  // output model
  return List::create(fm.save_model(), res.to_rtype());
}
