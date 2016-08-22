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
res <- test(iris[,1:4], iris[,5], 1, 30, 1, 0.01); res[[1]]
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


airq0 <- airq <- airquality[complete.cases(airquality),]#[sample(1:111, 100000, replace = T),]
airq <- as.matrix(airq)
airq[,1] <- ifelse(airq[,1] > 60, 1, -1)
airq[,2:6] <- apply(airq[,2:6], 2, scale)
airq0$Ozone <- ifelse(airq0$Ozone > 60, 1, -1)

res <- test(airq[,2:6], airq[,1], 1, 30, 1, 0.01); res[[1]]
summary(pnorm(res[[2]][airq[,1] > 0]))
summary(pnorm(res[[2]][airq[,1] < 0]))

 fit0 <- glm(factor(Ozone) ~ Solar.R + Wind + Temp + Month + Day, data = airq0, family = binomial(link = "logit"))
 x <- data.frame(y = airq0[,1], y_hat = res[[2]], y_hat2 = predict(fit0, airq0, type = "response"))
 ggplot(data = x) +
geom_density(aes(x = y_hat, color = factor(y))) +
geom_density(aes(x = y_hat2, color = factor(y+10)))

 res1 <- test_ftrl(airq[,2:6], airq[,1], 1, 10000, 1, 1)
 res2 <- test_ftrl(airq[,2:6], airq[,1], 1, 10000, 1, 3)
 res3 <- test_ftrl(airq[,2:6], airq[,1], 1, 10000, 1, 8)
 x <- data.frame(
 x = res1[[3]][[1]],
 y1 = res1[[4]],
 y2 = res2[[4]],
 y3 = res3[[4]])
 ggplot(data = x) +
 geom_point(aes(x = x, y = y1), color = "red") +
 geom_line(aes(x = x, y = y1), color = "red") +
 geom_point(aes(x = x, y = y2), color = "green") +
 geom_line(aes(x = x, y = y2), color = "green") +
 geom_point(aes(x = x, y = y3), color = "black") +
 geom_line(aes(x = x, y = y3), color = "black")
*/


#include <Rcpp.h>
#include <omp.h>

#include "../util/Dmatrix.h"
#include "../util/Dvector.h"
#include "../util/Macros.h"
#include "../util/Random.h"
#include "../util/Smatrix.h"
#include "../util/Swrap.h"

#include "../core/Data.h"
#include "../core/Model.h"
#include "../core/Learner.h"
#include "../core/Evaluation.h"
#include "../core/Validator.h"
#include "../solver/FTRL_Learner.h"


// [[Rcpp::export]]
List test_ftrl(NumericMatrix data_, NumericVector target, int factors, int max_iter, int nthreads, int step)
{
  // init Data
  List dl = as_SMatrix(data_, false);
  List dlt = as_SMatrix(data_, true);
  SMatrix<float> m(dl);
  SMatrix<float> mt(dlt);
  Data data;
  data.add_data(&m);
  data.add_data(&mt);

  DVector<float> tg;
  tg.assign(target);
  data.add_target(&tg);

  // init Model
  Model fm;
  //fm.TASK = REGRESSION;
  fm.num_attribute = data.num_features;
  fm.nthreads = nthreads;
  fm.k1 = true;
  fm.num_factor = factors;
  fm.regw = 0.001;
  fm.regv = 0.003;
  fm.init();
  //fm.v.init(x);
  //fm.w.init(x);

  // init Metainfo
  DataMetaInfo meta(data.num_features);

  // init Learner
  FTRL_Learner learner;
  learner.meta = &meta;
  learner.fm = &fm;
  learner.nthreads = 4;
  learner.max_iter = max_iter;
  learner.l1_regw = 1;
  learner.l2_regw = 1;
  learner.l1_regv = 0.3;
  learner.l2_regv = 0.1;
  learner.ramdom_step = step;

  learner.tracker.step_size = 50;
  learner.tracker.type = LL;

  learner.init();

  // learn model
  learner.learn(data);


  // predict data
  DVector<double> res(data.num_cases);
  fm.predict_prob(data, res);

  // output model
  return List::create(fm.save_model(), res.to_rtype(), learner.tracker.save(), learner.tracker.evaluations_of_train.to_rtype());
}
