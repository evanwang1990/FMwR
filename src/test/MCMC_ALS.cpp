#include <Rcpp.h>
using namespace Rcpp;

/***R/
setwd("/home/jinlingwang/project/github/FMwR/src/test/")
data(iris)
iris$Species <- ifelse(iris$Species == "setosa", 1, -1)
iris <- as.matrix(iris)
*/

//#include "../MCMC_ALS_Learner.h"

// [[Rcpp::export]]
void test()
{
  //MCMC_Learner learn;
}
