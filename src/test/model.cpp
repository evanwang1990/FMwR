#include <Rcpp.h>
#include <time.h>

using namespace Rcpp;
using namespace std;

#include "../Model.h"

//the main bottleneck: tranform large dense matrix to SMatrix

/***R
data <- matrix(rep(c(1,0,0,0,
                     0,1,0,0,
                     0,0,1,0,
                     1,0,0,0,
                     0,0,1,0,
                     1,0,0,1,
                     2,0,0,0,
                     0,0,0,1), 10^6),
               ncol = 500, byrow = T)
sdata <- as_SMatrix(data, F)
*/

// [[Rcpp::export]]
NumericVector model_predict_batch(List data, int threads)
{
  Model model;
  model.num_factor = 3;
  IntegerVector dim= data["dim"];
  model.num_attribute = dim[1];
  model.nthreads = threads;
  model.init();
  model.w.init(0.1);
  model.v.init(0.2);
  SMatrix<float> m(data);
  DVector<double> pred(m.nrow());

  for (int kk = 0; kk < 20; ++kk)
  {
    Data _data;
    _data.add_data(&m);
    model.predict_batch(_data, pred);
  }

  return pred.to_rtype();
}

//[[Rcpp::export]]
NumericVector model_predict_case(List data, int threads)
{
  Model model;
  model.num_factor = 3;
  IntegerVector dim= data["dim"];
  model.num_attribute = dim[1];
  model.nthreads = threads;
  model.init();
  model.w.init(0.1);
  model.v.init(0.2);
  SMatrix<float> m(data);
  DVector<double> pred(m.nrow());

  for (int kk = 0; kk < 20; ++kk)
  {
    for (uint i = 0; i < m.nrow(); ++i)
    {
      SMatrix<float>::Iterator it(m, i);
      pred[i] = model.predict(it);
    }
  }

  return pred.to_rtype();
}

/***R
res <- test_model(sdata, 2)

predict_ <- function(x_)
{
  x <- t(res[[1]]$v) * x_
  sum(apply(x, 2, sum)^2 - apply(x^2, 2, sum))/2
}
res1 <- apply(data, 1, predict_)
all(round(res1 - res[[2]], 5) == 0)
*/
