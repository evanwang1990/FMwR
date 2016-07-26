#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

#include "../model.h"

/***R
data <- matrix(c(1,0,0,0,
                 0,1,0,0,
                 0,1,1,1,
                 1,1,1,1,
                 0,0,1,1,
                 1,0,0,1,
                 2,3,4,5,
                 10,0,9,1),
               ncol = 4)
sdata <- as_SMatrix(data, F)
*/

// [[Rcpp::export]]
List test_model(List data)
{
  fm_model model;
  model.num_factor = 3;
  model.num_attribute = 4;
  model.init();
  List res = model.save_model();

  SMatrix<float> m(data);
  NumericVector pred(m.nrow());
  for (uint i = 0; i < m.nrow(); ++i)
  {
    SMatrix<float>::Iterator it(m, i);
    pred[i] = model.predict(it);
  }

  return List::create(res, pred);
}

/***R
res <- test_model(sdata)

predict_ <- function(x_)
{
  x <- t(res[[1]]$v) * x_
  sum(apply(x, 2, sum)^2 - apply(x^2, 2, sum))/2
}
res1 <- apply(data, 1, predict_)
all(round(res1 - res[[2]], 5) == 0)
*/
