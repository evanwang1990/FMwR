#include <Rcpp.h>
#include <../util/Swrap.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
List test_fm(NumericMatrix data_)
{
  return as_SMatrix(data_, false);
}

