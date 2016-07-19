#include <Rcpp.h>
#include <omp.h>
using namespace Rcpp;
using namespace std;

void v_init(NumericMatrix v, double init_mean, double init_stdev)
{
  for (NumericMatrix::iterator p = v.begin(); p != v.end(); p++)
    *p = Rf_rnorm(init_mean, init_stdev);
}

// [[Rcpp::export]]
double test(NumericMatrix v, NumericVector x)
{
  double result = 0.0;
  int num_factor = v.nrow();
  size_t len = x.size();
  NumericVector tmp(num_factor);
  NumericVector tmp2(num_factor);
  NumericMatrix::iterator p_v = v.begin();
  NumericVector::iterator p_x = x.begin();
  for (size_t i = 0; i < len; i++)
  {
    for (int f = 0; f < num_factor; f++)
    {
      double d = *p_v * *p_x;
      tmp[f] += d;
      tmp2[f] += d * d;
      p_v ++;
    }
    p_x ++;
  }
  for (int f = 0; f < num_factor; f++)
    result += 0.5 * (tmp[f] * tmp[f] -tmp2[f]);
  return result;
}

//[[Rcpp::export]]
double test1(NumericMatrix v, NumericVector x)
{
  double result = 0.0;
  int num_factor = v.nrow();
  NumericVector sum(num_factor);
  NumericVector sum_sqr(num_factor);

  for (int f = 0; f < num_factor; f++) {
    sum(f) = 0;
    sum_sqr(f) = 0;
    for (size_t i = 0; i < x.size(); i++) {
      double d = v(f,i) * x[i];
      sum(f) += d;
      sum_sqr(f) += d*d;
    }
    result += 0.5 * (sum(f)*sum(f) - sum_sqr(f));
  }
  return result;
}
