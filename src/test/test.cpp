#include <Rcpp.h>
#include <omp.h>
#include <time.h>
#include "../util/Random.h"
using namespace Rcpp;
using namespace std;
//[[Rcpp::export]]
NumericVector fast_dpnorm(NumericVector x)
{
  NumericVector x_(x.size());
  for (int i = 0; i < x.size(); ++i){
#include "../util/RandomData_.h"
    if (x[i] < _MIN_) { x_[i] =  0.0; continue; }
    if (x[i] > _MAX_) { x_[i] = 0.1943369 + 0.9754752 * x[i] + 0.4136861 * sqrt(abs(x[i])) - 0.5034295 * log(abs(x[i]) + 1e-07); continue;}
   // if (x[i] > _MAX_) { x_[i] = 0.187 + 0.9923 * x[i]; continue;}
    int ii = (int)( ( x[i] - _MIN_ ) * 5000);
    double w = (x[i] - __X__[ii]) * 5000;
    x_[i] = w * __Y__[ii + 1] + (1.0 - w) * __Y__[ii];
  }
  return x_;
}


double erf(double x) {
  double t;
  if (x >= 0) {
    t = 1.0 / (1.0 + 0.3275911 * x);
  } else {
    t = 1.0 / (1.0 - 0.3275911 * x);
  }

  double result = 1.0 - (t * (0.254829592 + t * (-0.284496736 + t * (1.421413741 + t * (-1.453152027 + t * 1.061405429)))))*exp(-x*x);
  if (x >= 0) {
    return result;
  } else {
    return -result;
  }
}

double cdf_gaussian(double x, double mean, double stdev) {
  return 0.5 + 0.5 * erf(0.707106781 * (x-mean) / stdev);
}

double cdf_gaussian(double x) {
  return 0.5 + 0.5 * erf(0.707106781 * x );
}

//[[Rcpp::export]]
NumericVector fast_dpnorm_(NumericVector x)
{
  NumericVector x_(x.size());
  for (int i = 0; i < (unsigned) x.size(); ++i)
  {
    double xx = x[i];
    double phi_minus_mu = exp(- xx * xx / 2.0) / 2.506628;
    double Phi_minus_mu = cdf_gaussian(xx);
    x_[i] = phi_minus_mu / (1 - Phi_minus_mu);
  }
  return x_;
}

//[[Rcpp::export]]
void trnorm(double x)
{
  cout<<fast_trnorm_left(0.0, x, 1.0)<<endl<<fast_trnorm_right(0.0, x, 1.0)<<endl;
}
