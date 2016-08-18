#ifndef RANDOM_H_
#define RANDOM_H_

#include <Rcpp.h>
#include <time.h>
#include "Macros.h"
using namespace Rcpp;

double fast_runif();
double fast_rexp();
double fast_rnorm();
double fast_trnorm_left_naive(double left);
double fast_trnorm_left(double left);
double fast_trnorm_left(double left, double mean, double stdev);
double fast_trnorm_right(double right);
double fast_trnorm_right(double right, double mean, double stdev);

uint random_select(int n);

double fast_runif()
{
  // srand(123);
  return rand() / ((double)RAND_MAX + 1);
}

double fast_rexp()
{
  return -log(1-fast_runif());
}

double fast_rnorm()
{
  // Joseph L. Leva: A fast normal Random number generator
  double u, v, x, y, Q;
	do {
		do {
			u = fast_runif();
		} while (u == 0.0);
		v = 1.7156 * (fast_runif() - 0.5);
		x = u - 0.449871;
		y = std::abs(v) + 0.386595;
		Q = x*x + y*(0.19600*y-0.25472*x);
		if (Q < 0.27597) { break; }
	} while ((Q > 0.27846) || ((v*v) > (-4.0*u*u*std::log(u))));
	return v / u;
}


double fast_trnorm_left_naive(double left)
{
  double res;
  for (;;)
  {
    res = fast_rnorm();
    if (res >= left) { return res; }
  }
}

double fast_trnorm_left(double left)
{
  if (left < 0.0) {
    return fast_trnorm_left_naive(left);
  } else {
    double alpha_star = 0.5 * (left + sqrt(left * left + 4.0));
    double z, d, u;
    for (;;)
    {
      z = fast_rexp() / alpha_star + left;
      d = z - alpha_star;
      d = exp(-(d*d)/2);
      u = fast_runif();
      if (u < d) { return z; }
    }
  }
}


double fast_trnorm_left(double left, double mean, double stdev)
{
  return mean + stdev * fast_trnorm_left((left - mean) / stdev);
}

double fast_trnorm_right(double right)
{
  return -fast_trnorm_left(-right);
}

double fast_trnorm_right(double right, double mean, double stdev)
{
  return mean + stdev * fast_trnorm_right((right - mean) / stdev);
}














double fast_pnorm(double x)
{
  #include "RandomData.h"
  double TMP(x) = abs(x);
  double res;
  if (TMP(x) > _MAX_) {
    res = 0.999999900524235; // truncated
  } else {
    int i = (int)(TMP(x) * _HINV_);
    double w = (TMP(x) - _X_[i]) * _HINV_;
    res = w * _Y_[i + 1] + (1.0 - w) * _Y_[i];
  }

  if (TMP(x) == x) { return res; }
  return 1.0 - res;
}

// dnorm(x) / (1 - pnorm(x))
double fast_dpnorm(double x)
{
  #include "RandomData_.h"
  if (x < _MIN_) { return 0.0; }
  if (x > _MAX_) { return 0.1943369 + 0.9754752 * x + 0.4136861 * sqrt(abs(x)) - 0.5034295 * log(abs(x) + 1e-07); }
  int i = (int)( (x - _MIN_) * 5000);
  double w = (x - __X__[i]) * 5000;
  return w * __Y__[i + 1] + (1.0 - w) * __Y__[i];
}

uint random_select(int n)
{
  if (n == 1) {
    return 1;
  }  
  return (uint) (fast_runif() * n + 1);
}

#endif
