#ifndef FM_MODEL_H_
#define FM_MODEL_H_

#include <Rcpp.h>
#include <omp.h>
#include <assert.h>

using namespace Rcpp;

void v_init(NumericMatrix v, double init_mean, double init_stdev);

class fm_model
{
private:
  NumericVector m_sum, m_sum_sqr;

public:
  double w0;
  NumericVector w;
  NumericMatrix v;

public:
  uint num_attribute;
  bool k0, k1;
  int num_factor;
  double reg0, regw, regv;
  double init_mean, init_stdev;

public:
  fm_model();
  ~fm_model() {};
  void init();
  double predict(NumericVector x);
  double predict(NumericVector x, NumericVector sum, NumericVector sum_sqr);
  List save_model();
};

fm_model::fm_model()
{
  num_factor = 0;
  init_mean  = 0;
  init_stdev = 0.01;
  reg0       = 0.0;
  regw       = 0.0;
  regv       = 0.0;
  k0         = true; //rename keep_w0
  k1         = true; //rename keep_w
}

void fm_model::init()
{
  w0 = 0.0;
  w  = NumericVector(num_attribute); // all elements are 0 //TODO:add to class?
  if (num_factor > 0)
  {
    v = NumericMatrix(num_factor, num_attribute);
    v_init(v, init_mean, init_stdev);
  }
  m_sum     = NumericVector(num_factor);
  m_sum_sqr = NumericVector(num_factor);
}

double fm_model::predict(NumericVector x) {return predict(x, m_sum, m_sum_sqr);}

double fm_model::predict(NumericVector x, NumericVector sum, NumericVector sum_sqr)
{
  double reuslt = 0.0;
  size_t len = x.size();
  if (k0) result += w0;
  if (k1)
  {
    assert((len == (size_t)(num_attribute)) && "The length of x is not equal the number of attributes\n");
    for (size_t i = 0; i < len; i++)
      result += w[i] * x[i];
  }

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

void v_init(NumericMatrix v, double init_mean, double init_stdev)
{
  for (NumericMatrix::iterator p = v.begin(); p != v.end(); p++)
    *p = Rf_rnorm(init_mean, init_stdev);
}

#endif
