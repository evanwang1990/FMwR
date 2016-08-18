#ifndef FTRL_H_
#define FTRL_H_

#include <Rcpp.h>
#include <omp.h>
#include "../util/Dvector.h"
#include "../util/Dmatrix.h"
#include "../util/Smatrix.h"
#include "../util/Random.h"
#include "Learner.h"

// just for logistic regression
class FTRL_Learner: public Learner
{
protected:
  double z_w0;
  double n_w0;
  DVector<double> z_w;
  DMatrix<double> z_v;
  DVector<double> n_w;
  DMatrix<double> n_v;

  DVector<double>* sum;

public:
  // regularization
  double l1_regw, l2_regw;
  double l1_regv, l2_regv;
  // learning rate
  double alpha_w, beta_w;
  double alpha_v, beta_v;

  int max_iter;
  int ramdom_step; //TODO: random_step

public:
  FTRL_Learner()
    : Learner(), max_iter(3000), l1_regw(0.5), l1_regv(1), l2_regw(0.1), l2_regv(0.5), alpha_w(0.1), alpha_v(0.1), beta_w(1.0), beta_v(1.0), ramdom_step(1)
  {}

  ~FTRL_Learner() {}

public:
  void init();
  void learn(Data& train);
  double calculate_grad_mult(double& y_hat, float& y_true);
  void calculate_param();
};


void FTRL_Learner::init()
{
  z_w0 = 0.0;
  n_w0 = 0.0;
  z_w.setSize(fm->num_attribute); z_w.init(0.0);
  n_w.setSize(fm->num_attribute); n_w.init(0.0);
  z_v.setSize(fm->num_factor, fm->num_attribute); z_v.init(0.0);
  n_v.setSize(fm->num_factor, fm->num_attribute); n_v.init(0.0);
}


void FTRL_Learner::learn(Data& train)
{
  SMatrix<float>* pdata = train.data;
  DVector<float>* target = train.target;
  double y_hat, g, delta;

  int iter = 0;
  for (;;)
  {
    for (uint i = random_select(ramdom_step); i < train.num_cases; i += random_select(ramdom_step))
    {
      SMatrix<float>::Iterator it(*pdata, i);
      y_hat = fm->predict(it);
      double mult = calculate_grad_mult(y_hat, (*target)[i]);

      if (fm->k0) {
        g = mult;
        double OLD(n_w0) = n_w0;
        n_w0 += g * g;
        delta = (sqrt(n_w0) - sqrt(OLD(n_w0))) / alpha_w;
        z_w0 += g  - delta * fm->w0;
      }

      if (fm->k1) {
        for (it.reset(); !it.is_end(); ++it)
        {
          g = mult * it.value;
          double& TMP(n_w) = n_w[it.index];
          double OLD(n_w) = TMP(n_w);
          TMP(n_w) += g * g;
          delta = (sqrt(TMP(n_w)) - sqrt(OLD(n_w))) / alpha_w;
          z_w[it.index] += g - delta * fm->w[it.index];
        }
      }

      for (uint f = 0; f < fm->num_factor; ++f)
      {
        double TMP(sum) = fm->m_sum[f];

        for (it.reset(); !it.is_end(); ++it)
        {
          g = mult * (TMP(sum) * it.value - fm->v(f, it.index) * it.value * it.value);
          double& TMP(n_v) = n_v(f, it.index);
          double OLD(n_v) = TMP(n_v);
          TMP(n_v) += g * g;
          delta = (sqrt(TMP(n_v)) - sqrt(OLD(n_v))) / alpha_v;
          z_v(f, it.index) += g - delta * fm->v(f, it.index);
        }
      }

      calculate_param();

      iter ++;
      if (iter >= max_iter) { break; }
    }
    if (iter >= max_iter) { break; }
  }
}

void FTRL_Learner::calculate_param()
{
  // w0
  fm->w0 = - z_w0 * alpha_w / (beta_w + sqrt(n_w0));

  // w
  for (uint i = 0; i < fm->num_attribute; ++i)
  {
    double TMP(z_w) = z_w[i];
    if (abs(TMP(z_w)) <= l1_regw) {
      fm->w[i] = 0.0;
    } else {
      double sign = TMP(z_w) < 0.0 ? -1.0:1.0;
      fm->w[i] = - (TMP(z_w) - sign * l1_regw) / ((beta_w + sqrt(n_w[i])) / alpha_w + l2_regw);
    }
  }

  // v
  for (uint f = 0; f < fm->num_factor; ++f)
  {
    for (uint i = 0; i < fm->num_attribute; ++i)
    {
      double TMP(z_v) = z_v(f, i);
      if (abs(TMP(z_v)) <= l1_regv) {
        fm->v(f, i) = 0.0;
      } else {
        double sign = TMP(z_v) < 0.0 ? -1.0:1.0;
        fm->v(f, i) = - (TMP(z_v) - sign * l1_regv) / ((beta_v + sqrt(n_v(f, i))) / alpha_v + l2_regv);
      }
    }
  }
}

double FTRL_Learner::calculate_grad_mult(double& y_hat, float& y_true)
{
  double mult;
  if (fm->TASK == REGRESSION) {
    y_hat = min(max_target, y_hat);
    y_hat = max(min_target, y_hat);
    mult = -(y_true - y_hat);
  } else if (fm->TASK == CLASSIFICATION) {
    mult = - y_true * (1.0 - 1.0 / (1.0 + exp(-y_true * y_hat)));
  }
  return mult;
}

#endif
