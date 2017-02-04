#ifndef FTRL_H_
#define FTRL_H_

#include <Rcpp.h>
#include <cmath>
#ifdef _OPENMP
#include <omp.h>
#endif
#include "../util/Dvector.h"
#include "../util/Dmatrix.h"
#include "../util/Smatrix.h"
#include "../util/Random.h"
#include "../core/Learner.h"

class FTRL_Learner: public Learner
{
protected:
  double z_w0;
  double n_w0;
  DVector<double> z_w;
  DMatrix<double> z_v;
  DVector<double> n_w;
  DMatrix<double> n_v;

public:
  // learning rate
  double alpha_w, beta_w;
  double alpha_v, beta_v;

  // int max_iter;
  int random_step; //TODO: random_step

public:
  FTRL_Learner()
    : Learner(), alpha_w(0.1), beta_w(1.0), alpha_v(0.1), beta_v(1.0), random_step(1)
  {}

  ~FTRL_Learner() {}

public:
  void init();
  void learn(Data& train);
  double calculate_grad_mult(double& y_hat, float& y_true);
  void calculate_param(SMatrix<float>::Iterator& it);
};


void FTRL_Learner::init()
{
  z_w0 = 0.0;
  n_w0 = 0.0;
  z_w.setSize(fm->num_attribute); z_w.init(0.0);
  n_w.setSize(fm->num_attribute); n_w.init(0.0);
  z_v.setSize(fm->num_factor, fm->num_attribute); z_v.init(0.0);
  n_v.setSize(fm->num_factor, fm->num_attribute); n_v.init(0.0);

  if (tracker.step_size > 0) {
    tracker.max_iter = max_iter;
    tracker.init();
  }
}


void FTRL_Learner::learn(Data& train)
{
  SMatrix<float>* pdata = train.data;
  DVector<float>* target = train.target;
  double y_hat, g, delta;

  int iter = 0, ii = -1;
  double OLD(eval_score) = 0.0;
  for (;;)
  {
    for (uint i = random_select(random_step); i < train.num_cases; i += random_select(random_step))
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

      it.reset();
      calculate_param(it);

      if (tracker.step_size > 0) {
        ii ++;
        if (ii == tracker.step_size) { ii = 0; }
        if (ii == 0 || iter == max_iter - 1) {
          DVector<double> y_hat_(train.num_cases);
          if (fm->TASK == REGRESSION) {
            fm->predict_batch(train, y_hat_);
            #pragma omp parallel for num_threads(nthreads)
            for (uint i = 0; i < train.num_cases; i++) {
              if (y_hat_[i] < min_target)
                y_hat_[i] = min_target;
              else if (y_hat_[i] > max_target)
                y_hat_[i] = max_target;
            }
          } else {
            fm->predict_prob(train, y_hat_);
          }
          double eval_score = tracker.evaluate(fm, y_hat_, *train.target);
          if (iter > tracker.step_size && fabs((eval_score - OLD(eval_score)) / (OLD(eval_score) + 1e-30)) <= conv_condition) {
            conv_times ++;
          } else {
            conv_times = 0;
          }
          OLD(eval_score) = eval_score;
          tracker.record(fm, iter, eval_score);
        }
      }

      iter ++;
      if (conv_times >= 3) {
        convergent = true;
        break;
      }
      if (iter >= max_iter) { break; }
    }
    if (conv_times >= 3) { break; }
    if (iter >= max_iter) { break; }
  }
}

void FTRL_Learner::calculate_param(SMatrix<float>::Iterator& it)
{
  // w0
  fm->w0 = - z_w0 * alpha_w / (beta_w + sqrt(n_w0));

  uint row_size = it.size();
  DVector<uint> non_zero_columns(row_size);
  for (uint i = 0; i < row_size; ++i, ++it) {
    non_zero_columns[i] = it.index;
  }
  
  // w
  double TMP(z_w), sign;
  uint col_idx;
  #pragma omp parallel for num_threads(nthreads) private(TMP(z_w), sign, col_idx)
  for (uint i = 0; i < row_size; ++i)
  {
    col_idx = non_zero_columns[i];
    TMP(z_w) = z_w[col_idx];
    if (fabs(TMP(z_w)) <= fm->l1_regw) {
      fm->w[col_idx] = 0.0;
    } else {
      sign = TMP(z_w) < 0.0 ? -1.0:1.0;
      fm->w[col_idx] = - (TMP(z_w) - sign * fm->l1_regw) / ((beta_w + sqrt(n_w[col_idx])) / alpha_w + fm->l2_regw);
    }
  }

  // v
  double TMP(z_v);
  for (uint f = 0; f < fm->num_factor; ++f)
  {
    #pragma omp parallel for num_threads(nthreads) private(TMP(z_v), col_idx)
    for (uint i = 0; i < row_size; ++i)
    {
      col_idx = non_zero_columns[i];
      TMP(z_v) = z_v(f, col_idx);
      if (fabs(TMP(z_v)) <= fm->l1_regv) {
        fm->v(f, col_idx) = 0.0;
      } else {
        sign = TMP(z_v) < 0.0 ? -1.0:1.0;
        fm->v(f, col_idx) = - (TMP(z_v) - sign * fm->l1_regv) / ((beta_v + sqrt(n_v(f, col_idx))) / alpha_v + fm->l2_regv);
      }
    }
  }
}

double FTRL_Learner::calculate_grad_mult(double& y_hat, float& y_true)
{
  double mult = 0.0;
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
