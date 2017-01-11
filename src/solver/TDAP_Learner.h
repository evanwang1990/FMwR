#ifndef TDAP_H_
#define TDAP_H_

#include <Rcpp.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include "../util/Dvector.h"
#include "../util/Dmatrix.h"
#include "../util/Smatrix.h"
#include "../core/Learner.h"

class TDAP_Learner: public Learner
{
protected:
  double u_w0;
  DVector<double> u_w;
  DMatrix<double> u_v;
  double nu_w0;
  DVector<double> nu_w;
  DMatrix<double> nu_v;
  double delta_w0;
  DVector<double> delta_w;
  DMatrix<double> delta_v;
  double h_w0;
  DVector<double> h_w;
  DMatrix<double> h_v;
  double z_w0;
  DVector<double> z_w;
  DMatrix<double> z_v;

public:
  // decay rate
  double gamma;
  // learn rate
  double alpha_w, alpha_v;
  // int max_iter;
  int random_step;

public:
  TDAP_Learner()
    : Learner(), gamma(0.001), alpha_w(0.1), alpha_v(0.1), random_step(1)
  {}

  ~TDAP_Learner() {}

public:
  virtual void init();
  void learn(Data& train);
  double calculate_grad_mult(double& y_hat, float& y_true);
  void calculate_param();
};

void TDAP_Learner::init()
{
  u_w0     = 0.0;
  nu_w0    = 0.0;
  delta_w0 = 0.0;
  h_w0     = 0.0;
  z_w0     = 0.0;
  u_w.setSize(fm->num_attribute); u_w.init(0.0);
  nu_w.setSize(fm->num_attribute); nu_w.init(0.0);
  delta_w.setSize(fm->num_attribute); delta_w.init(0.0);
  h_w.setSize(fm->num_attribute); h_w.init(0.0);
  z_w.setSize(fm->num_attribute); z_w.init(0.0);
  u_v.setSize(fm->num_factor, fm->num_attribute); u_v.init(0.0);
  nu_v.setSize(fm->num_factor, fm->num_attribute); nu_v.init(0.0);
  delta_v.setSize(fm->num_factor, fm->num_attribute); delta_v.init(0.0);
  h_v.setSize(fm->num_factor, fm->num_attribute); h_v.init(0.0);
  z_v.setSize(fm->num_factor, fm->num_attribute); z_v.init(0.0);

  if (tracker.step_size > 0) {
    tracker.max_iter = max_iter;
    tracker.init();
  }
}

void TDAP_Learner::learn(Data& train)
{
  SMatrix<float>* pdata = train.data;
  DVector<float>* target = train.target;
  double egamma = exp(-gamma);

  int iter = 0, ii = -1;
  double y_hat, mult,  g, sigma;
  double OLD(eval_score) = 0.0;
  for (;;)
  {
    for (uint i = random_select(random_step); i < train.num_cases; i += random_select(random_step))
    {
      SMatrix<float>::Iterator it(*pdata, i);
      y_hat = fm->predict(it);
      mult = calculate_grad_mult(y_hat, (*target)[i]);

      if (fm->k0) {
        g = mult;
        double OLD(u_w0) = u_w0;
        u_w0 += g * g;
        nu_w0 += g;
        sigma = (sqrt(u_w0) - sqrt(OLD(u_w0))) / alpha_w;
        delta_w0 = egamma * (delta_w0 + sigma);
        h_w0 = egamma * (h_w0 + sigma * fm->w0);
        z_w0 = nu_w0 - h_w0;
      }

      if (fm->k1) {
        for (it.reset(); !it.is_end(); ++it)
        {
          g = mult * it.value;
          double& TMP(u_w) = u_w[it.index];
          double& TMP(nu_w) = nu_w[it.index];
          double& TMP(delta_w) = delta_w[it.index];
          double& TMP(h_w) = h_w[it.index];
          double OLD(u_w) = TMP(u_w);
          TMP(u_w) += g * g;
          TMP(nu_w) += g;
          sigma = (sqrt(TMP(u_w)) - sqrt(OLD(u_w))) / alpha_w;
          TMP(delta_w) = egamma * (TMP(delta_w) + sigma);
          TMP(h_w) = egamma * (TMP(h_w) + sigma * fm->w[it.index]);
          z_w[it.index] = TMP(nu_w) - TMP(h_w);
        }
      }

      for (uint f = 0; f < fm->num_factor; ++f)
      {
        double TMP(sum) = fm->m_sum[f];
        for (it.reset(); !it.is_end(); ++it)
        {
          g = mult * (TMP(sum) * it.value - fm->v(f, it.index) * it.value * it.value);
          double& TMP(u_v) = u_v(f, it.index);
          double& TMP(nu_v) = nu_v(f, it.index);
          double& TMP(delta_v) = delta_v(f, it.index);
          double& TMP(h_v) = h_v(f, it.index);
          double OLD(u_v) = TMP(u_v);
          TMP(u_v) += g * g;
          TMP(nu_v) += g;
          sigma = (sqrt(TMP(u_v)) - sqrt(OLD(u_v))) / alpha_v;
          TMP(delta_v) = egamma * (TMP(delta_v) + sigma);
          TMP(h_v) = egamma * (TMP(h_v) + sigma * fm->v(f, it.index));
          z_v(f, it.index) = TMP(nu_v) - TMP(h_v);
        }
      }

      calculate_param();

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
          if (iter > tracker.step_size && abs((eval_score - OLD(eval_score)) / (OLD(eval_score) + 1e-30)) <= conv_condition) {
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


void TDAP_Learner::calculate_param()
{
  // w0
  fm->w0 = - z_w0 / delta_w0;

  // w
  for (uint i = 0; i < fm->num_attribute; ++i)
  {
    double TMP(z_w) = z_w[i];
    if (abs(TMP(z_w)) <= fm->l1_regw) {
      fm->w[i] = 0.0;
    } else {
      double sign = TMP(z_w) < 0.0 ? -1.0:1.0;
      fm->w[i] = - (TMP(z_w) - sign * fm->l1_regw) / (delta_w[i] + fm->l2_regw);
    }
  }

  // v
  for (uint f = 0; f < fm->num_factor; ++f)
  {
    for (uint i = 0; i < fm->num_attribute; ++i)
    {
      double TMP(z_v) = z_v(f, i);
      if (abs(TMP(z_v)) <= fm->l1_regv) {
        fm->v(f, i) = 0.0;
      } else {
        double sign = TMP(z_v) < 0.0 ? -1.0:1.0;
        fm->v(f, i) = - (TMP(z_v) - sign * fm->l1_regv) / (delta_v(f, i) + fm->l2_regv);
      }
    }
  }
}

double TDAP_Learner::calculate_grad_mult(double& y_hat, float& y_true)
{
  double mult = 0.0;
  if (fm->TASK == REGRESSION) {
    y_hat = min(max_target, y_hat);
    y_hat = max(min_target, y_hat);
    mult = -(y_true - y_hat);cout<<y_hat<<" "<<y_true<<endl;
  } else if (fm->TASK == CLASSIFICATION) {
    mult = - y_true * (1.0 - 1.0 / (1.0 + exp(-y_true * y_hat)));
  }
  return mult;
}

#endif
