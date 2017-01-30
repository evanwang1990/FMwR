#ifndef SGD_H_
#define SGD_H_

#include <Rcpp.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include "../util/Dvector.h"
#include "../util/Dmatrix.h"
#include "../util/Smatrix.h"
#include "../util/Random.h"
#include "../core/Learner.h"

class SGD_Learner: public Learner
{
protected:
  // l1 penalty
  double q_w0;
  DVector<double> q_w;
  DMatrix<double> q_v;
  double u_w0, u_w, u_v;

  double regw, regv;

public:
  double learn_rate;
  bool l1_penalty;
  DVector<double> learn_rates;

  int random_step;

public:
  SGD_Learner() : Learner(), l1_penalty(false), random_step(1) {};
  ~SGD_Learner() {}

public:
  void init();
  void learn(Data& train);
  double calculate_grad_mult(double& y_hat, float& y_true);
  void apply_penalty(double& theta, double& u, double& q);

};

void SGD_Learner::init()
{
  if (fm->l1_regw > 0 || fm->l1_regv > 0) {
    l1_penalty = true;
    regw = fm->l1_regw;
    regv = fm->l1_regv;
    if(fm->l2_regw > 0 || fm->l2_regv > 0)
      Rf_warning("Received L1 & L2 regularization rate > 0. Skipping L2 - only L1 will be used.");
  } else {
    regw = fm->l2_regw;
    regv = fm->l2_regv;
  }

  if (fm->TASK != CLASSIFICATION) {
    l1_penalty = false; //TODO:test if l1 can be used in regression
  }

  if (l1_penalty) {
    q_w0 = 0.0;
    q_w.setSize(fm->num_attribute); q_w.init(0.0);
    q_v.setSize(fm->num_factor, fm->num_attribute); q_v.init(0.0);

    u_w0 = 0.0;
    u_w = 0.0;
    u_v = 0.0;
  }

  learn_rates.setSize(3);

  if (tracker.step_size > 0) {
    tracker.max_iter = max_iter;
    tracker.init();
  }
}

void SGD_Learner::learn(Data& train)
{
  // mult.setSize(train.num_cases);
  SMatrix<float>* pdata = train.data;
  DVector<float>* target = train.target;
  int iter = 0, ii = -1;
  double OLD(eval_score) = 0.0;
  for (;;)
  {
    for (uint i = random_select(random_step); i < train.num_cases; i += random_select(random_step))
    {

      // update absolute value of l1 penalty
      if (l1_penalty) {
        // u_w += learn_rate * regw / train.num_cases;
        // u_v += learn_rate * regv / train.num_cases;
        u_w += learn_rate * regw;
        u_v += learn_rate * regv;
      }

      SMatrix<float>::Iterator it(*pdata, i);

      double y_hat = fm->predict(it);
      double mult = calculate_grad_mult(y_hat, (*target)[i]);

      // cout<<"y_hat = "<<y_hat<<" y_true = "<<(*target)[i]<<" mult = "<<mult<<endl;

      if (fm->k0) {
        double& w0 = fm->w0;
        w0 -= learn_rate * (mult + fm->l2_reg0 * w0);
      }

      if (fm->k1) {
        for (it.reset(); !it.is_end(); ++it)
        {
          double& w = fm->w[it.index];
          w -= learn_rate * mult * it.value;
          if (l1_penalty) {
            apply_penalty(w, u_w, q_w[it.index]);
          } else {
            w -= learn_rate * regw * w;
          }
        }
      }

      for (uint f = 0; f < fm->num_factor; ++f)
      {
        double TMP(sum) = fm->m_sum[f];
        for (it.reset(); !it.is_end(); ++it)
        {
          double& v = fm->v(f, it.index);
          double grad = TMP(sum) * it.value - v * it.value * it.value;
          v -= learn_rate * mult * grad;
          if (l1_penalty) {
            apply_penalty(v, u_v, q_v(f,it.index));
          } else {
            v -= learn_rate * regv * v;
          }
        }
      }

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

double SGD_Learner::calculate_grad_mult(double& y_hat, float& y_true)
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



void SGD_Learner::apply_penalty(double& theta, double& u, double& q)
{
  double OLD(theta) = theta;
  if (theta > 0) {
    theta = max(0.0, OLD(theta) - (u + q));
  } else if (theta < 0) {
    theta = min(0.0, OLD(theta) + (u - q));
  }
  q += theta - OLD(theta);
}

#endif
