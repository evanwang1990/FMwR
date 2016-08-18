#ifndef SGD_H_
#define SGD_H_

#include <Rcpp.h>
#include <omp.h>
#include "../util/Dvector.h"
#include "../util/Dmatrix.h"
#include "../util/Smatrix.h"
#include "Learner.h"

class SGD_Learner: public Learner
{
protected:
  // l1 penalty
  double q_w0;
  DVector<double> q_w;
  DMatrix<double> q_v;
  double u_w0, u_w, u_v;

protected:
  // DVector<double> mult;
  DVector<double>* sum;

public:
  int max_iter;
  double learn_rate;
  bool l1_penalty;
  DVector<double> learn_rates;
  double alpha_rate; //TODO

public:
  SGD_Learner() : Learner(), l1_penalty(false), max_iter(3000) {};
  ~SGD_Learner() {}

public:
  void init();
  void learn(Data& train);
  double calculate_grad_mult(double& y_hat, float& y_true);
  void apply_penalty(double& theta, double& u, double& q);

};

void SGD_Learner::init()
{
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
}

void SGD_Learner::learn(Data& train)
{
  // mult.setSize(train.num_cases);
  SMatrix<float>* pdata = train.data;
  DVector<float>* target = train.target;
  for (int iter = 0; iter < max_iter; iter++)
  {
    for (uint i = 0; i < train.num_cases; ++i)
    {

      // update absolute value of l1 penalty
      if (l1_penalty) {
        u_w += learn_rate * fm->regw / train.num_cases;
        u_v += learn_rate * fm->regv / train.num_cases;
      }

      SMatrix<float>::Iterator it(*pdata, i);

      double y_hat = fm->predict(it);
      double mult = calculate_grad_mult(y_hat, (*target)[i]);

      // cout<<"mult = "<<mult<<endl;

      if (fm->k0) {
        double& w0 = fm->w0;
        w0 -= learn_rate * (mult + fm->reg0 * w0);
        // cout<<"w0 ="<<w0<<endl;
      }

      if (fm->k1) {
        for (it.reset(); !it.is_end(); ++it)
        {
          double& w = fm->w[it.index];
          w -= learn_rate * mult * it.value;
          if (l1_penalty) {
            apply_penalty(w, u_w, q_w[it.index]);
          } else {
            w -= learn_rate * fm->regw * w;
            // cout<<"w ="<<w<<endl;
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
            v -= learn_rate * fm->regv * v;
          }
        }
      }
    }
  }
}

double SGD_Learner::calculate_grad_mult(double& y_hat, float& y_true)
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
