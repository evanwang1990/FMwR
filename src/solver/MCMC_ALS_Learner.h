#ifndef MCMC_ALS_H_
#define MCMC_ALS_H_

#include <Rcpp.h>
#include <omp.h>
#include "../util/Macros.h"
#include "../util/Random.h"
#include "../util/Dvector.h"
#include "../util/Dmatrix.h"
#include "../util/Smatrix.h"
#include "../core/Learner.h"


class MCMC_ALS_Learner : public Learner
{
protected:
  DVector<double> cache_for_group_values;

public:
  // uint max_iter;
  uint iter_cntr;
  uint num_eval_cases;

  double alpha_0, gamma_0, beta_0, mu_0;
  double alpha;

  double w0_mean_0;

  DVector<double> w_mu, w_lambda;
  DMatrix<double> v_mu, v_lambda;

  bool do_sample;
  bool do_multilevel;

protected:
  void update_all(Data& train, DVector<double>& error, DVector<double>& v_q);

  void update_alpha(Data& train, DVector<double>& error);
  void update_w_mu();
  void update_w_lambda();
  void update_v_mu();
  void update_v_lambda();

  void update_w0(Data& train, DVector<double>& error);
  void update_w(Data& train, DVector<double>& error);
  void update_v(Data& train, DVector<double>& error, DVector<double>& v_q);

  void calculate_error(Data& data, DVector<double>& error);

public:
  // virtual void predict(Data& data, DVector<double>& out);
  MCMC_ALS_Learner() : Learner() {}
  ~MCMC_ALS_Learner() {}

  virtual void init();
  virtual void learn(Data& train);
};

void MCMC_ALS_Learner::init()
{
  uint g = meta->num_attr_groups;
  cache_for_group_values.setSize(g);

  alpha_0   = 1.0;
  gamma_0   = 1.0;
  beta_0    = 1.0;
  mu_0      = 0.0;

  alpha     = 1.0;

  w0_mean_0 = 0.0;

  w_mu.setSize(g);
  w_mu.init(0.0);
  w_lambda.setSize(g);
  w_lambda.init(0.0);

  v_mu.setSize(g, fm->num_factor);
  v_mu.init(0.0);
  v_lambda.setSize(g, fm->num_factor);
  v_lambda.init(0.0);

  iter_cntr = 0;

  if (tracker.step_size > 0) {
    tracker.max_iter = max_iter;
    tracker.init();
  }
}

void MCMC_ALS_Learner::learn(Data& train)
{
  DVector<double> train_err(train.num_cases);
  DVector<double> v_q(train.num_cases);
  // DVector<double> test_err(test.num_cases);

  int ii = -1;
  for (; iter_cntr < max_iter; ++iter_cntr)  //TODO 加入收敛准则
  {
    fm->predict_batch(train, train_err);
    if (tracker.step_size > 0) { // MCMC ALS的valid_step要小于SGD之类
      ii++;
      if (ii == tracker.step_size) { ii = 0; }
      if (ii == 0 || iter_cntr == max_iter - 1) {
        DVector<double> y_hat_(train.num_cases);
        for (uint i = 0; i < train.num_cases; ++i) {
          y_hat_[i] = fast_pnorm(train_err[i]);
        }
        double eval_score = tracker.evaluate(fm, y_hat_, *train.target);
        tracker.record(fm, iter_cntr, eval_score);
      }
    }
    calculate_error(train, train_err);
    update_all(train, train_err, v_q);
  }
}


void MCMC_ALS_Learner::update_all(Data& train, DVector<double>& error, DVector<double>& v_q)
{
  if (train.num_cases != error.size()) {
    stop("there's no the same number of cases between train and error datasets...");
  }

  if (train.num_features != fm->num_attribute) {
    stop("there's no the same number of features between train and fm object...");
  }

  update_alpha(train, error);

  if (fm->k0) { update_w0(train, error); }

  if (fm->k1) {
    update_w_lambda();
    update_w_mu();
    update_w(train, error);
  }

  if (fm->num_factor > 0) {
    update_v_lambda();
    update_v_mu();
    update_v(train, error, v_q);
  }
}


/***
* update parameters
*/
void MCMC_ALS_Learner::update_w0(Data&train, DVector<double>& error)
{
  double err = 0;
  double& w0 = fm->w0;
  for (double* it = error.begin(); it != error.end(); ++it)
  { err += *it - w0; }

  double w0_var = (double) 1.0 / (fm->reg0 + alpha * train.num_cases);
  double w0_mean = - (alpha * err - w0_mean_0 * fm->reg0) * w0_var;
  double TMP(w0);
  double OLD(w0) = w0;

  if (do_sample) {
    TMP(w0) = Rf_rnorm(w0_mean, std::sqrt(w0_var));
  } else {
    TMP(w0) = w0_mean;
  }

  CHECK_PARAM(w0,);
  w0 = TMP(w0);

  // update error
  double diff_w0 = OLD(w0) - TMP(w0);
  for (double *it = error.begin(); it != error.end(); ++it) {
    *it -= diff_w0;
  }
}

void MCMC_ALS_Learner::update_w(Data& train, DVector<double>& error)
{
  SMatrix<float>* tdata = train.data_t;
  DVectorDouble& w = fm->w;

  omp_set_num_threads(nthreads);
  // set arrays to store the updated errors under each thread
  DMatrix<double> errs(nthreads, error.size());
  errs.assign_by_row(error);

  double w_mean, w_var, OLD(w), TMP(w), w_diff, tot_reduce, error_, val_;
  uint j, g, end;
  bool update_err;
  #pragma omp parallel
  {
    // update w
    #pragma omp for private(w_mean, w_var, OLD(w), TMP(w), w_diff, update_err, end)
    for (uint i = 0; i < tdata->nrow(); i++)
    {
      end = tdata->row_idx[i+1];
      w_mean = 0;
      w_var  = 0;
      OLD(w) = w[i];
      TMP(w) = w[i];
      g = meta->attr_group[i];
      update_err = true;

      //update w[i]
      for (j = tdata->row_idx[i]; j < end; ++j)
      {
        val_ = tdata->value[j];
        w_mean += errs(omp_get_thread_num(), tdata->col_idx[j]) * val_ - TMP(w) * val_ * val_;
        w_var += val_ * val_;
      }

      w_var = (double) 1.0 / (w_lambda[g] + alpha * w_var);
      w_mean = - w_var * (alpha * w_mean - w_mu[g] * w_lambda[g]);

      if (R_IsNaN(w_var) || w_var == R_PosInf || w_var == R_NegInf) {
        TMP(w) = 0.0;
      } else {
        if (do_sample) {
          TMP(w) = Rf_rnorm(w_mean, w_var);
        } else {
          TMP(w) = w_mean;
        }
      }

      CHECK_PARAM(w, update_err = false);
      w[i] = TMP(w);

      if (update_err) {
        w_diff = OLD(w) - TMP(w);
        for (j = tdata->row_idx[i]; j < end; ++j)
        {
          errs(omp_get_thread_num(), tdata->col_idx[j]) -= tdata->value[j] * w_diff; //partially update errors in parallel run type
        }
      }
    }

    // completely update errors
    #pragma omp for private(tot_reduce, error_, j)
    for (uint i = 0; i < error.size(); ++i)
    {
      tot_reduce = 0;
      error_ = error[i];
      for (j = 0; j < (uint)nthreads; ++j)
      {
        tot_reduce += error_ - errs(j,i);
      }
      error[i] -= tot_reduce;
    }
  }
}

void MCMC_ALS_Learner::update_v(Data& train, DVector<double>& error_, DVector<double>& v_q_)
{
  SMatrix<float>* tdata = train.data_t;
  DMatrixDouble& v = fm->v;
  DVector<double> error; error.assign(error_);
  DVector<double> v_q;   v_q.assign(v_q_);

  double v_mean, v_var, OLD(v), TMP(v), v_diff, h;
  float val_;
  uint end, m, idx_, g, j;
  bool update_err;
  for (uint f = 0; f < fm->num_factor; ++f)
  {
    // init v_q
    v_q.init(0.0);
    // #pragma omp parallel num_threads(nthreads)
    {
      // update v_q
      // #pragma omp for private(end, TMP(v), j)
      for (uint i = 0; i < tdata->nrow(); ++i)
      {
        end = tdata->row_idx[i+1];
        TMP(v) = v(f, i);
        for (j = tdata->row_idx[i]; j < end; ++j)
        {
          v_q[tdata->col_idx[j]] += tdata->value[j] * TMP(v);
        }
      }

      //update v(f,:)
      // #pragma omp for private(v_mean, v_var, TMP(v), OLD(v), v_diff, h, val_, end, m, idx_, g, update_err) private(v_q, error)
      for (uint i = 0; i < tdata->nrow(); ++i)
      {
        // update v(f, i)
        v_mean = 0;
        v_var = 0;
        update_err = true;
        OLD(v) = v(f, i);
        TMP(v) = v(f, i);
        end = tdata->row_idx[i+1];
        g = meta->attr_group[i];
        for (m = tdata->row_idx[i]; m < end; ++m)
        {
          val_ = tdata->value[m];
          idx_ = tdata->col_idx[m];
          h = val_ * v_q[idx_] - val_ * val_ * TMP(v);
          v_mean += h * error[idx_];
          v_var  += h * h;
        }
        v_mean -= TMP(v) * v_var;
        v_var = (double) 1.0 / (v_lambda(g, f) + alpha * v_var);
        v_mean = - v_var * (alpha * v_mean - v_mu(g, f) * v_lambda(g, f));


        if (R_IsNaN(v_var) || v_var == R_PosInf || v_var == R_NegInf) {
          TMP(v) = 0.0;
        } else {
          if (do_sample) {
            TMP(v) = Rf_rnorm(v_mean, std::sqrt(v_var));
          } else {
            TMP(v) = v_mean;
          }
        }

        CHECK_PARAM(v, update_err = false);
        v(f, i) = TMP(v);
        v_diff = OLD(v) - TMP(v);

        // update error
        if (update_err) {
          for (m = tdata->row_idx[i]; m < end; ++m)
          {
            val_ = tdata->value[m];
            idx_ = tdata->col_idx[m];
            h = val_ * v_q[idx_] - val_ * val_ * OLD(v);
            v_q[idx_] -= val_ * v_diff; //updated partially, only when hthreads = 1 then v_q will be completely and serially updated
            error[idx_] -= h * v_diff;  //like v_q TODO: test rate of convergence under parallel run
          }
        }
      }
    }
  }
}


/***
* update hyper parameters
*/
void MCMC_ALS_Learner::update_alpha(Data& train, DVector<double>& error)
{
  if (! do_multilevel) {
    alpha = alpha_0;
    return;
  }

  double alpha_n = alpha_0 + train.num_cases;
  double gamma_n = gamma_0;
  #pragma omp parallel for num_threads(nthreads) reduction(+:gamma_n)
  for (uint i = 0; i < train.num_cases; ++i)
  {
    gamma_n += error[i] * error[i];
  }

  double OLD(alpha) = alpha;
  double TMP(alpha);
  TMP(alpha) = Rf_rgamma(alpha_n / 2.0, gamma_n / 2.0);

  CHECK_PARAM(alpha,);
  alpha = TMP(alpha);
}


void MCMC_ALS_Learner::update_w_mu()
{
  if (! do_multilevel) {
    w_mu.init(mu_0);
    return;
  }

  DVector<double>& w_mu_mean = cache_for_group_values;
  w_mu_mean.init(0.0);
  for (uint i = 0; i < fm->num_attribute; ++i)
  {
    w_mu_mean[meta->attr_group[i]] += fm->w[i];
  }

  for (uint g = 0; g < meta->num_attr_groups; ++g)
  {
    w_mu_mean[g] = (w_mu_mean[g] + beta_0 * mu_0) / (meta->num_attr_per_group[g] + beta_0);
    double w_mu_var = (double) 1.0 / ((meta->num_attr_per_group[g] + beta_0) * w_lambda[g]);
    double TMP(w_mu);
    double OLD(w_mu) = w_mu[g];
    if (do_sample) {
      TMP(w_mu) = Rf_rnorm(w_mu_mean[g], std::sqrt(w_mu_var));
    } else {
      TMP(w_mu) = w_mu_mean[g];
    }

    CHECK_PARAM(w_mu,);
    w_mu[g] = TMP(w_mu);
  }
}


void MCMC_ALS_Learner::update_w_lambda()
{
  if (! do_multilevel) { return; }

  DVector<double>& w_lambda_gamma = cache_for_group_values;
  w_lambda_gamma.init(0.0);

  uint g;
  for (uint i = 0; i < fm->num_attribute; ++i)
  {
    g = meta->attr_group[i];
    w_lambda_gamma[g] += (fm->w[i] - w_mu[g]) * (fm->w[i] - w_mu[g]);
  }

  for (g = 0; g < meta->num_attr_groups; ++g)
  {
    w_lambda_gamma[g] += beta_0 * (w_mu[g] - mu_0) * (w_mu[g] - mu_0) + gamma_0;
    double w_lambda_alpha = alpha_0 + meta->num_attr_per_group[g] + 1;
    double TMP(w_lambda);
    double OLD(w_lambda) = w_lambda[g];

    if (do_sample) {
      TMP(w_lambda) = Rf_rgamma(w_lambda_alpha / 2.0, w_lambda_gamma[g] / 2.0);
    } else {
      TMP(w_lambda) = w_lambda_alpha / w_lambda_gamma[g];
    }

    CHECK_PARAM(w_lambda,);
    w_lambda[g] = TMP(w_lambda);
  }
}


void MCMC_ALS_Learner::update_v_mu()
{
  if (! do_multilevel) {
    v_mu.init(mu_0);
    return;
  }

  DVector<double>& v_mu_mean = cache_for_group_values;

  for (uint f = 0; f < fm->num_factor; ++f)
  {
    v_mu_mean.init(0.0);
    for (uint i = 0; i < fm->num_attribute; ++i)
    {
      v_mu_mean[meta->attr_group[i]] += fm->v(f, meta->attr_group[i]);
    }

    for (uint g = 0; g < meta->num_attr_groups; ++g)
    {
      v_mu_mean[g] = (v_mu_mean[g] + beta_0 * mu_0) / (meta->num_attr_per_group[g] + beta_0);
      double v_mu_var = double (1.0) / ((meta->num_attr_per_group[g] + beta_0) * v_lambda(g,f));

      double TMP(v_mu);
      double OLD(v_mu) = v_mu(g, f);

      if (do_sample) {
        TMP(v_mu) = Rf_rnorm(v_mu_mean[g], std::sqrt(v_mu_var));
      } else {
        TMP(v_mu) = v_mu_mean[g];
      }

      CHECK_PARAM(v_mu,);
      v_mu(g, f) = TMP(v_mu);
    }
  }
}


void MCMC_ALS_Learner::update_v_lambda()
{
  if (! do_multilevel) { return; }
  DVector<double>& v_lambda_gamma = cache_for_group_values;

  for (uint f = 0; f < fm->num_factor; ++f)
  {
    v_lambda_gamma.init(0.0);
    for (uint i = 0; i < fm->num_attribute; ++i)
    {
      uint g = meta->attr_group[i];
      v_lambda_gamma[g] += (fm->v(f,i) - v_mu(g,f)) * (fm->v(f,i) - v_mu(g,f));
    }

    for (uint g = 0; g < meta->num_attr_groups; ++g)
    {
      v_lambda_gamma[g] += beta_0 * (v_mu(g, f) - mu_0) * (v_mu(g, f) - mu_0) + gamma_0;
      double v_lambda_alpha = alpha_0 + meta->num_attr_per_group[g] + 1;
      double TMP(v_lambda);
      double OLD(v_lambda) = v_lambda(g, f);

      if (do_sample) {
        TMP(v_lambda) = Rf_rgamma(v_lambda_alpha / 2.0, v_lambda_gamma[g] / 2.0);
      } else {
        TMP(v_lambda) = v_lambda_alpha / v_lambda_gamma[g];
      }

      CHECK_PARAM(v_lambda,);
      v_lambda(g, f) = TMP(v_lambda);
    }
  }
}


void MCMC_ALS_Learner::calculate_error(Data& data, DVector<double>& error)
{
  if (fm->TASK == REGRESSION) {
    #pragma omp parallel for num_threads(nthreads)
    for (uint i = 0; i < data.num_cases; ++i)
    {
      error[i] -= data.target->get(i);
    }
  } else if (fm->TASK == CLASSIFICATION) {
    double TMP(error);
    if (do_sample) {
      #pragma omp parallel for num_threads(nthreads) private(TMP(error))
      for (uint i = 0; i < data.num_cases; ++i)
      {
        TMP(error) = error[i];
        if (data.target->get(i) >= 0.0) {
          error[i] -= fast_trnorm_left(TMP(error), 0.0, 1.0);
          // error[i] -= fast_trnorm_left(0.0, TMP(error), 1.0);
        } else {
          error[i] -= fast_trnorm_right(TMP(error), 0.0, 1.0);
          // error[i] -= fast_trnorm_right(0.0, TMP(error), 1.0);
        }
      }
    } else {
      #pragma omp parallel for num_threads(nthreads) private(TMP(error))
      for (uint i = 0; i < data.num_cases; ++i)
      {
        TMP(error) = error[i];
        // phi_minus_mu = exp(-TMP(error) * TMP(error) / 2.0) / SQRT2PI;
        // Phi_minus_mu = fast_pnorm(-TMP(error));
        if (data.target->get(i) >= 0.0) {
          // error[i] = -phi_minus_mu / (1 - Phi_minus_mu);
          error[i] = - fast_dpnorm(-TMP(error)); //v初始值很大时能很快收敛,比libFM中计算速度快6倍左右
        } else {
          // error[i] = phi_minus_mu / Phi_minus_mu;
          error[i] = fast_dpnorm(TMP(error));
        }
      }
    }
  } else {
    stop("unknown task...");
  }
}

class MCMC_Learner: public MCMC_ALS_Learner
{
public:
  MCMC_Learner()
    : MCMC_ALS_Learner()
  {
    do_sample = true;
    do_multilevel = true;
  }

  ~MCMC_Learner() {}
};

class ALS_Learner: public MCMC_ALS_Learner
{
public:
  ALS_Learner()
    : MCMC_ALS_Learner()
  {
    do_sample = false;
    do_multilevel = false;
  }

  ~ALS_Learner() {}
};

#endif
