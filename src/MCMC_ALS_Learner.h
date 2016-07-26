#ifndef MCMC_ALS_H_
#define MCMC_ALS_H_

#include <Rcpp.h>
#include "util/Dvector.h"
#include "util/Dmatrix.h"
#include "util/Smatrix.h"


struct e_q_term
{
  double e;
  double q;
};


struct relation_cache
{
  double wnum;
  double q;
  double wc;
  double wc_sqr;
  double y;
  double we;
  double weq;
}

class MCMC_ALS_Learner : public Learner
{
public: //TODO:??
  virtual double evaluate(Data& data) { return std::numeric_limits<double>::quiet_NaN(); }
protected:
  virtual double predict_case(Data& data) {
    throw "not supported for MCMC and ALS";
  }

protected:
  DVector<double> cache_for_group_values;
  //empty_data_row;

  DVector<double> pred_sum_all;
  DVector<double> pred_sum_all_but5;
  DVector<double> pred_this;

  e_q_term* cache;
  e_q_term* cache_test;

  DVector<relation_cache*> rel_cache;

public:
  uint max_iter;//??
  uint num_iter;
  uint num_eval_cases;

  double alpha_0, gamma_0, beta_0, mu_0;
  double alpha;

  double w0_mean_0;

  DVector<double> w_mu, w_lambda;
  DMatrix<double> v_mu, v_lambda;

  bool do_sample;
  bool do_multilevel;
  uint nan_cntr_v, nan_cntr_w, nan_cntr_w0, nan_cntr_alpha, nan_cntr_w_mu, nan_cntr_w_lambda, nan_cntr_v_mu, nan_cntr_v_lambda;
	uint inf_cntr_v, inf_cntr_w, inf_cntr_w0, inf_cntr_alpha, inf_cntr_w_mu, inf_cntr_w_lambda, inf_cntr_v_mu, inf_cntr_v_lambda;

protected:
  virtual void _learn(Data& train, Data&test);
  void predict_data_and_write_to_eterms(DVector<Data*>& main_data, DVector<e_q_term*>& main_cache);
  void add_main_q(Data& train, uing f);
  void draw_all(Data& train);
  void draw_w0(double& w0, double& reg, Data& train);
  void draw_w(double& w, double& w_mu, double& w_lambda, sparse_row<DATA_FLOAT>& feature_data);
  void draw_w_rel(double& w, double& w_mu, double& w_lambda, sparse_row<DATA_FLOAT>& feature_data, relation_cache* r_cache);
  void draw_v(double& v, double& v_mu, double& v_lambda, sparse_row<DATA_FLOAT>& feature_data);
  void draw_v_rel(double& v, double& v_mu, double& v_lambda, sparse_row<DATA_FLOAT>& feature_data, relation_cache* r_cache);
  void draw_alpha(double& alpha, uint num_train_total);
  void draw_w_mu(double* w);
  void draw_w_lambda(double* w);
  void draw_v_mu();
  void draw_v_lambda();

public:
  virtual void predict(Data& data, DVector<double>* out);
  virtual void init();
  virtual void lean();
};

void MCMC_ALS_Learner::init()
{
  Learner::init();
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
  v_lambda.setSize(0.0);
}

virtual void MCMC_ALS_Learner::learn(Data& train, Data& test)
{
  pred_sum_all.setSize(test.num_cases);
  pred_sum_all.init(0.0);
  pred_sum_all_but5.setSize(test.num_cases);
  pred_sum_all_but5.init(0.0);
  pred_this.setSize(test.num_cases);
  pred_this.init(0.0);

  cache = new e_q_term[train.num_cases]; //TODO ~MCMC_ALS_Learner()??
  cache_test = new e_q_term[test.num_cases];

  rel_cache.setSize(train.relation.size());
  for (uint i = 0; i < train.relation.size(); ++i)
  {
    uint rel_num_cases = train.relation[i].data->num_cases;
    rel_cache[i] = new relation_cache[rel_num_cases]; //TODO
    relation_cache* it = rel_cache[i]
    for (uint j = 0; j < rel_num_cases; ++j)
    {
      it->wnum = 0;
      it ++;
    }
  }

  for (uint i = 0; i < train.relation.size(); ++i)
  {
    for (uint j = 0; j < train.relation[i].data_row_to_relation_row.size(); ++j)
    {
      rel_cache[i][train.relation[r].data_row_to_relation_row[r]].wnum += 1.0;
    }
  }

  _learn(train, test);

  // free data structures
  for(uint i = 0; i < train.relation.size(); i++) { delete [] rel_cache[i]; }
  delete [] cache_test;
  delete [] cache;
}


#endif
