#ifndef FM_LEARN__
#define FM_LEARN__

#include <Rcpp.h>
#include "Data.h"
#include "Model.h"
using namespace Rcpp;

#define CLASSIFICATION 10
#define REGRESSION     20
#define RANKING        30

class Learner
{
protected:
  DVector<double> sum, sum_sqr;
  DMatrix<double> pred_q_term;

protected:
  virtual double predict_case(SMatrix<float>::Iterator it)
  {
    return fm->predict(SMatrix<float>::Iterator it)
  }

public:
  DataMetaInfo* meta;
  Data* validation;
  Model* fm;
  float min_target;
  float max_target;

public:
  int TASK;

public:
  Learner(): TASK(CLASSIFICATION), meta(NULL) {}

  ~Learner() {}

  virtual void init ()
  {
    sum.setSize(fm->num_factor);
    sum_sqr.setSize(fm->num_factor);
    pred_q_term.setSize(fm->num_factor, meta->num_relations + 1);
  }

  virtual void learn(Data& train, Data& test) {}

  virtual void predict(Data& data, DVector<double>& out) = 0;

  virtual double evaluate(data& data)
  {
    if (data.data == NULL) { stop("there's no data..."); }
    if (TASK == CLASSIFICATION) { return evaluate_classification(data); }
    else if (TASK == REGRESSION) { return evaluate_regression(data); }
    else { stop("unknown task..."); }
  }

public:
  virtual double evaluate_classification(Data& data) //TODO: add threshold??
  {
    uint num_correct = 0;
    uint num_cases = data.data->nrow();
    double p;

    #pragma omp parallel for private(p) reduction(+:num_correct)
    for (uint i = 0; i < num_cases; ++i)
    {
      p = predict_case(Matrix<float>::Iterator it(data.data, i));
      if ( ((p >= 0) && (data.target[i] >= 0)) || ((p < 0) && (data.target[i] < 0)) )
      { num_correct += 1; }
    }
    return (double)num_correct / (double)num_cases;
  }

  virtual double evaluate_regression(Data& data)
  {
    double rmse_sum_sqr = 0.0;
    double mae_sum_abs = 0.0; //TODO: not used
    double y_hat, err;
    uint num_cases = data.data->nrow();

    #pragma omp parallel for private(y_hat) private(err) reduction(+:rmse_sum_sqr) reduction(+:mae_sum_abs)
    for (uint i = 0; i < num_cases; ++i)
    {
      y_hat = predict_case(Matrix<float>::Iterator it(data.data, i));
      y_hat = max(min(y_hat, max_target), min_target);
      err = y_hat - data.target[i];
      rmse_sum_sqr += err * err;
      mae_sum_abs  += abs(err);
    }
    return sqrt(rmse_sum_sqr / num_cases);
  }

};

#endif
