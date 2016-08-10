#ifndef FM_LEARN__
#define FM_LEARN__

#include <Rcpp.h>
#include "util/Macros.h"
#include "util/Smatrix.h"
#include "Data.h"
#include "Model.h"
using namespace Rcpp;

class Learner
{
public:
  DataMetaInfo* meta;
  Data* validation;
  Model* fm;
  double min_target;
  double max_target;
  int nthreads;

public:
  int TASK;

public:
  Learner(): TASK(CLASSIFICATION), meta(NULL), nthreads(1) {}

  ~Learner() {}

  // virtual void init ()
  // {
  //   sum.setSize(fm->num_factor);
  //   sum_sqr.setSize(fm->num_factor);
  //   pred_q_term.setSize(fm->num_factor, meta->num_relations + 1);
  // }

  virtual void learn(Data& train, Data& test) {}

  virtual void learn(Data& train) {}

  // virtual void predict(Data& data, DVector<double>& out) = 0;

  virtual double evaluate(Data& data)
  {
    if (data.data == NULL) { stop("there's no data..."); }
    if (TASK == CLASSIFICATION) { return evaluate_classification(data); }
    else if (TASK == REGRESSION) { return evaluate_regression(data); }
    else { stop("unknown task..."); }
    return R_NegInf;
  }

public:
  virtual double evaluate_classification(Data& data) //TODO: add threshold??
  {
    SMatrix<float>* pdata = data.data;
    DVector<float>* ptarget = data.target;
    uint num_cases = pdata->nrow();
    DVector<double> p(num_cases);
    fm->predict_batch(data, p);

    uint num_correct = 0;
    #pragma omp parallel for num_threads(nthreads) private(p) reduction(+:num_correct)
    for (uint i = 0; i < num_cases; ++i)
    {
      if ( ((p[i] >= 0) && ((*ptarget)[i] >= 0)) || ((p[i] < 0) && ((*ptarget)[i] < 0)) )
      { num_correct += 1; }
    }
    return (double)num_correct / (double)num_cases;
  }

  virtual double evaluate_regression(Data& data)
  {
    SMatrix<float>* pdata = data.data;
    DVector<float>* ptarget = data.target;
    uint num_cases = pdata->nrow();
    DVector<double> p(num_cases);
    fm->predict_batch(data, p);

    double rmse_sum_sqr = 0.0;
    double mae_sum_abs = 0.0; //TODO: not used
    double y_hat, err;
    #pragma omp parallel for num_threads(nthreads) private(y_hat) private(err) reduction(+:rmse_sum_sqr) reduction(+:mae_sum_abs)
    for (uint i = 0; i < num_cases; ++i)
    {
      y_hat = std::min(p[i], max_target);
      y_hat = std::max(y_hat, min_target);
      err = y_hat - (*ptarget)[i];
      rmse_sum_sqr += err * err;
      mae_sum_abs  += abs(err);
    }
    return sqrt(rmse_sum_sqr / num_cases);
  }

};

#endif
