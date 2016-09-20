#ifndef FM_LEARN__
#define FM_LEARN__

#include <Rcpp.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include <vector>
#include <utility>
#include "../util/Macros.h"
#include "../util/Smatrix.h"
#include "../util/Macros.h"
#include "Data.h"
#include "Model.h"
#include "Evaluation.h"
#include "Validator.h"
using namespace Rcpp;

class Learner
{
public:
  friend class Validator;

public:
  DataMetaInfo* meta;
  Model* fm;
  double min_target;
  double max_target;
  int nthreads;
  int max_iter;

// validation
public:
  Validator tracker;
  int type;

// convergence condtions
public:
  bool convergent;
  double conv_condition;
  int conv_times;

public:
  Learner(): meta(NULL), nthreads(1), max_iter(3000), convergent(false) , conv_condition(1e-4), conv_times(0){}

  ~Learner() {}

public:
  virtual void init() {}
  virtual void learn(Data& train, Data& test) {}
  virtual void learn(Data& train) {}
  virtual double evaluate(DVector<double>& y_hat, DVector<float>& y_true);
};

double Learner::evaluate(DVector<double>& y_hat, DVector<float>& y_true)
{
  return evaluates(fm, y_hat, y_true, type);
}

#endif
