#ifndef FM_LEARN__
#define FM_LEARN__

#include <Rcpp.h>
#include <omp.h>
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
  Data* validation;
  Model* fm;
  double min_target;
  double max_target;
  int nthreads;
  uint max_iter;

public:
  Validator tracker;
  int type;

public:
  Learner(): meta(NULL), nthreads(1), max_iter(3000) {}

  ~Learner() {}

public:
  virtual void learn(Data& train, Data& test) {}
  virtual void learn(Data& train) {}
  virtual double evaluate(DVector<double>& y_hat, DVector<float>& y_true);
};

double Learner::evaluate(DVector<double>& y_hat, DVector<float>& y_true)
{
  return evaluates(fm, y_hat, y_true, type);
}

#endif
