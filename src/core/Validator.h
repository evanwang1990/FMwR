#ifndef VALIDATOR_H_
#define VALIDATOR_H_

#include <Rcpp.h>
#include "Evaluation.h"
#include "../util/Macros.h"

//TODO: 根据准则收敛或者确定max_iter两种方式下的track方式？ add MAX_REC and default valid_step
#define MAX_REC 10000

struct Params {
  double w0;
  DVector<double> w;
  DMatrix<double> v;
};

class Validator
{
public:
  uint max_iter;
  int step_size;
  int record_cnter;
  int record_times;
  DVector<Params> parameters;
  DVector<double> evaluations_of_train;
  DVector<double> evaluations_of_test;
  DVector<int> record_index;

  int type;

public:
  Validator() : step_size(-1), type(LL) {}
  void init();
  void record(Model* fm, int iter_idx, double eval_score);
  double evaluate(Model* fm, DVector<double>& y_hat, DVector<float>& y_true);
  void report(Model* fm, Data& data);
  List save();
  void load(Model* fm, List valid);
};

void Validator::init()
{
  record_cnter = 0;
  record_times = (int)(std::ceil(((double)max_iter - 0.5) / (double)step_size)) + 1;
  if (record_times > MAX_REC) {
    step_size = int((double)(max_iter + 1) / (double)MAX_REC) + 1;
    record_times = (int)(std::ceil(((double)max_iter - 0.5) / (double)step_size)) + 1;
  }
  record_index.setSize(record_times);
  parameters.setSize(record_times);
  evaluations_of_train.setSize(record_times);
  // evaluations_of_test.setSize(record_times);
}

void Validator::record(Model* fm, int iter_idx, double eval_score)
{
  Params& P = parameters[record_cnter];
  if (fm->k0) { P.w0 = fm->w0; }
  if (fm->k1) { P.w.assign(fm->w); }
  if (fm->num_factor > 0) { P.v.assign(fm->v); }
  evaluations_of_train[record_cnter] = eval_score;
  record_index[record_cnter] = iter_idx;
  record_cnter ++;
}

double Validator::evaluate(Model* fm, DVector<double>& y_hat, DVector<float>& y_true)
{
  return evaluates(fm, y_hat, y_true, type);
}

void Validator::report(Model* fm, Data& data)
{
  evaluations_of_test.setSize(record_times);
  DVector<double> out(data.num_cases);
  for (int i = 0; i < record_times; ++i)
  {
    Params& P = parameters[i];
    if (fm->k0) { fm->w0 = P.w0; }
    if (fm->k1) { fm->w.assign(P.w); }
    if (fm->num_factor > 0) { fm->v.assign(P.v); }
    fm->predict_prob(data, out);
    evaluations_of_test[i] = evaluate(fm, out, *data.target);
  }
}

List Validator::save()
{
  //TODO: record_times -> record_cnter ??
  List valid(record_times + 1);
  valid[0] = record_index.to_rtype();
  for (int i = 1; i <= record_times; ++i)
  {
    Params& P = parameters[i - 1];
    valid[i] = List::create(
      _["w0"] = P.w0,
      _["w"]  = P.w.to_rtype(),
      _["v"]  = P.v.to_rtype()
    );
  }
  return valid;
}

void Validator::load(Model* fm, List valid)
{
  record_times = valid.size() - 1;
  parameters.setSize(record_times);
  record_index.assign(as<NumericVector>(valid[0]));
  for (int i = 1; i <= record_times; ++i)
  {
    Params& P = parameters[i-1];
    List tmp_valid = valid[i];
    if (fm->k0) { P.w0 = tmp_valid["w0"]; }
    if (fm->k1) { P.w.assign(as<NumericVector>(tmp_valid["w"])); }
    if (fm->num_factor > 0) { P.v.assign(as<NumericMatrix>(tmp_valid["v"])); }
  }
}

#endif
