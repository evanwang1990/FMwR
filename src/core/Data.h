#ifndef DATA_H_
#define DATA_H_

#include "../util/Dvector.h"
#include "../util/Dmatrix.h"
#include "../util/Smatrix.h"

using namespace Rcpp;

class DataMetaInfo
{
public:
  DVector<uint> attr_group;
  uint num_attr_groups;
  DVector<uint> num_attr_per_group;

public:
  DataMetaInfo(uint num_attributes, uint num_attr_groups_ = 1)
    : num_attr_groups(num_attr_groups_)
  {
    attr_group.setSize(num_attributes);
    attr_group.init(0);
    num_attr_per_group.setSize(num_attr_groups);
    num_attr_per_group[0] = num_attributes;
  }
};


class Data
{
public:
  bool has_x, has_xt;
  SMatrix<float>* data_t;
  SMatrix<float>* data;
  DVector<float>* target;

public:
  uint num_features;
  uint num_cases;
  float min_target;
  float max_target;

public:
  Data ()
    : has_x(false), has_xt(false), data_t(NULL), data(NULL), target(NULL), min_target(R_PosInf), max_target(R_NegInf)
  {}

  void add_data(SMatrix<float>* _data)
  {
    if (!_data->transposed)
    {
      has_x = true;
      data = _data;
      num_features = _data->ncol();
      num_cases = _data->nrow();
    }
    else
    {
      has_xt = true;
      data_t = _data;
      num_features = _data->nrow();
      num_cases = _data->ncol();
    }

    if (has_x && has_xt)
    {
      if (data_t->ncol() != data->nrow()) { stop("data's rows not equal to data_t's columns..."); }
      if (data_t->nrow() != data->ncol()) { stop("data's columns not equal to data_t's rows..."); }
      if (data_t->nvalues() != data->nvalues()) { stop("data's values not equal to data_t's value"); }
    }

  }

  void add_target(DVector<float>* _target)
  {
    target = _target;
    if (!((has_x || has_xt) && (num_cases == target->size())))
    { stop("target's length is not equal the number of cases..."); }

    for(float* it = target->begin(); it != target->end(); ++it)
    {
      if (*it > max_target) max_target = *it;
      if (*it < min_target) min_target = *it;
    }
  }
};


#endif
