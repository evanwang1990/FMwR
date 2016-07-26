#ifndef DATA_H_
#define DATA_H_

#include "util/Dvector.h"
#include "util/Dmatrix.h"
#include "util/Smatrix.h"

using namespace Rcpp;

class DataMetaInfo
{
public:
  DVector<uint> attr_group;
  uint num_attr_groups;
  DVector<uint> num_attr_per_group;
  uint num_relations;

public:
  DataMetaInfo(uint num_attributes)
    : num_attr_groups(1)
  {
    attr_group.setSize(num_attributes);
    num_attr_per_group.setSize(num_attributes);
    num_attr_per_group[0] = num_attributes;
  }
};




class RelationData
{
protected:
  bool has_x, has_xt;

public:
  DataMetaInfo* meta;
  SMatrix<float>* data;
  SMatrix<float>* data_t;
  uint num_features;
  uint num_cases;
  uint attr_offset;

public:
  RelationData()
    : data(NULL), data_t(NULL), has_x(false), has_xt(false), meta(NULL)
  {}

  void load(List matrix)
  {
    if (matrix.attr("class") != "SMatrix")
      stop("the data to be loaded is not SMatrix format");

    if (matrix.attr("transposed") == true)
    {
      has_xt = true;
      data_t = new SMatrix<float>(matrix);
      num_features = data_t.nrow();
      num_cases = data_t.ncol();
    }
    else
    {
      has_x  = true;
      data = new SMatrix<float>(matrix);
      num_features = data.ncol();
      num_cases = data.nrow();
    }

    if (has_x && has_xt)
    {
      if (data_t.ncol() != data.nrow()) { stop("data's rows not equal to data_t's columns..."); }
      if (data_t.nrow() != data.ncol()) { stop("data's columns not equal to data_t's rows..."); }
      if (data_t.nvalues() != data.nvalues()) { stop("data's values not equal to data_t's value"); }
    }

    if (!has_x && !has_xt)
      meta = new DataMetaInfo(num_features);
  }
};



class RelationJoin
{
public:
  DVector<uint> data_row_to_relation_row;
  RelationData* data;

public:
  RelationJoin() {}
};


class Data
{
protected:
  bool has_x, has_xt;

public:
  SMatrix<float>* data_t;
  SMatrix<float>* data;
  DVector<float>* target;
  DVector<RelationJoin> relation;

public:
  uint num_features;
  uint num_cases;
  float min_target;
  float max_target;

public:
  Data ()
    : has_x(false), has_xt(false), data(NULL), data_t(NULL), target(NULL), min_target(R_PosInf), max_target(R_NegInf)
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

    if (!has_x && !has_xt)
      meta = new DataMetaInfo(num_features);
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
