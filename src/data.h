#ifndef DATA_H_
#define DATA_H_

class DataMetaInfo
{
public:
  IntegerVector attr_group;
  int num_attr_groups;
  IntegerVector num_attr_per_group;
  int num_relations;

public:
  DataMetaInfo(int num_attributes);
}

DataMetaInfo::DataMetaInfo(int num_attributes)
{
  attr_group = IntegerVector(num_attributes);
  num_attr_groups = 1;
  num_attr_per_group = IntegerVector(num_attr_groups);
  num_attr_per_group[0] = num_attributes;
}

#include "relation.h"

class Data
{
protected:
  int cache_size;
  bool has_xt;
  bool has_x;

public:
  NumericMatrix data_t;
  NumericMatrix data;
  NumericVector target;
  int num_features;
  int num_cases;
  double min_target;
  double max_target;
  

public:
  Data(int cache_size, bool has_xt, bool has_x);
}
