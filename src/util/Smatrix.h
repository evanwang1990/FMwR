#ifndef LSMATRIX_H_
#define LSMATRIX_H_

#include <Rcpp.h>
#include <assert.h>
#include "Dvector.h"

typedef unsigned long long int uint64;
typedef signed long long int int64;

template <typename T> struct sparse_entry
{
  uint id;
  T value;
};

template <typename T> struct sparse_row
{
  sparse_entry<T>* data;
  uint size;
};

template <typename T> class SMatrixMeta
{
public:
  virtual void begin() = 0;
  virtual bool end() = 0;
  virtual void next() = 0;
  virtual sparse_row<T>& get_row() = 0;
  virtual uint get_row_index() = 0;
  virtual uint nrow() = 0;
  virtual uint ncol() = 0;
  virtual uint size() = 0;
};

template <typename T> class SMatrix//: public SMatrixMeta<T>
{
protected:
  DVector<uint64*> row_idx;
  DVector<uint64> col_idx;
  DVector<T> value;

public:
  uint dim1;
  uint dim2;
  uint size;

public:
  SMatrix()
    : dim1(0), dim2(0), size(0)
  {}

  SMatrix(List& _list)
  {
    assert((_list.attr("class") == "SMatrix") && "The input is not SMatrix...");
    IntegerVector dim      = _list["dim"];
    dim1                   = dim[0];
    dim2                   = dim[1];
    size                   = (uint)_list["size"];
    IntegerVector row_size = _list["row_size"];
    assert((dim1 == row_size.size()) && "the length of row_size is not equal to nrow...");
    value.assign(_list["value"]);
    col_idx.assign(_list["col_idx"]);
    row_idx.setSize(dim1);
    uint64* it = col_idx.begin();
    for (uint i = 0; i < dim1; ++i)
    {
      row_idx(i) = it;
      it += row_size[i];
    }
  }
};


#endif
