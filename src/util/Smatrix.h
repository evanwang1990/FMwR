#ifndef LSMATRIX_H_
#define LSMATRIX_H_

#include <Rcpp.h>
#include <assert.h>
#include "Dvector.h"

typedef unsigned long long int uint64;
typedef signed long long int int64;

template <typename T> class SMatrixMeta
{
public:
  virtual void begin() = 0;
  virtual bool end() = 0;
  virtual void next() = 0;
//  virtual sparse_row<T>& get_row() = 0;
  virtual uint get_row_index() = 0;
  virtual uint nrow() = 0;
  virtual uint ncol() = 0;
  virtual uint size() = 0;
};

template <typename T> class SMatrix//: public SMatrixMeta<T>
{
protected:
  DVector<uint> row_idx;
  DVector<uint> col_idx;
  DVector<T> value;

public:
  uint dim1;
  uint dim2;
  uint size;

public:
  SMatrix()
    : dim1(0), dim2(0), size(0)
  {}

  SMatrix(List& _list) //TODO:统一数据类型
  {
    assert((_list.attr("class") == "SMatrix") && "The input is not SMatrix...");
    IntegerVector dim      = _list["dim"];
    dim1                   = dim[0];
    dim2                   = dim[1];
    size                   = _list["size"];
    IntegerVector row_size = _list["row_size"];
    assert((dim1 == row_size.size()) && "the length of row_size is not equal to nrow...");
    value.assign(_list["value"]);
    col_idx.assign(_list["col_idx"]);
    row_idx.setSize(dim1);
    row_idx.init(0);
    for (uint i = 1; i < dim1; ++i)
    {
      row_idx[i] = row_idx[i-1] + row_size[i-1];
    }
  }

  T operator() (uint i, uint j) const
  {
    if (size == 0) { return 0.0; } // SMatrix is empty
    uint low = row_idx[i];
    uint high = (i == (dim1 - 1)) ? col_idx.size() : row_idx[i+1];
    if (high == low) { return 0.0; } // i row is empty
    if ((j > col_idx[high-1]) || (j < col_idx[low])) { return 0.0; } // not in range
    if (col_idx[low] == j) { return value[low]; }
    if (col_idx[high-1] == j) { return value[high-1]; }
    uint id = search_column(low, high, j);
    return (col_idx[id] == j) ? value[id] : 0.0;
  }

  uint search_column(uint low, uint high, uint column) const
  {
    uint mid;
    while (high > low)
    {
      mid = (low + high) >> 1;
      if (col_idx[mid] == column) { break; }
      if (mid < column) { low = mid + 1; }
      else { high = mid; }
    }
    return mid;
  }

  uint nrow() const { return dim1; }

  uint ncol() const { return dim2; }

  uint nvalues() const { return size; }

public:
  class Iterator;

};

template<typename T>
class SMatrix<T>::Iterator
{
protected:
  const SMatrix<T>& matrix;
  uint begin;
  uint end;
  uint pointer;
public:
  uint index;
  T value;

public:
  Iterator(const SMatrix<T>& _matrix, uint row_no)
    : matrix(_matrix)
  {
    begin   = matrix.row_idx[row_no];
    end     = (row_no == (matrix.dim1 - 1)) ? matrix.size : matrix.row_idx[row_no+1];
    pointer = begin;
    index   = matrix.col_idx[pointer];
    value   = matrix.value[pointer];
  }

  Iterator(const SMatrix<T>& _matrix)
    : matrix(_matrix), begin(0), end(_matrix.size), pointer(0), index(_matrix.col_idx[0]), value(_matrix.value[0])
  {}

  ~Iterator() {}

  bool is_end() { return pointer == end; }

  void operator ++()
  {
    pointer += 1;
    index = matrix.col_idx[pointer];
    value = matrix.value[pointer];
  }

};


#endif
