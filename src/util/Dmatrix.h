#ifndef DMATRIX_H_
#define DMATRIX_H_

#include <vector>
#include <string.h>
#include <assert.h>

template <typename T> class DMatrix
{
public:
  T** value; //each element is a pointer to the first value of each row
  std::vector<std::string> col_names; //TODO:need?
  uint dim1, dim2;

public:
  DMatrix() { dim1 = 0; dim2 = 0; value = NULL; }

  DMatrix(uint p_dim1, uint p_dim2)
  {
    dim1 = 0; dim2 = 0; value = NULL;
    setSize(p_dim1, p_dim2);
  }

  ~DMatrix()
  {
    if (value != NULL)
    {
      delete [] value[0]; // delete value
      delete [] value;  // delete row pointer
    }
  }

  void setSize(uint p_dim1, uint p_dim2)
  {
    if ((p_dim1 == dim1) && (p_dim2 == dim2)) { return; }
    if (value != NULL)
    {
      delete [] value[0]; // delete value
      delete [] value;    // delete row pointer
    }
    dim1 = p_dim1;
    dim2 = p_dim2;
    value = new T*[dim1];
    value[0] = new T[dim1 * dim2];
    for (uint i = 1; i < dim1; i++) { value[i] = value[0] + dim2 * i; }
    col_names.resize(dim2);
    for (std::vector<std::string>::iterator it = col_names.begin(); it != col_names.end(); ++it) { *it = ""; }
  }

  void init(T v)
  {
    for (T* p = this->begin(); p != this->end(); ++p) { *p = v; }
  }

  void assign(DMatrix<T>& v)
  {
    if ((dim1 != v.dim1) && (dim2 != v.dim2)) { setSize(v.dim1, v.dim2); }
    for (T *it0 = this->begin(), *it1 = v.begin(); it0 != this->end(); ++it0, ++it1) { *it0 = *it1; }
  }

  T& operator() (uint x, uint y) { return value[x][y]; }

  T operator() (uint x, uint y) const { return value[x][y]; }

  T* row_begin(uint x) const { return value[x]; }

  T* row_end(uint x) const { return value[x] + dim2; }

  T* begin() const { return value[0]; }

  T* end() const { return value[0] + dim1 * dim2; }

  uint nrow() const { return dim1; }

  uint ncol() const { return dim2; }

};

class DMatrixDouble: public DMatrix<double>
{
public:
  DMatrixDouble(): DMatrix<double>() {}

  DMatrixDouble(uint p_dim1, uint p_dim2)
    : DMatrix<double>(p_dim1, p_dim2)
  {}

  void init_norm(double mean, double stdev)
  {
    for (double* it = begin(); it != end(); ++it) { *it = Rf_rnorm(mean, stdev); }
  }

  Rcpp::NumericMatrix to_rtype()
  {
    Rcpp::NumericMatrix result(dim1, dim2);
    for (uint i = 0; i < dim1; ++i)
    {
      for (uint j = 0; j < dim2; ++j) { result(i, j) = value[i][j]; }
    }
    return result;
  }
};

#endif
