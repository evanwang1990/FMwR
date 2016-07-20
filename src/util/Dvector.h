#ifndef DVECTOR_H_
#define DVECTOR_H_

#include <Rcpp.h>

template <typename T> class DVector
{
public:
  uint dim;
  T* value;

public:
  DVector()
  {
    dim = 0;
    value = NULL;
  }

  DVector(uint p_dim)
  {
    dim = 0;
    value = NULL;
    setSize(p_dim);
  }

  ~DVector()
  {
    if (value != NULL) { delete [] value; }
  }

public:
  void setSize(uint p_dim)
  {
    if (p_dim == dim) { return; }
    if (value != NULL) { delete [] value; }
    dim = p_dim;
    value = new T[dim];
  }

  void init(T v)
  {
    for (uint i = 0; i < dim; i++) { value[i] = v; }
  }

  T get(uint x) { return value[x]; }

  T& operator() (uint x) {return value[x]; }

  T operator() (uint x) const { return value[x]; }

  void wrap(std::vector<T>& v)
  {
    assert((v.size() == dim) && "The length is different!");
    for (uint i = 0; i < dim; i++) { value[i] = v[i]; }
  }

  void assign(T* v)
  {
    if (v->dim != dim) { setSize(v->dim); }
    for (uint i = 0; i < dim; i++) { value[i] = v[i]; }
  }

  void assign(DVector<T>& v)
  {
    if (v.dim != dim) { setSize(v.dim); }
    for (uint i = 0; i < dim; i++) { value[i] = v.value[i]; }
  }

public:
  T* begin() { return value; }

  T* end() { return value + dim; }

  uint size() { return dim; }
};

#endif
