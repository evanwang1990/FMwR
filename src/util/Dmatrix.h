#ifndef DMATRIX_H_
#define DMATRIX_H_

#include <vector>
#include <string.h>
#include <assert.h>

template <typename T> class DMatrix
{
public:
  T** value; //each element is a pointer to the first value of each row
  std::vector<std::string> col_names;
  uint dim1, dim2;

public:
  DMatrix() { dim1 = 0; dim2 = 0; value = NULL; }

  DMatrix(uint p_dim1, uint p_dim2)
  {
    dim1 = p_dim1; dim2 = p_dim2; value = NULL;
    setSize(p_dim1, p_dim2);
  }

  ~DMatrix()
  {
    if (value != NULL)
    {
      delete [] value[0]; // delete value array
      delete [] value;  // delete pointer
    }
  }

  void setSize(uint p_dim1, uint p_dim2)
  {
    if ((p_dim1 == dim1) && (p_dim2 == dim2)) { return; }

  }
};

#endif
