#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

typedef unsigned long long int uint64;

List as_SMatrix1(NumericMatrix data, bool transpose);
List as_SMatrix2(NumericMatrix data, bool transpose);

// [[Rcpp::export]]
List as_SMatrix(NumericMatrix data, bool transpose = true)
{
  List smatrix;
  if (sizeof(data(0, 0)) * data.nrow() * data.ncol() > 1073741824)
    smatrix = as_SMatrix2(data, transpose);
  else
    smatrix = as_SMatrix1(data, transpose);
  smatrix.attr("class") = "SMatrix";
  return smatrix;
}


List as_SMatrix1(NumericMatrix data, bool transpose)
{
  int nr = data.nrow(), nc = data.ncol();
  vector<double> value(nr*nc);
  vector<int> col_idx(nr*nc);
  vector<int> row_size(nr);

  NumericMatrix::iterator it_data = data.begin();
  vector<double>::iterator it_value = value.begin();
  vector<int>::iterator it_col = col_idx.begin();
  vector<int>::iterator it_row = row_size.begin();

  uint k = 0;
  if (transpose)
  {
    for (int i = 0, j = 0; i < nr*nc; ++i, ++j)
    {
      if (j == nr) { j = 0; it_row ++; }

      double val = *it_data;
      if (val != 0.0)
      {
        *it_value = val;
        it_value ++;
        *it_col = j;
        it_col ++;
        *it_row += 1;
        k ++;
      }
      it_data ++;
    }
  }
  else
  {
    for (int i = 0; i < nr; ++i)
    {
      it_data = data.begin() + i;
      for (int j = 0; j < nc; ++j)
      {
        double val = *it_data;
        if (val != 0.0)
        {
          *it_value = val;
          it_value ++;
          *it_col = j;
          it_col ++;
          *it_row += 1;
          k++;
        }
        it_data += nr;
      }
      it_row ++;
    }
  }

  value.resize(k);
  col_idx.resize(k);
  return List::create(
    _["value"] = value,
    _["col_idx"] = col_idx,
    _["row_size"] = row_size,
    _["dim"] = IntegerVector::create(nr, nc),
    _["size"] = k
  );
}


List as_SMatrix2(NumericMatrix data, bool transpose)
{
  int nr = data.nrow(), nc = data.ncol();
  vector<int> row_size(nr);
  uint size = 0;
  NumericMatrix::iterator it_data = data.begin();
  vector<double> value(nr*nc);
  vector<double>::iterator it_value = value.begin();

  if (transpose)
  {
    value.assign(data.begin(), data.end());
    for (;it_data != data.end(); ++it_data)
    {
      if (*it_data != 0.0) { size += 1; }
    }
  }
  else
  {
    for (int i = 0; i < nc; ++i)
    {
      it_data  = data.begin() + i * nr;
      it_value = value.begin() + i;
      for(int j = 0; j < nr; ++j)
      {
        double val = *it_data;
        if (val != 0.0) size += 1;
        *it_value = val;
        it_data ++;
        it_value += nc;
      }
    }
  }

  it_value = value.begin();
  vector<int> col_idx(size);
  vector<double>::iterator it1 = it_value, it2 = it_value;
  vector<int>::iterator it_row = row_size.begin();
  vector<int>::iterator it_col = col_idx.begin();

  int thres = transpose ? nr : nc;
  for (int i = 0, j = 0; i < nr * nc; ++i, ++j)
  {
    if (j == thres) { j = 0; it_row ++; }
    if (*it1 != 0.0)
    {
      if (it1 != it2) *it2 = *it1;
      it2 ++;
      *it_col = j;
      it_col ++;
      *it_row += 1;
    }
    it1 ++;
  }

  value.resize(size);

  return  List::create(
    _["value"] = value,
    _["col_idx"] = col_idx,
    _["row_size"] = row_size,
    _["dim"] = IntegerVector::create(nr, nc),
    _["size"] = size
  );
}
