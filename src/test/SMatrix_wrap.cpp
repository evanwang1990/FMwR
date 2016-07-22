// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
#include <Rcpp.h>
#include <algorithm>
#include <vector>

using namespace Rcpp;
using namespace std;


// [[Rcpp::export]]
SEXP asdgCMatrix_( NumericMatrix XX_ , bool transpose = true){
  typedef Eigen::SparseMatrix<double, Eigen::RowMajor> SpMatR;
  typedef Eigen::SparseMatrix<double> SpMat;
  typedef Eigen::Map<Eigen::MatrixXd> MapMatd; // Input: must be double
  MapMatd X(Rcpp::as<MapMatd>(XX_));
  if (!transpose)
  {
    SpMatR Xsparse_r = X.sparseView();
    S4 Xout(wrap(Xsparse_r));                      // Output: as S4 object
    return Xout;
  }
  else
  {
    SpMat Xsparse = X.sparseView();
    S4 Xout(wrap(Xsparse));                      // Output: as S4 object
    return Xout;
  }

}



return List::create(
  _["size"] = size,
  _["row_idx"] = row_idx,
  _["value"] = value
);
}

// [[Rcpp::export]]
List as_SMatrix1(NumericMatrix data, bool transpose = true)
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
      if (j == nr) { j = 0; it_row = row_size.begin(); }

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
      it_row ++;
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
    _["row_size"] = row_size
  );
}


// [[Rcpp::export]]
List as_SMatrix2(NumericMatrix data, bool transpose = true)
{
  int nr = data.nrow(), nc = data.ncol();
  vector<int> row_size(nr);
  uint size = 0;
  NumericMatrix::iterator it_data = data.begin();
  vector<double> value(nr*nc);
  vector<double>::iterator it_value = value.begin();

  if (transpose)
  {
    value = as< vector<double> >(data);
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
    if (j == thres) { j = 0; it_row = row_size.begin(); }
    if (*it1 != 0.0)
    {
      if (it1 != it2) *it2 = *it1;
      it2 ++;
      *it_col = j;
      it_col ++;
      *it_row += 1;
    }
    it1 ++;
    it_row ++;
  }

  value.resize(size);

  return List::create(
    _["value"] = value,
    _["col_idx"] = col_idx,
    _["row_size"] = row_size
  );
}

// test
/***R
library(microbenchmark)
library(magrittr)

#### dense matrix test

x <- matrix(runif(10000), ncol = 10)
microbenchmark(asdgCMatrix_(x), as_SMatrix1(x), as_SMatrix2(x), times = 100)

#Unit: microseconds
#expr     min       lq     mean   median      uq      max neval
#  asdgCMatrix_(x) 134.284 203.9950 221.0925 230.0620 241.381  277.994   100
#as_SMatrix1(x)  59.980  99.1315 190.6988 113.8490 118.504 8206.795   100
#as_SMatrix2(x) 136.174 178.8955 189.5789 194.2925 198.602  237.530   100

microbenchmark(asdgCMatrix_(x,F), as_SMatrix1(x,F), as_SMatrix2(x,F), times = 100)

#Unit: microseconds
#expr     min       lq     mean   median       uq     max neval
#asdgCMatrix_(x, F) 171.277 208.0880 215.5542 212.9125 221.5345 270.515   100
#as_SMatrix1(x, F)  79.316 133.0020 138.3638 136.0040 142.8575 255.829   100
#as_SMatrix2(x, F) 121.061 161.3555 168.0353 164.8330 174.3090 218.315   100

x <- matrix(runif(10^7), ncol = 1000)
microbenchmark(asdgCMatrix_(x), as_SMatrix1(x), as_SMatrix2(x), times = 20)

#Unit: milliseconds
#expr       min        lq     mean   median       uq      max neval
#asdgCMatrix_(x) 1504.8021 1603.3375 1751.652 1719.653 1863.432 2139.747    20
#as_SMatrix1(x)  679.4929  913.8314 1043.575 1072.790 1166.267 1408.960    20
#as_SMatrix2(x) 1035.7741 1124.8788 1343.984 1212.458 1511.979 2058.597    20

microbenchmark(asdgCMatrix_(x,F), as_SMatrix1(x,F), as_SMatrix2(x,F), times = 20)

#Unit: milliseconds
#expr       min        lq     mean   median       uq      max neval
#asdgCMatrix_(x, F) 1007.8457 1378.6914 1464.809 1516.009 1580.205 2006.375  20
#as_SMatrix1(x, F)  803.5353  959.2372 1211.153 1265.337 1370.626 1649.608     20
#as_SMatrix2(x, F)  737.5675 1003.5448 1134.917 1163.909 1303.555 1503.487     20


#### sparse matrix test
x <- matrix(sample(c(rep(0.0, 5), runif(5)), 10000, replace = T), ncol = 10)
microbenchmark(asdgCMatrix_(x), as_SMatrix1(x), as_SMatrix2(x), times = 100)

#Unit: microseconds
#expr     min       lq     mean   median      uq     max neval
#asdgCMatrix_(x) 164.184 174.2070 184.8242 185.8805 192.397 235.678   100
#as_SMatrix1(x) 107.335 119.4525 123.2428 124.4840 128.663 142.209   100
#as_SMatrix2(x) 196.551 216.5200 224.4426 222.6125 230.566 262.964   100

microbenchmark(asdgCMatrix_(x,F), as_SMatrix1(x,F), as_SMatrix2(x,F), times = 100)

#Unit: microseconds
#expr     min       lq     mean   median       uq     max neval
#asdgCMatrix_(x, F) 174.289 181.4430 189.6683 188.4745 194.7725 228.767   100
#as_SMatrix1(x, F) 114.422 118.9365 123.7542 121.7165 126.3755 163.298   100
#as_SMatrix2(x, F) 144.032 148.0585 153.2831 152.0905 157.3460 196.752   100

x <- matrix(sample(c(rep(0.0, 5), runif(5)), 10^7, replace = T), ncol = 1000)
microbenchmark(asdgCMatrix_(x), as_SMatrix1(x), as_SMatrix2(x), times = 20)

#Unit: milliseconds
#expr      min       lq     mean   median       uq      max neval
#asdgCMatrix_(x) 167.5998 179.3690 201.1917 182.2489 195.8420 342.1056    20
#as_SMatrix1(x) 118.7033 123.5998 134.3742 125.1867 129.1712 286.3430    20
#as_SMatrix2(x) 215.6302 218.0311 232.5586 220.1486 228.1834 373.2674    20

microbenchmark(asdgCMatrix_(x,F), as_SMatrix1(x,F), as_SMatrix2(x,F), times = 20)

#Unit: milliseconds
#expr      min       lq     mean   median       uq      max neval
#asdgCMatrix_(x, F) 261.8739 267.4506 273.0739 270.5435 276.3130 299.1941    20
#as_SMatrix1(x, F) 133.4742 138.2319 155.7176 138.9910 143.5585 305.2889    20
#as_SMatrix2(x, F) 162.1562 167.9273 201.4681 171.6636 173.3474 333.6482    20


#### super sparse matrix test
x <- matrix(sample(c(rep(0.0, 10), runif(3)), 10000, replace = T), ncol = 10)
microbenchmark(asdgCMatrix_(x), as_SMatrix1(x), as_SMatrix2(x), times = 100)

#Unit: microseconds
#expr     min       lq      mean   median       uq     max neval
#asdgCMatrix_(x)  91.450  99.0725 105.43060 104.8775 108.2820 152.714   100
#as_SMatrix1(x)  72.144  75.3435  78.83952  77.5845  80.4655 107.966   100
#as_SMatrix2(x) 158.075 163.8365 168.09954 165.8610 169.4240 243.402   100

microbenchmark(asdgCMatrix_(x,F), as_SMatrix1(x,F), as_SMatrix2(x,F), times = 100)

#Unit: microseconds
#expr     min       lq      mean   median       uq     max neval
#asdgCMatrix_(x, F) 103.861 109.3300 112.68068 111.8245 113.7055 158.746   100
#as_SMatrix1(x, F)  70.313  72.1560  74.54341  73.0605  74.7085 123.648   100
#as_SMatrix2(x, F)  91.730  94.3655  96.57739  95.8645  97.7170 114.822   100

x <- matrix(sample(c(rep(0.0, 10), runif(3)), 10^7, replace = T), ncol = 1000)
microbenchmark(asdgCMatrix_(x), as_SMatrix1(x), as_SMatrix2(x), times = 20)

#Unit: milliseconds
#expr       min       lq     mean   median       uq       max neval
#asdgCMatrix_(x)  80.33279 105.9706 213.8027 120.6875 232.6643  634.0423    20
#as_SMatrix1(x)  99.02890 437.7283 496.8148 559.9737 610.4286  831.8505    20
#as_SMatrix2(x) 223.06373 606.4854 723.9024 695.8563 757.4859 1208.9416    20

microbenchmark(asdgCMatrix_(x,F), as_SMatrix1(x,F), as_SMatrix2(x,F), times = 20)

#Unit: milliseconds
#expr      min       lq     mean   median       uq      max neval
#asdgCMatrix_(x, F) 144.3443 158.9455 192.1334 163.7748 170.8159 544.9277    20
#as_SMatrix1(x, F) 240.2239 540.3864 562.6528 584.7727 652.2340 788.5258    20
#as_SMatrix2(x, F) 229.8755 259.0549 479.1711 525.2788 641.4731 794.3647    20
*/


// conclusion:
// when the matrix is too large use as_SMatrix2 which is memory efficient
// when matrix is small or dense use as_SMatrix1 which is fast
