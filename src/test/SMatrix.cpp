#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

#include "../util/Smatrix.h"

/*
library(Rcpp)
setwd("/home/jinlingwang/project/github/FMwR/src/test/")
data <- matrix(c(1, 0, 0, 0,
                 0, 3, 4, 0,
                 0, 0, 5, 6,
                 0, 8, 0, 1),
               nrow = 4,
               byrow = T)
sdata <- as_SMatrix(data, T)
*/

void test_iter (SMatrix<int>::Iterator& it)
{
  for (; !it.is_end(); ++it)
  {
    cout<<it.index<<" : "<<it.value<<" | ";
  }
  cout<<endl;
}

// [[Rcpp::export]]
void test_smatrix(List x)
{
  SMatrix<int> y(x);
  // by index
  for (uint i = 0; i < y.nrow(); ++i)
  {
    for (uint j = 0; j < y.ncol(); ++j)
    {
      cout<<y(i, j)<<" ";
    }
    cout<<endl;
  }

  // by Iterator
  SMatrix<int>::Iterator it(y);
  test_iter(it);

  // by row Iterator
  for (uint i = 0; i < y.nrow(); ++i)
  {
    for (SMatrix<int>::Iterator it(y, i); !it.is_end(); ++it)
      cout<<it.index<<" : "<<it.value<<" | ";
    cout<<endl;
  }

}

// [[Rcpp::export]]
List test_normalize(List data_, IntegerVector x)
{
  SMatrix<double> data(data_);
  List sc = data.scales(x);
  data.normalize(sc);
  return List::create(sc, data.value.to_rtype());
}

// [[Rcpp::export]]
int test_transpose(List data_)
{
  SMatrix<double> data(data_);
  SMatrix<double> data_t;
  data.transpose(data_t);
  return 0;
}

/***
library(Matrix)
data <- matrix(c(1, 0, 0, 0,
                 0, 3, 4, 0,
                 0, 0, 5, 6,
                 0, 8, 0, 1),
                 nrow = 4,
                 byrow = T)
data_ <- as(data, "dgCMatrix")
data_ <- Smatrix(data_)

test_transpose(data_)
test_normalize(data_)
*/

