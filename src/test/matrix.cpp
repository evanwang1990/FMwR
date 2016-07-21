#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// test Dvector
#include "../util/Dvector.h"

// [[Rcpp::export]]
void test_DVector()
{
  DVector<int> x0;
  DVector<int> x1(10);
  x1.init(2);
  x0.assign(x1);
  x0(4) = 100;
  for (int* it = x0.begin(); it != x0.end(); ++it)
    cout<<*it<<" ";
  cout<<endl;
}


// test DMatrix
#include "../util/Dmatrix.h"

// [[Rcpp::export]]
void test_DMatrix()
{
  DMatrix<double> x0;
  DMatrix<double> x1(2,5);
  x1.init(0.01);
  x1(1,2) = 99.8;
  x0.assign(x1);
  for (uint i = 0; i < x0.nrow(); i++)
  {
    for (double* it = x0.row_begin(i); it != x0.row_end(i); it++)
      cout<<*it<<" ";
    cout<<endl;
  }
}

