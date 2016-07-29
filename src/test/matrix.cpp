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
  x0[4] = 100;
  for (int* it = x0.begin(); it != x0.end(); ++it)
    cout<<*it<<" ";
  cout<<endl;

  DVectorDouble x2(10);
  x2.init_norm(0, 0.2);
  for (double* it = x2.begin(); it != x2.end(); ++it)
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

  cout<<endl;
  DMatrixDouble x2(4, 4);
  x2.init_norm(10,1);
  for (uint i = 0; i < x2.nrow(); i++)
  {
    for (double* it = x2.row_begin(i); it != x2.row_end(i); it++)
      cout<<*it<<" ";
    cout<<endl;
  }
}

//[[Rcpp::export]]
List test_DMatrix_traspose()
{
  DMatrixDouble m(2,5);
  m.init_norm(0,2);
  NumericMatrix res1 = m.to_rtype();
  m.transpose();
  NumericMatrix res2 = m.to_rtype();
  return List::create(res1, res2);
}

#include <time.h>

// [[Rcpp::export]]
void matrix_benchmark_ptr(int rows, int cols, int times)
{
  clock_t b1 = clock();
  for (int i = 0; i < times; i++)
  {
    NumericMatrix a(rows, cols);
    a.fill(1.2);
    for (NumericMatrix::iterator it = a.begin(); it != a.end(); ++it)
      *it *= 3;
  }
  clock_t e1 = clock();
  for (int i = 0; i < times; i++)
  {
    DMatrix<double> y(rows, cols);
    y.init(1.2);
    for (double* it = y.begin(); it != y.end(); it++)
      *it *= 3;
  }
  clock_t e2 = clock();

  cout<<"time elapsed by NumericMatrix: "<<(double)(e1 - b1)/CLOCKS_PER_SEC<<endl;
  cout<<"time elapsed by DMatrix: "<<(double)(e2 - e1)/CLOCKS_PER_SEC<<endl;
}


// > matrix_benchmark_ptr(1000, 1000, 1000)
// time elapsed by NumericMatrix: 9.65
// time elapsed by DMatrix: 1.39

// [[Rcpp::export]]
void matrix_benchmark_bycol(int rows, int cols, int times)
{
  clock_t b1 = clock();
  for (int i = 0; i < times; i++)
  {
    NumericMatrix a(rows, cols);
    a.fill(1.2);
    for (int i = 0; i < cols; i++)
    {
      for (int j = 0; j < rows; j++)
        a(j, i) *= 3;
    }
  }
  clock_t e1 = clock();
  for (int i = 0; i < times; i++)
  {
    DMatrix<double> y(rows, cols);
    y.init(1.2);
    for (int i = 0; i < cols; i++)
    {
      for (int j = 0; j < rows; j++)
        y(j, i) *= 3;
    }
  }
  clock_t e2 = clock();

  cout<<"time elapsed by NumericMatrix: "<<(double)(e1 - b1)/CLOCKS_PER_SEC<<endl;
  cout<<"time elapsed by DMatrix: "<<(double)(e2 - e1)/CLOCKS_PER_SEC<<endl;
}

// > matrix_benchmark_bycol(1000, 1000, 1000)
//   time elapsed by NumericMatrix: 3.39
//   time elapsed by DMatrix: 11.98


// [[Rcpp::export]]
void matrix_benchmark_byrow(int rows, int cols, int times)
{
  clock_t b1 = clock();
  for (int i = 0; i < times; i++)
  {
    NumericMatrix a(rows, cols);
    a.fill(1.2);
    for (int i = 0; i < rows; i++)
    {
      for (int j = 0; j < cols; j++)
        a(i, j) *= 3;
    }
  }
  clock_t e1 = clock();
  for (int i = 0; i < times; i++)
  {
    DMatrix<double> y(rows, cols);
    y.init(1.2);
    for (int i = 0; i < rows; i++)
    {
      for (int j = 0; j < cols; j++)
        y(i, j) *= 3;
    }
  }
  clock_t e2 = clock();

  cout<<"time elapsed by NumericMatrix: "<<(double)(e1 - b1)/CLOCKS_PER_SEC<<endl;
  cout<<"time elapsed by DMatrix: "<<(double)(e2 - e1)/CLOCKS_PER_SEC<<endl;
}


//> matrix_benchmark_byrow(1000,1000,1000)
//  time elapsed by NumericMatrix: 14.25
//  time elapsed by DMatrix: 1.73
