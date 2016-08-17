#include <Rcpp.h>
#include <omp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
double Sum1(NumericVector x, int nthreads)
{
  double res = 0;
  int j;
#pragma omp parallel for num_threads(nthreads)
for (int i = 0; i < x.size(); ++i)
{

#pragma omp critical
{
  res = x[i];
}
}
return res;
}


// [[Rcpp::export]]
double Sum2(NumericVector x, int nthreads)
{
  double res = 0;
#pragma omp parallel for num_threads(nthreads) private(res)
  for (int i = 0; i < x.size(); ++i)
  {
  res = x[i];
  }
  return res;
}



