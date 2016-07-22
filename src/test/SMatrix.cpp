#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

#include "../util/Smatrix.h"


// [[Rcpp::export]]
void test_smatrix(List x)
{
  SMatrix<double> y(x);
  cout<<y.dim1<<" "<<y.size<<endl;
}
