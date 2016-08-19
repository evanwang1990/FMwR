#include <Rcpp.h>
#include <utility>
#include <vector>

using namespace Rcpp;
using namespace std;
typedef pair<double, float> P;

bool cmp(P x, P y)
{
  return (x.first < y.first);
}

// [[Rcpp::export]]
void pair_test()
{
  vector<P> x(10);
  for (int i = 0; i < 10; ++i)
  {
    x[i] = make_pair(rand(), i);
  }

  sort(x.begin(), x.end(), cmp);

  for (int i = 0; i < 10; ++i)
  {
    cout<<x[i].first<<"\t"<<x[i].second<<endl;
  }
}
