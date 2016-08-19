#ifndef EVALUAT_H_
#define EVALUAT_H_

#include <Rcpp.h>
#include "../util/Dvector.h"
#include "../util/Dmatrix.h"
#include "Model.h"

using namespace std;

bool compare(double x, double y) { return abs(x) < abs(y); }
double evaluates(Model* fm, DVector<double>& y_hat, DVector<float>& y_true, int type);
double accurancy(DVector<double>& y_hat, DVector<float>& y_true, double cutoff = 0.5);
double auc(DVector<double>& y_hat_, DVector<float>& y_true);
double loglikehood(DVector<double>& y_hat, DVector<float>& y_true);
double rmse(DVector<double>& y_hat, DVector<float>& y_true);
double mae(DVector<double>& y_hat, DVector<float>& y_true);


double evaluates(Model* fm, DVector<double>& y_hat, DVector<float>& y_true, int type)
{
  double res = 0.0;
  if (fm->TASK == REGRESSION) {
    if (type <= RMSE) {
      res = rmse(y_hat, y_true);
    } else {
      res = mae(y_hat, y_true);
    }
  } else if (fm->TASK == CLASSIFICATION) {
    if (type >= ACC) {
      res = accurancy(y_hat, y_true);
    } else if (type == LL) {
      res = loglikehood(y_hat, y_true);
    } else {
      res = auc(y_hat, y_true);
    }
  } else {
    stop("unknown task...");
  }
  return res;
}

double accurancy(DVector<double>& y_hat, DVector<float>& y_true, double cutoff)
{
  uint num_correct = 0;
  uint num_cases = y_true.size();
  for (uint i = 0; i < num_cases; ++i)
  {
    if ( ((y_hat[i] >= cutoff) && (y_true[i] > 0)) || ((y_hat[i] < cutoff) && (y_true[i] < 0)) )
    { num_correct += 1; }
  }
  return (double)num_correct / (double)y_hat.size();
}

double auc(DVector<double>& y_hat_, DVector<float>& y_true)
{
  uint num_cases = y_true.size();
  std::vector<double> tmp(num_cases);
  for (uint i = 0; i < num_cases; ++i)
  {
    tmp[i] = y_true[i] > 0 ? y_hat_[i]:(-y_hat_[i]);
  }

  sort(tmp.begin(), tmp.end(), compare);

  double area = 0, cum_tp = 0;
  for (uint i = 0; i < num_cases; ++i)
  {
    if (tmp[i] > 0) {
      cum_tp += 1.0;
    } else {
      area += cum_tp;
    }
  }
  if (cum_tp == 0 || cum_tp == num_cases) return 1.0;
  area /= cum_tp * (num_cases - cum_tp);
  return area < 0.5 ? 1 - area : area;
}

double loglikehood(DVector<double>& y_hat, DVector<float>& y_true)
{
  uint num_cases = y_true.size();
  double res = 0.0;
  for (uint i = 0; i < num_cases; ++i)
  {
    res += (1 + y_true[i]) * log(y_hat[i] + 1e-20) + (1 - y_true[i]) * log(1 - y_hat[i] - 1e-20);
  }
  return res/2.0;
}

double rmse(DVector<double>& y_hat, DVector<float>& y_true)
{
  uint num_cases = y_true.size();
  double rmse_sum_sqr = 0.0;
  double err;
  for (uint i = 0; i < num_cases; ++i)
  {
    err = y_hat[i] - y_true[i];
    rmse_sum_sqr += err * err;
  }
  return sqrt(rmse_sum_sqr / num_cases);
}

double mae(DVector<double>& y_hat, DVector<float>& y_true)
{
  uint num_cases = y_true.size();
  double err;
  double mae_sum_abs = 0.0;
  for (uint i = 0; i < num_cases; ++i)
  {
    err = y_hat[i] - y_true[i];
    mae_sum_abs  += abs(err);
  }
  return sqrt(mae_sum_abs / num_cases);
}

#endif
