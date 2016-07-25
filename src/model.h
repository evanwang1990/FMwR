#ifndef FM_MODEL_H_
#define FM_MODEL_H_

#include <Rcpp.h>
#include <omp.h>
#include <assert.h>
#include "util/Dvector.h"
#include "util/Dmatrix.h"
#include "util/Smatrix.h"

using namespace Rcpp;

class fm_model // FM with gbm use SMatrix<int> data, otherwise use SMatrix<float> data
{
private:
  DVector<double> m_sum, m_sum_sqr;

public:
  double w0;
  DVectorDouble w;
  DMatrixDouble v;

public:
  uint num_attribute;
  bool k0, k1;
  uint num_factor;
  double reg0, regw, regv;
  double init_mean, init_stdev;

public:
  fm_model()
    : num_factor(0), init_mean(0.0), init_stdev(0.01), reg0(0.0), regw(0.0), regv(0.0), k0(true), k1(true) //rename keep_w0
  {}

  ~fm_model() {}

  void init()
  {
    w0 = 0.0;
    w.setSize(num_attribute);
    w.init(0.0);
    v.setSize(num_factor, num_attribute);
    v.init_norm(init_mean, init_stdev);
    m_sum.setSize(num_factor);
    m_sum_sqr.setSize(num_factor);
  }

  double predict(SMatrix<float>::Iterator &x)
  {
    return predict(x, m_sum, m_sum_sqr);
  }

  double predict(SMatrix<float>::Iterator x, DVector<double>& sum, DVector<double>& sum_sqr)
  {
    double pred = 0.0;
    if (k0) { pred += w0; }
    sum.init(0.0);
    sum_sqr.init(0.0);
    for ( ; !x.is_end(); ++x)
    {
      double _val = x.value;
      uint _idx = x.index;
      if (_idx >= num_attribute) { stop("Length of x is greater then then number of attributes..."); }
      if (k1) pred += w[_idx] * _val;

      double* it_v = v.begin() + _idx;
      for (uint i = 0; i < num_factor; ++i)
      {
        double _tmp= *it_v * _val;
        sum[i] += _tmp;
        sum_sqr[i] += _tmp * _tmp;
        it_v += num_attribute;
      }
    }

    for (uint i = 0; i < num_factor; ++i) { pred += 0.5 * (sum[i] * sum[i] - sum_sqr[i]); }

    return pred;
  }

  List save_model()
  {
    return List::create(
      _["w0"] = w0,
      _["w"]  = w.to_rtype(),
      _["v"]  = v.to_rtype()
    );
  }

};


#endif
