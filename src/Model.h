#ifndef FM_MODEL_H_
#define FM_MODEL_H_

#include <Rcpp.h>
#include <omp.h>
#include <assert.h>
#include <omp.h>
#include "util/Dvector.h"
#include "util/Dmatrix.h"
#include "util/Smatrix.h"
#include "Data.h"

//TODO: 添加SOLVOE
using namespace Rcpp;

class Model
{
private:
  DVector<double> m_sum, m_sum_sqr;

public:
  double w0;
  DVectorDouble w;
  DMatrixDouble v;

public:
  int nthreads; // multi-threads prediction
  uint num_attribute;
  bool k0, k1;
  uint num_factor;
  double reg0, regw, regv;
  double init_mean, init_stdev;

public:
  Model()
    : num_factor(0), init_mean(0.0), init_stdev(0.01), reg0(0.0), regw(0.0), regv(0.0), k0(true), k1(true), nthreads(1) //rename keep_w0
  {}

  ~Model() {}

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

  double predict(SMatrix<float>::Iterator& x)
  {
    return predict(x, m_sum, m_sum_sqr);
  }

  // void predict_batch(Data& _data, DVector<double>& _out)
  // {
  //   SMatrix<float>* pdata = _data.data;
  //   uint _nr = pdata->nrow();
  //   uint _nc = pdata->ncol();
  //
  //   if (_nr != _out.size()) { stop("output vector's length is not equal to number of cases..."); }
  //   if ((w.size() != _nc) || _nc != v.ncol()) { stop("data's features not equal to parametors' number..."); }
  //
  //   DVector<double> sum(num_factor);
  //   DVector<double> sum_sqr(num_factor);
  //   #pragma omp parallel for private(sum) private(sum_sqr) num_threads(nthreads)
  //   for (uint i = 0; i < _nr; ++i)
  //   {
  //     _out[i] = predict_(i, *pdata, sum, sum_sqr);
  //   }
  // }

  void predict_batch(Data& _data, DVector<double>& _out)
  {
    SMatrix<float>* pdata = _data.data;
    uint _nr = pdata->nrow();
    uint _nc = pdata->ncol();

    if (_nr != _out.size()) { stop("length of output vector is not correct..."); }
    if ((w.size() != _nc) || _nc != v.ncol()) { stop("number of input's features is not correct..."); }

    if (k0) { _out.init(w0); }
    else { _out.init(0.0); }

    uint begin, end, f, j;
    double w_sum, v_sum, v_sum_sqr, v_res, tmp_;

    if (k1)
    {
      #pragma omp parallel for num_threads(nthreads) private(j, end, w_sum)
      for (uint i = 0; i < _nr; ++i)
      {
        end = pdata->row_idx[i+1];
        w_sum = 0.0;
        for (j = pdata->row_idx[i]; j < end; ++j)
        {
          w_sum += w[pdata->col_idx[j]] * pdata->value[j];
        }
        _out[i] += w_sum;
      }
    }

    #pragma omp parallel for num_threads(nthreads) private(v_res, v_sum, v_sum_sqr, begin, end, f, j, tmp_)
    for (uint i = 0; i < _nr; ++i)
    {
      end = pdata->row_idx[i+1];
      begin = pdata->row_idx[i];
      v_res = 0.0;
      for (f = 0; f < num_factor; ++f)
      {
        v_sum = 0; v_sum_sqr = 0;
        for (j = begin; j < end; ++j)
        {
          tmp_ = pdata->value[j] * v(f, pdata->col_idx[j]);
          v_sum += tmp_;
          v_sum_sqr += tmp_ * tmp_;
        }
        v_res += (0.5*v_sum*v_sum - 0.5*v_sum_sqr);
      }
      _out[i] += v_res;
    }
  }

  List save_model()
  {
    return List::create(
      _["w0"] = w0,
      _["w"]  = w.to_rtype(),
      _["v"]  = v.to_rtype()
    );
  }

  double predict(SMatrix<float>::Iterator& x, DVector<double>& sum, DVector<double>& sum_sqr)
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

  double predict_(uint i, SMatrix<float>& data, DVector<double>& sum, DVector<double>& sum_sqr)
  {
    SMatrix<float>::Iterator it(data, i);
    return predict(it, sum, sum_sqr);
  }

};


#endif
