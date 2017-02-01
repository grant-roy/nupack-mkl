/*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 2007-2008 Intel Corporation. All Rights Reserved.
!   The source code contained  or  described herein and all documents related to
!   the source code ("Material") are owned by Intel Corporation or its suppliers
!   or licensors.  Title to the  Material remains with  Intel Corporation or its
!   suppliers and licensors. The Material contains trade secrets and proprietary
!   and  confidential  information of  Intel or its suppliers and licensors. The
!   Material  is  protected  by  worldwide  copyright  and trade secret laws and
!   treaty  provisions. No part of the Material may be used, copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way without Intel's prior express written permission.
!   No license  under any  patent, copyright, trade secret or other intellectual
!   property right is granted to or conferred upon you by disclosure or delivery
!   of the Materials,  either expressly, by implication, inducement, estoppel or
!   otherwise.  Any  license  under  such  intellectual property  rights must be
!   express and approved by Intel in writing.
!******************************************************************************/

package com.intel.mkl;

/**
 * Wrappers to VSL functions from Intel MKL.
 *
 * <p>The VSL Interface for C/C++ is used for accessing to
 * the VSL RNG functions. See MKL Reference Manual, Chapter 10
 * "Statistical Functions", Section "Random Number Generators".
 *
 * <p>These wrappers assume using 1-dimensional Java arrays of
 * the type double[] or float[] to process floating-point data.
 * A multi-dimensional data series is stored into 1-dimensional
 * array as column-major or row-major 1-dimensional data series.
 *
 * <p>For more details, please see the MKL User's Guide.
 */
public final class VSL {

    /** Instantiation is disabled. */
    private VSL() {}

    /**
     * Java counterpart for VSLStreamStatePtr type of the VSL
     * Interface for C/C++.
     *
     * <p>For the C/C++ interface, a VSL stream state handle is just
     * a reference to VSL stream state structure. Instance of this
     * Java class holds such native reference in the form of 64-bits
     * integer (8 bytes must be enough to encode the native address).
     *
     * <p>If copying a handle object, the software also copies the
     * stream state using vslCopyStream() function from MKL.
     * So, each VSL stream state created via this interface has the
     * unique handler object which holds the reference to it.
     *
     * <p>This correspondence is used for garbage collecting VSL
     * stream states. When a handler object is being collected, its
     * finalizer method also calls vslDeleteStream() to destroy
     * the corresponding VSL stream state structure.
     *
     * <p>This allows to treat VSL stream states as managed objects;
     * because one do not need to worry about freeing them.
     * However, the VSL wrapper also provides the DeleteStream()
     * method for destroying VSL stream states explicitly.
     */
    /**
     * Wrapper for MKL function vslNewStream().
     */
    public static int vslNewStream(VSLStreamStatePtr stream, int brng, int seed)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        return VSLStreamStatePtr.vslNewStream(stream,brng,seed);
    }

    /**
     * Wrapper for MKL function vslNewStreamEx().
     */
    public static int vslNewStreamEx(VSLStreamStatePtr stream, int brng, int n, int[] params)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(params.length < n) {
            throw new IllegalArgumentException("Too few elements in array params in vslNewStreamEx");
        }
        return VSLStreamStatePtr.vslNewStreamEx(stream,brng,n,params);
    }

    /**
     * Wrapper for MKL function vslDeleteStream().
     */
    public static int vslDeleteStream(VSLStreamStatePtr stream)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        return VSLStreamStatePtr.vslDeleteStream(stream);
    }

    /**
     * Wrapper for MKL function vslCopyStream().
     */
    public static int vslCopyStream(VSLStreamStatePtr newstream, VSLStreamStatePtr srcstream)
    {
        if(newstream == null || srcstream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        return VSLStreamStatePtr.vslCopyStream(newstream,srcstream);
    }

    /**
     * Wrapper for MKL function vslCopyStreamState().
     */
    public static int vslCopyStreamState(VSLStreamStatePtr deststream, VSLStreamStatePtr srcstream)
    {
        if(deststream == null || srcstream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        return VSLStreamStatePtr.vslCopyStreamState(deststream,srcstream);
    }

    /**
     * Wrapper for MKL function vslLeapfrogStream().
     */
    public static int vslLeapfrogStream(VSLStreamStatePtr stream, int k, int nstreams)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        return VSLStreamStatePtr.vslLeapfrogStream(stream,k,nstreams);
    }

    /**
     * Wrapper for MKL function vdRngCauchy().
     */
    public static int vdRngCauchy(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdRngCauchy");
        }
        return VSLStreamStatePtr.vdRngCauchy(method,stream,n,r,a,beta);
    }

    /**
     * Wrapper for MKL function vsRngCauchy().
     */
    public static int vsRngCauchy(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsRngCauchy");
        }
        return VSLStreamStatePtr.vsRngCauchy(method,stream,n,r,a,beta);
    }

    /**
     * Wrapper for MKL function vdRngUniform().
     */
    public static int vdRngUniform(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double b)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdRngUniform");
        }
        return VSLStreamStatePtr.vdRngUniform(method,stream,n,r,a,b);
    }

    /**
     * Wrapper for MKL function vsRngUniform().
     */
    public static int vsRngUniform(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float b)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsRngUniform");
        }
        return VSLStreamStatePtr.vsRngUniform(method,stream,n,r,a,b);
    }

    /**
     * Wrapper for MKL function vdRngGaussian().
     */
    public static int vdRngGaussian(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double sigma)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdRngGaussian");
        }
        return VSLStreamStatePtr.vdRngGaussian(method,stream,n,r,a,sigma);
    }

    /**
     * Wrapper for MKL function vsRngGaussian().
     */
    public static int vsRngGaussian(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float sigma)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsRngGaussian");
        }
        return VSLStreamStatePtr.vsRngGaussian(method,stream,n,r,a,sigma);
    }

    /**
     * Wrapper for MKL function vdRngGaussianMV().
     */
    public static int vdRngGaussianMV(int method, VSLStreamStatePtr stream, int n, double[] r, int dimen, int mstorage, double[] a, double[] t)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        int size_t;
        if(mstorage == VSL.MATRIX_STORAGE_FULL) {
            size_t = dimen * dimen;
        } else if(mstorage == VSL.MATRIX_STORAGE_PACKED) {
            size_t = dimen * (dimen + 1 ) / 2;
        } else if(mstorage == VSL.MATRIX_STORAGE_DIAGONAL) {
            size_t = dimen;
        } else {
            throw new IllegalArgumentException("Wrong matrix storage scheme in vdRngGaussianMV");
        }
        if(r.length < n*dimen) {
            throw new IllegalArgumentException("Too few elements in array r in vdRngGaussianMV");
        }
        if(a.length < dimen) {
            throw new IllegalArgumentException("Too few elements in array a in vdRngGaussianMV");
        }
        if(t.length < size_t) {
            throw new IllegalArgumentException("Too few elements in array t in vdRngGaussianMV");
        }
        return VSLStreamStatePtr.vdRngGaussianMV(method,stream,n,r,dimen,mstorage,a,t);
    }

    /**
     * Wrapper for MKL function vsRngGaussianMV().
     */
    public static int vsRngGaussianMV(int method, VSLStreamStatePtr stream, int n, float[] r, int dimen, int mstorage, float[] a, float[] t)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        int size_t;
        if(mstorage == VSL.MATRIX_STORAGE_FULL) {
            size_t = dimen * dimen;
        } else if(mstorage == VSL.MATRIX_STORAGE_PACKED) {
            size_t = dimen * (dimen + 1 ) / 2;
        } else if(mstorage == VSL.MATRIX_STORAGE_DIAGONAL) {
            size_t = dimen;
        } else {
            throw new IllegalArgumentException("Wrong matrix storage scheme in vsRngGaussianMV");
        }
        if(r.length < n*dimen) {
            throw new IllegalArgumentException("Too few elements in array r in vsRngGaussianMV");
        }
        if(a.length < dimen) {
            throw new IllegalArgumentException("Too few elements in array a in vsRngGaussianMV");
        }
        if(t.length < size_t) {
            throw new IllegalArgumentException("Too few elements in array t in vsRngGaussianMV");
        }
        return VSLStreamStatePtr.vsRngGaussianMV(method,stream,n,r,dimen,mstorage,a,t);
    }

    /**
     * Wrapper for MKL function vdRngExponential().
     */
    public static int vdRngExponential(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdRngExponential");
        }
        return VSLStreamStatePtr.vdRngExponential(method,stream,n,r,a,beta);
    }

    /**
     * Wrapper for MKL function vsRngExponential().
     */
    public static int vsRngExponential(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsRngExponential");
        }
        return VSLStreamStatePtr.vsRngExponential(method,stream,n,r,a,beta);
    }

    /**
     * Wrapper for MKL function vdRngLaplace().
     */
    public static int vdRngLaplace(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdRngLaplace");
        }
        return VSLStreamStatePtr.vdRngLaplace(method,stream,n,r,a,beta);
    }

    /**
     * Wrapper for MKL function vsRngLaplace().
     */
    public static int vsRngLaplace(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsRngLaplace");
        }
        return VSLStreamStatePtr.vsRngLaplace(method,stream,n,r,a,beta);
    }

    /**
     * Wrapper for MKL function vdRngWeibull().
     */
    public static int vdRngWeibull(int method, VSLStreamStatePtr stream, int n, double[] r, double alpha, double a, double beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdRngWeibull");
        }
        return VSLStreamStatePtr.vdRngWeibull(method,stream,n,r,alpha,a,beta);
    }

    /**
     * Wrapper for MKL function vsRngWeibull().
     */
    public static int vsRngWeibull(int method, VSLStreamStatePtr stream, int n, float[] r, float alpha, float a, float beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsRngWeibull");
        }
        return VSLStreamStatePtr.vsRngWeibull(method,stream,n,r,alpha,a,beta);
    }

    /**
     * Wrapper for MKL function vdRngRayleigh().
     */
    public static int vdRngRayleigh(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdRngRayleigh");
        }
        return VSLStreamStatePtr.vdRngRayleigh(method,stream,n,r,a,beta);
    }

    /**
     * Wrapper for MKL function vsRngRayleigh().
     */
    public static int vsRngRayleigh(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsRngRayleigh");
        }
        return VSLStreamStatePtr.vsRngRayleigh(method,stream,n,r,a,beta);
    }

    /**
     * Wrapper for MKL function vdRngLognormal().
     */
    public static int vdRngLognormal(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double sigma, double b, double beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdRngLognormal");
        }
        return VSLStreamStatePtr.vdRngLognormal(method,stream,n,r,a,sigma,b,beta);
    }

    /**
     * Wrapper for MKL function vsRngLognormal().
     */
    public static int vsRngLognormal(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float sigma, float b, float beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsRngLognormal");
        }
        return VSLStreamStatePtr.vsRngLognormal(method,stream,n,r,a,sigma,b,beta);
    }

    /**
     * Wrapper for MKL function vdRngGumbel().
     */
    public static int vdRngGumbel(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdRngGumbel");
        }
        return VSLStreamStatePtr.vdRngGumbel(method,stream,n,r,a,beta);
    }

    /**
     * Wrapper for MKL function vsRngGumbel().
     */
    public static int vsRngGumbel(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsRngGumbel");
        }
        return VSLStreamStatePtr.vsRngGumbel(method,stream,n,r,a,beta);
    }

    /**
     * Wrapper for MKL function vdRngGamma().
     */
    public static int vdRngGamma(int method, VSLStreamStatePtr stream, int n, double[] r, double alpha, double a, double beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdRngGamma");
        }
        return VSLStreamStatePtr.vdRngGamma(method,stream,n,r,alpha,a,beta);
    }

    /**
     * Wrapper for MKL function vsRngGamma().
     */
    public static int vsRngGamma(int method, VSLStreamStatePtr stream, int n, float[] r, float alpha, float a, float beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsRngGamma");
        }
        return VSLStreamStatePtr.vsRngGamma(method,stream,n,r,alpha,a,beta);
    }

    /**
     * Wrapper for MKL function vdRngBeta().
     */
    public static int vdRngBeta(int method, VSLStreamStatePtr stream, int n, double[] r, double p, double q, double a, double beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdRngBeta");
        }
        return VSLStreamStatePtr.vdRngBeta(method,stream,n,r,p,q,a,beta);
    }

    /**
     * Wrapper for MKL function vsRngBeta().
     */
    public static int vsRngBeta(int method, VSLStreamStatePtr stream, int n, float[] r, float p, float q, float a, float beta)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsRngBeta");
        }
        return VSLStreamStatePtr.vsRngBeta(method,stream,n,r,p,q,a,beta);
    }

    /**
     * Wrapper for MKL function viRngBernoulli().
     */
    public static int viRngBernoulli(int method, VSLStreamStatePtr stream, int n, int[] r, double p)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in viRngBernoulli");
        }
        return VSLStreamStatePtr.viRngBernoulli(method,stream,n,r,p);
    }

    /**
     * Wrapper for MKL function viRngUniform().
     */
    public static int viRngUniform(int method, VSLStreamStatePtr stream, int n, int[] r, int a, int b)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in viRngUniform");
        }
        return VSLStreamStatePtr.viRngUniform(method,stream,n,r,a,b);
    }

    /**
     * Wrapper for MKL function viRngUniformBits().
     */
    public static int viRngUniformBits(int method, VSLStreamStatePtr stream, int n, int[] r)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in viRngUniformBits");
        }
        return VSLStreamStatePtr.viRngUniformBits(method,stream,n,r);
    }

    /**
     * Wrapper for MKL function viRngGeometric().
     */
    public static int viRngGeometric(int method, VSLStreamStatePtr stream, int n, int[] r, double p)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in viRngGeometric");
        }
        return VSLStreamStatePtr.viRngGeometric(method,stream,n,r,p);
    }

    /**
     * Wrapper for MKL function viRngBinomial().
     */
    public static int viRngBinomial(int method, VSLStreamStatePtr stream, int n, int[] r, int ntrial, double p)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in viRngBinomial");
        }
        return VSLStreamStatePtr.viRngBinomial(method,stream,n,r,ntrial,p);
    }

    /**
     * Wrapper for MKL function viRngHypergeometric().
     */
    public static int viRngHypergeometric(int method, VSLStreamStatePtr stream, int n, int[] r, int l, int s, int m)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in viRngHypergeometric");
        }
        return VSLStreamStatePtr.viRngHypergeometric(method,stream,n,r,l,s,m);
    }

    /**
     * Wrapper for MKL function viRngNegbinomial().
     */
    public static int viRngNegbinomial(int method, VSLStreamStatePtr stream, int n, int[] r, double a, double p)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in viRngNegbinomial");
        }
        return VSLStreamStatePtr.viRngNegbinomial(method,stream,n,r,a,p);
    }

    /**
     * Wrapper for MKL function viRngPoisson().
     */
    public static int viRngPoisson(int method, VSLStreamStatePtr stream, int n, int[] r, double lambda)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in viRngPoisson");
        }
        return VSLStreamStatePtr.viRngPoisson(method,stream,n,r,lambda);
    }

    /**
     * Wrapper for MKL function viRngPoissonV().
     */
    public static int viRngPoissonV(int method, VSLStreamStatePtr stream, int n, int[] r, double[] lambda)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in viRngPoissonV");
        }
        if(lambda.length < n) {
            throw new IllegalArgumentException("Too few elements in array lambda in viRngPoissonV");
        }
        return VSLStreamStatePtr.viRngPoissonV(method,stream,n,r,lambda);
    }

    /**
     * Wrapper for MKL function vslSkipAheadStream().
     */
    public static int vslSkipAheadStream(VSLStreamStatePtr stream, int nskip)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        return VSLStreamStatePtr.vslSkipAheadStream(stream,nskip);
    }

    /**
     * Wrapper for MKL function vslGetStreamStateBrng().
     */
    public static int vslGetStreamStateBrng(VSLStreamStatePtr stream)
    {
        if(stream == null) {
            throw new IllegalArgumentException("NULL stream");
        }
        return VSLStreamStatePtr.vslGetStreamStateBrng(stream);
    }

    /**
     * Wrapper for MKL function vslGetNumRegBrngs().
     */
    public static int vslGetNumRegBrngs()
    {
        return VSLStreamStatePtr.vslGetNumRegBrngs();
    }

    /** Constant for VSL_STATUS_OK from "mkl_vsl_defines.h" */
    public final static int STATUS_OK = 0;

    /** Constant for VSL_ERROR_OK from "mkl_vsl_defines.h" */
    public final static int ERROR_OK = 0;

    /** Constant for VSL_ERROR_FEATURE_NOT_IMPLEMENTED from "mkl_vsl_defines.h" */
    public final static int ERROR_FEATURE_NOT_IMPLEMENTED = -1;

    /** Constant for VSL_ERROR_UNKNOWN from "mkl_vsl_defines.h" */
    public final static int ERROR_UNKNOWN = -2;

    /** Constant for VSL_ERROR_BADARGS from "mkl_vsl_defines.h" */
    public final static int ERROR_BADARGS = -3;

    /** Constant for VSL_ERROR_MEM_FAILURE from "mkl_vsl_defines.h" */
    public final static int ERROR_MEM_FAILURE = -4;

    /** Constant for VSL_ERROR_NULL_PTR from "mkl_vsl_defines.h" */
    public final static int ERROR_NULL_PTR = -5;

    /** Constant for VSL_ERROR_INVALID_BRNG_INDEX from "mkl_vsl_defines.h" */
    public final static int ERROR_INVALID_BRNG_INDEX = -1000;

    /** Constant for VSL_ERROR_LEAPFROG_UNSUPPORTED from "mkl_vsl_defines.h" */
    public final static int ERROR_LEAPFROG_UNSUPPORTED = -1002;

    /** Constant for VSL_ERROR_SKIPAHEAD_UNSUPPORTED from "mkl_vsl_defines.h" */
    public final static int ERROR_SKIPAHEAD_UNSUPPORTED = -1003;

    /** Constant for VSL_ERROR_BRNGS_INCOMPATIBLE from "mkl_vsl_defines.h" */
    public final static int ERROR_BRNGS_INCOMPATIBLE = -1005;

    /** Constant for VSL_ERROR_BAD_STREAM from "mkl_vsl_defines.h" */
    public final static int ERROR_BAD_STREAM = -1006;

    /** Constant for VSL_ERROR_BRNG_TABLE_FULL from "mkl_vsl_defines.h" */
    public final static int ERROR_BRNG_TABLE_FULL = -1007;

    /** Constant for VSL_ERROR_BAD_STREAM_STATE_SIZE from "mkl_vsl_defines.h" */
    public final static int ERROR_BAD_STREAM_STATE_SIZE = -1008;

    /** Constant for VSL_ERROR_BAD_WORD_SIZE from "mkl_vsl_defines.h" */
    public final static int ERROR_BAD_WORD_SIZE = -1009;

    /** Constant for VSL_ERROR_BAD_NSEEDS from "mkl_vsl_defines.h" */
    public final static int ERROR_BAD_NSEEDS = -1010;

    /** Constant for VSL_ERROR_BAD_NBITS from "mkl_vsl_defines.h" */
    public final static int ERROR_BAD_NBITS = -1011;

    /** Constant for VSL_ERROR_BAD_UPDATE from "mkl_vsl_defines.h" */
    public final static int ERROR_BAD_UPDATE = -1120;

    /** Constant for VSL_ERROR_NO_NUMBERS from "mkl_vsl_defines.h" */
    public final static int ERROR_NO_NUMBERS = -1121;

    /** Constant for VSL_ERROR_INVALID_ABSTRACT_STREAM from "mkl_vsl_defines.h" */
    public final static int ERROR_INVALID_ABSTRACT_STREAM = -1122;

    /** Constant for VSL_ERROR_FILE_CLOSE from "mkl_vsl_defines.h" */
    public final static int ERROR_FILE_CLOSE = -1100;

    /** Constant for VSL_ERROR_FILE_OPEN from "mkl_vsl_defines.h" */
    public final static int ERROR_FILE_OPEN = -1101;

    /** Constant for VSL_ERROR_FILE_WRITE from "mkl_vsl_defines.h" */
    public final static int ERROR_FILE_WRITE = -1102;

    /** Constant for VSL_ERROR_FILE_READ from "mkl_vsl_defines.h" */
    public final static int ERROR_FILE_READ = -1103;

    /** Constant for VSL_ERROR_BAD_FILE_FORMAT from "mkl_vsl_defines.h" */
    public final static int ERROR_BAD_FILE_FORMAT = -1110;

    /** Constant for VSL_ERROR_UNSUPPORTED_FILE_VER from "mkl_vsl_defines.h" */
    public final static int ERROR_UNSUPPORTED_FILE_VER = -1111;

    /** Constant for VSL_MAX_REG_BRNGS from "mkl_vsl_defines.h" */
    public final static int MAX_REG_BRNGS = 512;

    /** Constant for VSL_BRNG_MCG31 from "mkl_vsl_defines.h" */
    public final static int BRNG_MCG31 = 0x100000;

    /** Constant for VSL_BRNG_R250 from "mkl_vsl_defines.h" */
    public final static int BRNG_R250 = 0x200000;

    /** Constant for VSL_BRNG_MRG32K3A from "mkl_vsl_defines.h" */
    public final static int BRNG_MRG32K3A = 0x300000;

    /** Constant for VSL_BRNG_MCG59 from "mkl_vsl_defines.h" */
    public final static int BRNG_MCG59 = 0x400000;

    /** Constant for VSL_BRNG_WH from "mkl_vsl_defines.h" */
    public final static int BRNG_WH = 0x500000;

    /** Constant for VSL_BRNG_SOBOL from "mkl_vsl_defines.h" */
    public final static int BRNG_SOBOL = 0x600000;

    /** Constant for VSL_BRNG_NIEDERR from "mkl_vsl_defines.h" */
    public final static int BRNG_NIEDERR = 0x700000;

    /** Constant for VSL_BRNG_MT19937 from "mkl_vsl_defines.h" */
    public final static int BRNG_MT19937 = 0x800000;

    /** Constant for VSL_BRNG_MT2203 from "mkl_vsl_defines.h" */
    public final static int BRNG_MT2203 = 0x900000;

    /** Constant for VSL_BRNG_IABSTRACT from "mkl_vsl_defines.h" */
    public final static int BRNG_IABSTRACT = 0xa00000;

    /** Constant for VSL_BRNG_DABSTRACT from "mkl_vsl_defines.h" */
    public final static int BRNG_DABSTRACT = 0xb00000;

    /** Constant for VSL_BRNG_SABSTRACT from "mkl_vsl_defines.h" */
    public final static int BRNG_SABSTRACT = 0xc00000;

    /** Constant for VSL_METHOD_ACCURACY_FLAG from "mkl_vsl_defines.h" */
    public final static int METHOD_ACCURACY_FLAG = 0x40000000;

    /** Constant for VSL_METHOD_SUNIFORM_STD from "mkl_vsl_defines.h" */
    public final static int METHOD_SUNIFORM_STD = 0;

    /** Constant for VSL_METHOD_DUNIFORM_STD from "mkl_vsl_defines.h" */
    public final static int METHOD_DUNIFORM_STD = 0;

    /** Constant for VSL_METHOD_IUNIFORM_STD from "mkl_vsl_defines.h" */
    public final static int METHOD_IUNIFORM_STD = 0;

    /** Constant for VSL_METHOD_SUNIFORM_STD_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_SUNIFORM_STD_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_DUNIFORM_STD_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_DUNIFORM_STD_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_IUNIFORMBITS_STD from "mkl_vsl_defines.h" */
    public final static int METHOD_IUNIFORMBITS_STD = 0;

    /** Constant for VSL_METHOD_SGAUSSIAN_BOXMULLER from "mkl_vsl_defines.h" */
    public final static int METHOD_SGAUSSIAN_BOXMULLER = 0;

    /** Constant for VSL_METHOD_SGAUSSIAN_BOXMULLER2 from "mkl_vsl_defines.h" */
    public final static int METHOD_SGAUSSIAN_BOXMULLER2 = 1;

    /** Constant for VSL_METHOD_SGAUSSIAN_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_SGAUSSIAN_ICDF = 2;

    /** Constant for VSL_METHOD_DGAUSSIAN_BOXMULLER from "mkl_vsl_defines.h" */
    public final static int METHOD_DGAUSSIAN_BOXMULLER = 0;

    /** Constant for VSL_METHOD_DGAUSSIAN_BOXMULLER2 from "mkl_vsl_defines.h" */
    public final static int METHOD_DGAUSSIAN_BOXMULLER2 = 1;

    /** Constant for VSL_METHOD_DGAUSSIAN_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_DGAUSSIAN_ICDF = 2;

    /** Constant for VSL_METHOD_SGAUSSIANMV_BOXMULLER from "mkl_vsl_defines.h" */
    public final static int METHOD_SGAUSSIANMV_BOXMULLER = 0;

    /** Constant for VSL_METHOD_SGAUSSIANMV_BOXMULLER2 from "mkl_vsl_defines.h" */
    public final static int METHOD_SGAUSSIANMV_BOXMULLER2 = 1;

    /** Constant for VSL_METHOD_SGAUSSIANMV_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_SGAUSSIANMV_ICDF = 2;

    /** Constant for VSL_METHOD_DGAUSSIANMV_BOXMULLER from "mkl_vsl_defines.h" */
    public final static int METHOD_DGAUSSIANMV_BOXMULLER = 0;

    /** Constant for VSL_METHOD_DGAUSSIANMV_BOXMULLER2 from "mkl_vsl_defines.h" */
    public final static int METHOD_DGAUSSIANMV_BOXMULLER2 = 1;

    /** Constant for VSL_METHOD_DGAUSSIANMV_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_DGAUSSIANMV_ICDF = 2;

    /** Constant for VSL_METHOD_SEXPONENTIAL_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_SEXPONENTIAL_ICDF = 0;

    /** Constant for VSL_METHOD_DEXPONENTIAL_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_DEXPONENTIAL_ICDF = 0;

    /** Constant for VSL_METHOD_SEXPONENTIAL_ICDF_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_SEXPONENTIAL_ICDF_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_DEXPONENTIAL_ICDF_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_DEXPONENTIAL_ICDF_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_SLAPLACE_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_SLAPLACE_ICDF = 0;

    /** Constant for VSL_METHOD_DLAPLACE_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_DLAPLACE_ICDF = 0;

    /** Constant for VSL_METHOD_SWEIBULL_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_SWEIBULL_ICDF = 0;

    /** Constant for VSL_METHOD_DWEIBULL_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_DWEIBULL_ICDF = 0;

    /** Constant for VSL_METHOD_SWEIBULL_ICDF_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_SWEIBULL_ICDF_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_DWEIBULL_ICDF_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_DWEIBULL_ICDF_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_SCAUCHY_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_SCAUCHY_ICDF = 0;

    /** Constant for VSL_METHOD_DCAUCHY_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_DCAUCHY_ICDF = 0;

    /** Constant for VSL_METHOD_SRAYLEIGH_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_SRAYLEIGH_ICDF = 0;

    /** Constant for VSL_METHOD_DRAYLEIGH_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_DRAYLEIGH_ICDF = 0;

    /** Constant for VSL_METHOD_SRAYLEIGH_ICDF_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_SRAYLEIGH_ICDF_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_DRAYLEIGH_ICDF_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_DRAYLEIGH_ICDF_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_SLOGNORMAL_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_SLOGNORMAL_ICDF = 0;

    /** Constant for VSL_METHOD_DLOGNORMAL_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_DLOGNORMAL_ICDF = 0;

    /** Constant for VSL_METHOD_SLOGNORMAL_ICDF_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_SLOGNORMAL_ICDF_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_DLOGNORMAL_ICDF_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_DLOGNORMAL_ICDF_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_SGUMBEL_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_SGUMBEL_ICDF = 0;

    /** Constant for VSL_METHOD_DGUMBEL_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_DGUMBEL_ICDF = 0;

    /** Constant for VSL_METHOD_SGAMMA_GNORM from "mkl_vsl_defines.h" */
    public final static int METHOD_SGAMMA_GNORM = 0;

    /** Constant for VSL_METHOD_DGAMMA_GNORM from "mkl_vsl_defines.h" */
    public final static int METHOD_DGAMMA_GNORM = 0;

    /** Constant for VSL_METHOD_SGAMMA_GNORM_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_SGAMMA_GNORM_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_DGAMMA_GNORM_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_DGAMMA_GNORM_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_SBETA_CJA from "mkl_vsl_defines.h" */
    public final static int METHOD_SBETA_CJA = 0;

    /** Constant for VSL_METHOD_DBETA_CJA from "mkl_vsl_defines.h" */
    public final static int METHOD_DBETA_CJA = 0;

    /** Constant for VSL_METHOD_SBETA_CJA_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_SBETA_CJA_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_DBETA_CJA_ACCURATE from "mkl_vsl_defines.h" */
    public final static int METHOD_DBETA_CJA_ACCURATE = 0x40000000;

    /** Constant for VSL_METHOD_IBERNOULLI_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_IBERNOULLI_ICDF = 0;

    /** Constant for VSL_METHOD_IGEOMETRIC_ICDF from "mkl_vsl_defines.h" */
    public final static int METHOD_IGEOMETRIC_ICDF = 0;

    /** Constant for VSL_METHOD_IBINOMIAL_BTPE from "mkl_vsl_defines.h" */
    public final static int METHOD_IBINOMIAL_BTPE = 0;

    /** Constant for VSL_METHOD_IHYPERGEOMETRIC_H2PE from "mkl_vsl_defines.h" */
    public final static int METHOD_IHYPERGEOMETRIC_H2PE = 0;

    /** Constant for VSL_METHOD_IPOISSON_PTPE from "mkl_vsl_defines.h" */
    public final static int METHOD_IPOISSON_PTPE = 0;

    /** Constant for VSL_METHOD_IPOISSON_POISNORM from "mkl_vsl_defines.h" */
    public final static int METHOD_IPOISSON_POISNORM = 1;

    /** Constant for VSL_METHOD_IPOISSONV_POISNORM from "mkl_vsl_defines.h" */
    public final static int METHOD_IPOISSONV_POISNORM = 0;

    /** Constant for VSL_METHOD_INEGBINOMIAL_NBAR from "mkl_vsl_defines.h" */
    public final static int METHOD_INEGBINOMIAL_NBAR = 0;

    /** Constant for VSL_MATRIX_STORAGE_FULL from "mkl_vsl_defines.h" */
    public final static int MATRIX_STORAGE_FULL = 0;

    /** Constant for VSL_MATRIX_STORAGE_PACKED from "mkl_vsl_defines.h" */
    public final static int MATRIX_STORAGE_PACKED = 1;

    /** Constant for VSL_MATRIX_STORAGE_DIAGONAL from "mkl_vsl_defines.h" */
    public final static int MATRIX_STORAGE_DIAGONAL = 2;

}
