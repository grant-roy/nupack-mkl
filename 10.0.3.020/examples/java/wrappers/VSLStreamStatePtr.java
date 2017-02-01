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
 * Java counterpart for VSLStreamStatePtr type of the VSL
 * Interface for C/C++.
 *
 * <p>For the C/C++ interface, a VSL stream state is just a reference
 * to VSL stream state descriptor. Instance of this Java class
 * holds such native reference in the form of 64-bits integer
 * (8 bytes must be enough to encode the native address).
 *
 * <p>If copying a handle object, the software also copies the
 * stream using vslCopyStream() function from MKL.
 * So, each VSL stream created via this interface has the
 * unique handler object which holds the reference to it.
 *
 * <p>This correspondence is used for garbage collecting VSL
 * streams. When a handler object is being collected, its
 * finalizer method also calls vslDeleteStream() to destroy
 * the corresponding VSL stream.
 *
 * <p>This allows to treat VSL stream as managed objects;
 * because one do not need to worry about freeing them.
 * However, the VSL wrapper also provides the DeleteStream()
 * method for destroying VSL stream explicitly.
 *
 * @see VSL
 */
public final class VSLStreamStatePtr {

    /** Load native library */
    static { System.loadLibrary( "mkl_java_stubs" ); }

    /** Native address encoded as bytes array. */
    private long handle = 0;

    /** Explicit instantiation creates null handle. */
    public VSLStreamStatePtr() {}

    /** Free the VSL stream descriptor being held, if any. */
    protected void finalize() {
        if (handle != 0) {
            // Get the status, but ignore its value:
            int status = vslDeleteStream(this);
            handle = 0;
        }
    }


    /** Binding for MKL function vslNewStream(). */
    static native int vslNewStream(VSLStreamStatePtr stream, int brng, int seed);

    /** Binding for MKL function vslNewStreamEx(). */
    static native int vslNewStreamEx(VSLStreamStatePtr stream, int brng, int n, int[] params);

    /** Binding for MKL function vslDeleteStream(). */
    static native int vslDeleteStream(VSLStreamStatePtr stream);

    /** Binding for MKL function vslCopyStream(). */
    static native int vslCopyStream(VSLStreamStatePtr newstream, VSLStreamStatePtr srcstream);

    /** Binding for MKL function vslCopyStreamState(). */
    static native int vslCopyStreamState(VSLStreamStatePtr deststream, VSLStreamStatePtr srcstream);

    /** Binding for MKL function vslLeapfrogStream(). */
    static native int vslLeapfrogStream(VSLStreamStatePtr stream, int k, int nstreams);

    /** Binding for MKL function vdRngCauchy(). */
    static native int vdRngCauchy(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double beta);

    /** Binding for MKL function vsRngCauchy(). */
    static native int vsRngCauchy(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float beta);

    /** Binding for MKL function vdRngUniform(). */
    static native int vdRngUniform(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double b);

    /** Binding for MKL function vsRngUniform(). */
    static native int vsRngUniform(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float b);

    /** Binding for MKL function vdRngGaussian(). */
    static native int vdRngGaussian(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double sigma);

    /** Binding for MKL function vsRngGaussian(). */
    static native int vsRngGaussian(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float sigma);

    /** Binding for MKL function vdRngGaussianMV(). */
    static native int vdRngGaussianMV(int method, VSLStreamStatePtr stream, int n, double[] r, int dimen, int mstorage, double[] a, double[] t);

    /** Binding for MKL function vsRngGaussianMV(). */
    static native int vsRngGaussianMV(int method, VSLStreamStatePtr stream, int n, float[] r, int dimen, int mstorage, float[] a, float[] t);

    /** Binding for MKL function vdRngExponential(). */
    static native int vdRngExponential(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double beta);

    /** Binding for MKL function vsRngExponential(). */
    static native int vsRngExponential(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float beta);

    /** Binding for MKL function vdRngLaplace(). */
    static native int vdRngLaplace(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double beta);

    /** Binding for MKL function vsRngLaplace(). */
    static native int vsRngLaplace(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float beta);

    /** Binding for MKL function vdRngWeibull(). */
    static native int vdRngWeibull(int method, VSLStreamStatePtr stream, int n, double[] r, double alpha, double a, double beta);

    /** Binding for MKL function vsRngWeibull(). */
    static native int vsRngWeibull(int method, VSLStreamStatePtr stream, int n, float[] r, float alpha, float a, float beta);

    /** Binding for MKL function vdRngRayleigh(). */
    static native int vdRngRayleigh(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double beta);

    /** Binding for MKL function vsRngRayleigh(). */
    static native int vsRngRayleigh(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float beta);

    /** Binding for MKL function vdRngLognormal(). */
    static native int vdRngLognormal(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double sigma, double b, double beta);

    /** Binding for MKL function vsRngLognormal(). */
    static native int vsRngLognormal(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float sigma, float b, float beta);

    /** Binding for MKL function vdRngGumbel(). */
    static native int vdRngGumbel(int method, VSLStreamStatePtr stream, int n, double[] r, double a, double beta);

    /** Binding for MKL function vsRngGumbel(). */
    static native int vsRngGumbel(int method, VSLStreamStatePtr stream, int n, float[] r, float a, float beta);

    /** Binding for MKL function vdRngGamma(). */
    static native int vdRngGamma(int method, VSLStreamStatePtr stream, int n, double[] r, double alpha, double a, double beta);

    /** Binding for MKL function vsRngGamma(). */
    static native int vsRngGamma(int method, VSLStreamStatePtr stream, int n, float[] r, float alpha, float a, float beta);

    /** Binding for MKL function vdRngBeta(). */
    static native int vdRngBeta(int method, VSLStreamStatePtr stream, int n, double[] r, double p, double q, double a, double beta);

    /** Binding for MKL function vsRngBeta(). */
    static native int vsRngBeta(int method, VSLStreamStatePtr stream, int n, float[] r, float p, float q, float a, float beta);

    /** Binding for MKL function viRngBernoulli(). */
    static native int viRngBernoulli(int method, VSLStreamStatePtr stream, int n, int[] r, double p);

    /** Binding for MKL function viRngUniform(). */
    static native int viRngUniform(int method, VSLStreamStatePtr stream, int n, int[] r, int a, int b);

    /** Binding for MKL function viRngUniformBits(). */
    static native int viRngUniformBits(int method, VSLStreamStatePtr stream, int n, int[] r);

    /** Binding for MKL function viRngGeometric(). */
    static native int viRngGeometric(int method, VSLStreamStatePtr stream, int n, int[] r, double p);

    /** Binding for MKL function viRngBinomial(). */
    static native int viRngBinomial(int method, VSLStreamStatePtr stream, int n, int[] r, int ntrial, double p);

    /** Binding for MKL function viRngHypergeometric(). */
    static native int viRngHypergeometric(int method, VSLStreamStatePtr stream, int n, int[] r, int l, int s, int m);

    /** Binding for MKL function viRngNegbinomial(). */
    static native int viRngNegbinomial(int method, VSLStreamStatePtr stream, int n, int[] r, double a, double p);

    /** Binding for MKL function viRngPoisson(). */
    static native int viRngPoisson(int method, VSLStreamStatePtr stream, int n, int[] r, double lambda);

    /** Binding for MKL function viRngPoissonV(). */
    static native int viRngPoissonV(int method, VSLStreamStatePtr stream, int n, int[] r, double[] lambda);

    /** Binding for MKL function vslSkipAheadStream(). */
    static native int vslSkipAheadStream(VSLStreamStatePtr stream, int nskip);

    /** Binding for MKL function vslGetStreamStateBrng(). */
    static native int vslGetStreamStateBrng(VSLStreamStatePtr stream);

    /** Binding for MKL function vslGetNumRegBrngs(). */
    static native int vslGetNumRegBrngs();
}
