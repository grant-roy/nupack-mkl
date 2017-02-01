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
 * Wrappers to CBLAS functions from Intel MKL. The CBLAS
 * interface is used for accessing to BLAS functions.
 * See MKL Reference Manual, Chapter 2 "BLAS and Sparse BLAS
 * Routines", and appendix D "CBLAS Interface to the BLAS".
 *
 * <p>This is demo wrapper which processes only few CBLAS
 * functions in order to demonstrate:
 * <ul>
 * <li>binding MKL/CBLAS functions with Java
 * <li>encoding of 1- and 2-dimensional arrays
 * <li>encoding of complex numbers
 * </ul>
 *
 * <p>CAUTION: This demo wrapper does not:
 * <ul>
 *   <li>demonstrate using huge arrays (>2 billion elements)
 *   <li>demonstrate processing arrays in native memory
 *   <li>check correctness of function parameters
 *   <li>demonstrate performance optimizations
 * </ul>
 *
 * <p>These wrappers assume using 1-dimensional Java arrays of
 * the type double[] or float[] to process floating-point data.
 * A 2-dimensional data series is stored to 1-dimensional array
 * as column-major or row-major 1-dimensional data series.
 *
 * <p>A complex number is stored as 2-elements array: real part
 * of the number as the 1st element of the array, and imaginary
 * part as the 2nd.
 *
 * <p>A complex data series of the length N is stored as a real
 * series of the length 2*N. The n-th complex element is stored
 * as the pair of 2*n-th and (2*n+1)-th real elements: the even
 * indexed for real part and the odd indexed for imaginary part.
 *
 * <p>For more details, please see the MKL User's Guide.
 */
public final class CBLAS {
    //////////////////////////////////////////////////////////

    /**
     * Instantiation is disabled.
     */
    private CBLAS() {}

    /**
     * Load the stubs to the native MKL functions.
     */
    static {
        System.loadLibrary("mkl_java_stubs");
    }

    //////////////////////////////////////////////////////////

    /** Constants for CBLAS_ORDER enum. */
    public final static class ORDER {
        private ORDER() {}
        /** row-major arrays */
        public final static int RowMajor=101;
        /** column-major arrays */
        public final static int ColMajor=102;
    }

    /** Constants for CBLAS_TRANSPOSE enum. */
    public final static class TRANSPOSE {
        private TRANSPOSE() {}
        /** trans='N' */
        public final static int NoTrans  =111;
        /** trans='T' */
        public final static int Trans    =112;
        /** trans='C' */
        public final static int ConjTrans=113;
    }

    /** Constants for CBLAS_UPLO enum. */
    public final static class UPLO {
        private UPLO() {}
        /** uplo ='U' */
        public final static int Upper=121;
        /** uplo ='L' */
        public final static int Lower=122;
    }

    /** Constants for CBLAS_DIAG enum. */
    public final static class DIAG {
        private DIAG() {}
        /** diag ='N' */
        public final static int NonUnit=131;
        /** diag ='U' */
        public final static int Unit   =132;
    }

    /** Constants for CBLAS_SIDE enum. */
    public final static class SIDE {
        private SIDE() {}
        /** side ='L' */
        public final static int Left =141;
        /** side ='R' */
        public final static int Right=142;
    }

    //////////////////////////////////////////////////////////

    /** Wrapper to cblas_sgemm(). */
    public static native void sgemm(int Order, int TransA, int TransB, int M, int N, int K,
        float alpha, float[] A, int lda, float[] B, int ldb, float beta, float[] C, int ldc);

    /** Wrapper to cblas_dgemm(). */
    public static native void dgemm(int Order, int TransA, int TransB, int M, int N, int K,
        double alpha, double[] A, int lda, double[] B, int ldb, double beta, double[] C, int ldc);

    /** Wrapper to cblas_cgemm(). */
    public static native void cgemm(int Order, int TransA, int TransB, int M, int N, int K,
        float[] alpha, float[] A, int lda, float[] B, int ldb, float[] beta, float[] C, int ldc);

    /** Wrapper to cblas_zgemm(). */
    public static native void zgemm(int Order, int TransA, int TransB, int M, int N, int K,
        double[] alpha, double[] A, int lda, double[] B, int ldb, double[] beta, double[] C, int ldc);

    //////////////////////////////////////////////////////////

    /** Wrapper to cblas_sgemv(). */
    public static native void sgemv(int order, int TransA, int M, int N,
        float alpha, float[] A, int lda, float[] X, int incX, float beta, float[] Y, int incY);

    /** Wrapper to cblas_dgemv(). */
    public static native void dgemv(int order, int TransA,int M, int N,
        double alpha, double[] A, int lda, double[] X, int incX, double beta, double[] Y, int incY);

    /** Wrapper to cblas_cgemv(). */
    public static native void cgemv(int order, int TransA, int M, int N,
        float[] alpha, float[] A, int lda, float[] X, int incX, float[] beta, float[] Y, int incY);

    /** Wrapper to cblas_zgemv(). */
    public static native void zgemv(int order, int TransA, int M, int N,
        double[] alpha, double[] A, int lda, double[] X, int incX, double[] beta, double[] Y, int incY);

    //////////////////////////////////////////////////////////

    /** Wrapper to cblas_sdot(). */
    public static native float sdot(int N, float[] X, int incX, float[] Y, int incY);

    /** Wrapper to cblas_ddot(). */
    public static native double ddot(int N, double[] X, int incX, double[] Y, int incY);

    /** Wrapper to cblas_cdotc_sub(). */
    public static native void cdotc_sub(int N, float[] X, int incX, float[] Y, int incY, float[] dotc);

    /** Wrapper to cblas_zdotc_sub(). */
    public static native void zdotc_sub(int N, double[] X, int incX, double[] Y, int incY, double[] dotc);

    /** Wrapper to cblas_cdotu_sub(). */
    public static native void cdotu_sub(int N, float[] X, int incX, float[] Y, int incY, float[] dotu);

    /** Wrapper to cblas_zdotu_sub(). */
    public static native void zdotu_sub(int N, double[] X, int incX, double[] Y, int incY, double[] dotu);
}
