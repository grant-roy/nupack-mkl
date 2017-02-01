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
 * Wrappers to LAPACK functions from Intel MKL.
 *
 * <p>The LAPACK Interface for C/C++ is used for accessing to
 * the LAPACK functions. See MKL Reference Manual, Chapters 3,4,5.
 *
 * <p>Both Java and C parts of the wrapper LAPACK demonstrate the
 * straightforward approach similar to CBLAS wrapper.
 *
 * <p>To make interface more suitable some modification of original
 * LAPACK subroutines has been performed. This is mainly removing
 * work arrays and replacing chars arguments with integers ones,
 * similar to CBLAS interface to BLAS subroutines. Also "info" is
 * returned as function value. For this purposes one more layer
 * has been created, see files clapack.h and clapack.c.
 *
 * <p>These wrappers assume using 1-dimensional Java arrays of
 * the type double[] or float[] to process floating-point data.
 * A multi-dimensional data series is stored into 1-dimensional
 * array as column-major 1-dimensional data series.
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
 *
 */
public final class LAPACK {

    /** Instantiation is disabled. */
    private LAPACK() {}

    //////////////////////////////////////////////////////////

    /** Constants for LAPACK_JOB enum. */
    public final static class JOB {
        private JOB() {}
        /** job ='N' */
        public final static int JobN=201;
        /** job ='V' */
        public final static int JobV=202;
        /** job ='A' */
        public final static int JobA=203;
        /** job ='S' */
        public final static int JobS=204;
        /** job ='O' */
        public final static int JobO=205;
    }

    //////////////////////////////////////////////////////////

    /**
     * Wrapper for MKL function cgesv().
     */
    public static int cgesv(int n, int nrhs, float[] a, int lda, int[] ipiv, float[] b, int ldb)
    {
		return LAPACKNative.cgesv(n, nrhs, a, lda, ipiv, b, ldb);
    }

    /**
     * Wrapper for MKL function sgesv().
     */
    public static int sgesv(int n, int nrhs, float[] a, int lda, int[] ipiv, float[] b, int ldb)
    {
		return LAPACKNative.sgesv(n, nrhs, a, lda, ipiv, b, ldb);
    }

    /**
     * Wrapper for MKL function dgesv().
     */
    public static int dgesv(int n, int nrhs, double[] a,int lda, int[] ipiv, double[] b,int ldb)
    {
		return LAPACKNative.dgesv(n, nrhs, a, lda, ipiv, b, ldb);
    }

    /**
     * Wrapper for MKL function zgesv().
     */
    public static int zgesv(int n, int nrhs, double[] a,int lda, int[] ipiv, double[] b,int ldb)
    {
		return LAPACKNative.zgesv(n, nrhs, a, lda, ipiv, b, ldb);
    }

    /**
     * Wrapper for MKL function ssyev().
     *
     * <p>Unlike the original LAPACK subroutine argument jobz is integer type.
     * Also original LAPACK arguments work and lwork are skipped.
     */
    public static int ssyev(int jobz, int uplo, int n, float[] a, int lda, float[] w)
    {
		return LAPACKNative.ssyev(jobz, uplo, n, a, lda, w);
    }

    /**
     * Wrapper for MKL function dsyev().
     *
     * <p>Unlike the original LAPACK subroutine argument jobz is integer type.
     * Also original LAPACK arguments work and lwork are skipped.
     */
    public static int dsyev(int jobz, int uplo, int n, double[] a, int lda, double[] w)
    {
		return LAPACKNative.dsyev(jobz, uplo, n, a, lda, w);
    }

    /**
     * Wrapper for MKL function sgeev().
     *
     * <p>Unlike the original LAPACK subroutine argument jobvl and jobvr are integer type.
     * Also original LAPACK arguments work and lwork are skipped.
     */
    public static int sgeev(int jobvl, int jobvr, int n, float[] a, int lda, float[] wr, float[] wi, float[] vl, int ldvl, float[] vr, int ldvr)
    {
		return LAPACKNative.sgeev(jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr, ldvr);
    }

    /**
     * Wrapper for MKL function dgeev().
     *
     * <p>Unlike the original LAPACK subroutine argument jobvl and jobvr are integer type.
     * Also original LAPACK arguments work and lwork are skipped.
     */
    public static int dgeev(int jobvl, int jobvr, int n, double[] a, int lda, double[] wr, double[] wi, double[] vl, int ldvl, double[] vr, int ldvr)
    {
		return LAPACKNative.dgeev(jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr, ldvr);
    }

    /**
     * Wrapper for MKL function cgeev().
     *
     * <p>Unlike the original LAPACK subroutine argument jobvl and jobvr are integer type.
     * Also original LAPACK arguments work and lwork are skipped.
     */
    public static int cgeev(int jobvl, int jobvr, int n, float[] a, int lda, float[] w, float[] vl, int ldvl, float[] vr, int ldvr)
    {
		return LAPACKNative.cgeev(jobvl, jobvr, n, a, lda, w, vl, ldvl, vr, ldvr);
    }

    /**
     * Wrapper for MKL function zgeev().
     *
     * <p>Unlike the original LAPACK subroutine argument jobvl and jobvr are integer type.
     * Also original LAPACK arguments work and lwork are skipped.
     */
    public static int zgeev(int jobvl, int jobvr, int n, double[] a, int lda, double[] w, double[] vl, int ldvl, double[] vr, int ldvr)
    {
		return LAPACKNative.zgeev(jobvl, jobvr, n, a, lda, w, vl, ldvl, vr, ldvr);
    }

    /**
     * Wrapper for MKL function sgesvd().
     *
     * <p>Unlike the original LAPACK subroutine argument jobu and jobvt are integer type.
     * Also original LAPACK arguments work and lwork are skipped.
     * Additional argument sd contains the unconverged
     * superdiagonal elements of an upper bidiagonal matrix B
     * whose diagonal is in s if result (info) > 0 (i.e work(2:min(m,n)))
     */
    public static int sgesvd(int jobu, int jobvt, int m, int n, float[] a, int lda, float[] s, float[] u, int ldu, float[] vt, int ldvt, float[] sd)
    {
		return LAPACKNative.sgesvd(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, sd);
    }

    /**
     * Wrapper for MKL function dgesvd().
     *
     * <p>Unlike the original LAPACK subroutine argument jobu and jobvt are integer type.
     * Also original LAPACK arguments work and lwork are skipped.
     * Additional argument sd contains the unconverged
     * superdiagonal elements of an upper bidiagonal matrix B
     * whose diagonal is in s if result (info) > 0 (i.e work(2:min(m,n)))
     */
    public static int dgesvd(int jobu, int jobvt, int m, int n, double[] a, int lda, double[] s, double[] u, int ldu, double[] vt, int ldvt, double[] sd)
    {
		return LAPACKNative.dgesvd(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, sd);
    }

    /**
     * Wrapper for MKL function cgesvd().
     *
     * <p>Unlike the original LAPACK subroutine argument jobu and jobvt are integer type.
     * Also original LAPACK arguments work, rwork and lwork are skipped.
     * Additional argument sd contains the unconverged
     * superdiagonal elements of an upper bidiagonal matrix B
     * whose diagonal is in s if result (info) > 0 (i.e rwork(1:min(m,n)-1))
     */
    public static int cgesvd(int jobu, int jobvt, int m, int n, float[] a, int lda, float[] s, float[] u, int ldu, float[] vt, int ldvt, float[] sd)
    {
		return LAPACKNative.cgesvd(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, sd);
    }

    /**
     * Wrapper for MKL function zgesvd().
     *
     * <p>Unlike the original LAPACK subroutine argument jobu and jobvt are integer type.
     * Also original LAPACK arguments work, rwork and lwork are skipped.
     * Additional argument sd contains the unconverged
     * superdiagonal elements of an upper bidiagonal matrix B
     * whose diagonal is in s if result (info) > 0 (i.e rwork(1:min(m,n)-1))
     */
    public static int zgesvd(int jobu, int jobvt, int m, int n, double[] a, int lda, double[] s, double[] u, int ldu, double[] vt, int ldvt, double[] sd)
    {
		return LAPACKNative.zgesvd(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, sd);
    }
}
