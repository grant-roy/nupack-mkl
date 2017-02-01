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
 * Native methods for LAPACK class.
 *
 * <p>This class is added to differentiate native names from
 * Java ones in the LAPACK class.
 *
 * @see LAPACK
 */
 class LAPACKNative {

    /** Load native library */
    static { System.loadLibrary( "mkl_java_stubs" ); }

    static native int cgesv(int n, int nrhs, float[] a, int lda, int[] ipiv, float[] b, int ldb);
    static native int sgesv(int n, int nrhs, float[] a, int lda, int[] ipiv, float[] b, int ldb);
    static native int dgesv(int n, int nrhs, double[] a,int lda, int[] ipiv, double[] b,int ldb);
    static native int zgesv(int n, int nrhs, double[] a,int lda, int[] ipiv, double[] b,int ldb);

    static native int ssyev(int jobz, int uplo, int n, float[] a,int lda, float[] w);
    static native int dsyev(int jobz, int uplo, int n, double[] a,int lda, double[] w);

    static native int sgeev(int jobvl, int jobvr, int n, float[] a, int lda, float[] wr, float[] wi, float[] vl, int ldvl, float[] vr, int ldvr);
    static native int dgeev(int jobvl, int jobvr, int n, double[] a, int lda, double[] wr, double[] wi, double[] vl, int ldvl, double[] vr, int ldvr);
    static native int cgeev(int jobvl, int jobvr, int n, float[] a, int lda, float[] w, float[] vl, int ldvl, float[] vr, int ldvr);
    static native int zgeev(int jobvl, int jobvr, int n, double[] a, int lda, double[] w, double[] vl, int ldvl, double[] vr, int ldvr);

    static native int sgesvd(int jobu, int jobvt, int m, int n, float[] a, int lda, float[] s, float[] u, int ldu, float[] vt, int ldvt, float[] sd);
    static native int dgesvd(int jobu, int jobvt, int m, int n, double[] a, int lda, double[] s, double[] u, int ldu, double[] vt, int ldvt, double[] sd);
    static native int cgesvd(int jobu, int jobvt, int m, int n, float[] a, int lda, float[] s, float[] u, int ldu, float[] vt, int ldvt, float[] sd);
    static native int zgesvd(int jobu, int jobvt, int m, int n, double[] a, int lda, double[] s, double[] u, int ldu, double[] vt, int ldvt, double[] sd);
}
