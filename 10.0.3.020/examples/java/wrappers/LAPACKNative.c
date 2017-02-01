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

#include "com_intel_mkl_LAPACKNative.h"

#include "clapack.h"

#include <jni.h>

#include <stdlib.h>
#include <assert.h>

/************************************************************/

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    cgesv
 * Signature: (II[FI[I[FI)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_cgesv
  (JNIEnv *env, jclass clazz, jint n, jint nrhs, jfloatArray a, jint lda, jintArray ipiv, jfloatArray b, jint ldb)
{
    float* a_inout;
    float* b_inout;
	int* ipiv_out;
	int info;

	a_inout = (*env)->GetFloatArrayElements(env,a,NULL);
    b_inout = (*env)->GetFloatArrayElements(env,b,NULL);
	ipiv_out= (*env)->GetIntArrayElements(env,b,NULL);
    assert(a_inout && b_inout && ipiv_out);

	info = clapack_cgesv(n, nrhs, a_inout, lda, ipiv_out, b_inout, ldb);

    (*env)->ReleaseFloatArrayElements(env,a,a_inout,0);
    (*env)->ReleaseFloatArrayElements(env,b,b_inout,0);
    (*env)->ReleaseIntArrayElements(env,ipiv,ipiv_out,0);
	return info;
}

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    sgesv
 * Signature: (II[FI[I[FI)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_sgesv
  (JNIEnv *env, jclass clazz, jint n, jint nrhs, jfloatArray a, jint lda, jintArray ipiv, jfloatArray b, jint ldb)
{
    float* a_inout;
    float* b_inout;
	int* ipiv_out;
	int info;

	a_inout = (*env)->GetFloatArrayElements(env,a,NULL);
    b_inout = (*env)->GetFloatArrayElements(env,b,NULL);
	ipiv_out= (*env)->GetIntArrayElements(env,b,NULL);
    assert(a_inout && b_inout && ipiv_out);

	info = clapack_sgesv(n, nrhs, a_inout, lda, ipiv_out, b_inout, ldb);

    (*env)->ReleaseFloatArrayElements(env,a,a_inout,0);
    (*env)->ReleaseFloatArrayElements(env,b,b_inout,0);
    (*env)->ReleaseIntArrayElements(env,ipiv,ipiv_out,0);
	return info;
}

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    dgesv
 * Signature: (II[DI[I[DI)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_dgesv
  (JNIEnv *env, jclass clazz, jint n, jint nrhs, jdoubleArray a, jint lda, jintArray ipiv, jdoubleArray b, jint ldb)
{
    double* a_inout;
    double* b_inout;
	int* ipiv_out;
	int info;

	a_inout = (*env)->GetDoubleArrayElements(env,a,NULL);
    b_inout = (*env)->GetDoubleArrayElements(env,b,NULL);
	ipiv_out= (*env)->GetIntArrayElements(env,b,NULL);
    assert(a_inout && b_inout && ipiv_out);

	info = clapack_dgesv(n, nrhs, a_inout, lda, ipiv_out, b_inout, ldb);

    (*env)->ReleaseDoubleArrayElements(env,a,a_inout,0);
    (*env)->ReleaseDoubleArrayElements(env,b,b_inout,0);
    (*env)->ReleaseIntArrayElements(env,ipiv,ipiv_out,0);
	return info;
}

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    zgesv
 * Signature: (II[DI[I[DI)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_zgesv
  (JNIEnv *env, jclass clazz, jint n, jint nrhs, jdoubleArray a, jint lda, jintArray ipiv, jdoubleArray b, jint ldb)
{
    double* a_inout;
    double* b_inout;
	int* ipiv_out;
	int info;

	a_inout = (*env)->GetDoubleArrayElements(env,a,NULL);
    b_inout = (*env)->GetDoubleArrayElements(env,b,NULL);
	ipiv_out= (*env)->GetIntArrayElements(env,b,NULL);
    assert(a_inout && b_inout && ipiv_out);

	info = clapack_zgesv(n, nrhs, a_inout, lda, ipiv_out, b_inout, ldb);

    (*env)->ReleaseDoubleArrayElements(env,a,a_inout,0);
    (*env)->ReleaseDoubleArrayElements(env,b,b_inout,0);
    (*env)->ReleaseIntArrayElements(env,ipiv,ipiv_out,0);
	return info;
}

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    ssyev
 * Signature: (III[FI[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_ssyev
  (JNIEnv *env, jclass clazz, jint jobz, jint uplo, jint n, jfloatArray a, jint lda, jfloatArray w)
{
    float* a_inout;
    float* w_out;
	int info;

	a_inout = (*env)->GetFloatArrayElements(env,a,NULL);
    w_out = (*env)->GetFloatArrayElements(env,w,NULL);
    assert(a_inout && w_out);

	info = clapack_ssyev((CLAPACK_JOB)jobz, (CBLAS_UPLO)uplo, n, a_inout, lda, w_out);

    (*env)->ReleaseFloatArrayElements(env,a,a_inout,0);
    (*env)->ReleaseFloatArrayElements(env,w,w_out,0);
	return info;
}

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    dsyev
 * Signature: (III[DI[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_dsyev
  (JNIEnv *env, jclass clazz, jint jobz, jint uplo, jint n, jdoubleArray a, jint lda, jdoubleArray w)
{
    double* a_inout;
    double* w_out;
	int info;

	a_inout = (*env)->GetDoubleArrayElements(env,a,NULL);
    w_out = (*env)->GetDoubleArrayElements(env,w,NULL);
    assert(a_inout && w_out);

	info = clapack_dsyev((CLAPACK_JOB)jobz, (CBLAS_UPLO)uplo, n, a_inout, lda, w_out);

    (*env)->ReleaseDoubleArrayElements(env,a,a_inout,0);
    (*env)->ReleaseDoubleArrayElements(env,w,w_out,0);
	return info;
}

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    sgeev
 * Signature: (III[FI[F[F[FI[FI)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_sgeev
  (JNIEnv *env, jclass clazz, jint jobvl, jint jobvr, jint n, jfloatArray a, jint lda, jfloatArray wr, jfloatArray wi, jfloatArray vl, jint ldvl, jfloatArray vr, jint ldvr)
{
    float* a_inout;
    float *wr_out, *wi_out;
	float *vl_out, *vr_out;
	int info;

	a_inout = (*env)->GetFloatArrayElements(env,a,NULL);
    wr_out = (*env)->GetFloatArrayElements(env,wr,NULL);
    wi_out = (*env)->GetFloatArrayElements(env,wi,NULL);
    assert(a_inout && wr_out && wi_out);
	if(jobvl == (int)ClapackJobV) {
	    vl_out = (*env)->GetFloatArrayElements(env,vl,NULL);
	    assert(vl_out);
	} else {
		vl_out = NULL;
	}
	if(jobvr == (int)ClapackJobV) {
	    vr_out = (*env)->GetFloatArrayElements(env,vr,NULL);
	    assert(vr_out);
	} else {
		vr_out = NULL;
	}

	info = clapack_sgeev((CLAPACK_JOB)jobvl, (CLAPACK_JOB)jobvr, n, a_inout, lda, wr_out, wi_out, vl_out, ldvl, vr_out, ldvr);

    (*env)->ReleaseFloatArrayElements(env,a,a_inout,0);
    (*env)->ReleaseFloatArrayElements(env,wr,wr_out,0);
    (*env)->ReleaseFloatArrayElements(env,wi,wi_out,0);
	if(jobvl == (int)ClapackJobV) {
        (*env)->ReleaseFloatArrayElements(env,vl,vl_out,0);
	}
	if(jobvr == (int)ClapackJobV) {
        (*env)->ReleaseFloatArrayElements(env,vr,vr_out,0);
	}
	return info;
}

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    dgeev
 * Signature: (III[DI[D[D[DI[DI)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_dgeev
  (JNIEnv *env, jclass clazz, jint jobvl, jint jobvr, jint n, jdoubleArray a, jint lda, jdoubleArray wr, jdoubleArray wi, jdoubleArray vl, jint ldvl, jdoubleArray vr, jint ldvr)
{
    double* a_inout;
    double *wr_out, *wi_out;
	double *vl_out, *vr_out;
	int info;

	a_inout = (*env)->GetDoubleArrayElements(env,a,NULL);
    wr_out = (*env)->GetDoubleArrayElements(env,wr,NULL);
    wi_out = (*env)->GetDoubleArrayElements(env,wi,NULL);
    assert(a_inout && wr_out && wi_out);
	if(jobvl == (int)ClapackJobV) {
	    vl_out = (*env)->GetDoubleArrayElements(env,vl,NULL);
	    assert(vl_out);
	} else {
		vl_out = NULL;
	}
	if(jobvr == (int)ClapackJobV) {
	    vr_out = (*env)->GetDoubleArrayElements(env,vr,NULL);
	    assert(vr_out);
	} else {
		vr_out = NULL;
	}

	info = clapack_dgeev((CLAPACK_JOB)jobvl, (CLAPACK_JOB)jobvr, n, a_inout, lda, wr_out, wi_out, vl_out, ldvl, vr_out, ldvr);

    (*env)->ReleaseDoubleArrayElements(env,a,a_inout,0);
    (*env)->ReleaseDoubleArrayElements(env,wr,wr_out,0);
    (*env)->ReleaseDoubleArrayElements(env,wi,wi_out,0);
	if(jobvl == (int)ClapackJobV) {
        (*env)->ReleaseDoubleArrayElements(env,vl,vl_out,0);
	}
	if(jobvr == (int)ClapackJobV) {
        (*env)->ReleaseDoubleArrayElements(env,vr,vr_out,0);
	}
	return info;
}

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    cgeev
 * Signature: (III[FI[F[FI[FI)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_cgeev
  (JNIEnv *env, jclass clazz, jint jobvl, jint jobvr, jint n, jfloatArray a, jint lda, jfloatArray w, jfloatArray vl, jint ldvl, jfloatArray vr, jint ldvr)
{
    float* a_inout;
    float *w_out;
	float *vl_out, *vr_out;
	int info;

	a_inout = (*env)->GetFloatArrayElements(env,a,NULL);
    w_out = (*env)->GetFloatArrayElements(env,w,NULL);
    assert(a_inout && w_out);
	if(jobvl == (int)ClapackJobV) {
	    vl_out = (*env)->GetFloatArrayElements(env,vl,NULL);
	    assert(vl_out);
	} else {
		vl_out = NULL;
	}
	if(jobvr == (int)ClapackJobV) {
	    vr_out = (*env)->GetFloatArrayElements(env,vr,NULL);
	    assert(vr_out);
	} else {
		vr_out = NULL;
	}

	info = clapack_cgeev((CLAPACK_JOB)jobvl, (CLAPACK_JOB)jobvr, n, a_inout, lda, w_out, vl_out, ldvl, vr_out, ldvr);

    (*env)->ReleaseFloatArrayElements(env,a,a_inout,0);
    (*env)->ReleaseFloatArrayElements(env,w,w_out,0);
	if(jobvl == (int)ClapackJobV) {
        (*env)->ReleaseFloatArrayElements(env,vl,vl_out,0);
	}
	if(jobvr == (int)ClapackJobV) {
        (*env)->ReleaseFloatArrayElements(env,vr,vr_out,0);
	}
	return info;
}

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    zgeev
 * Signature: (III[DI[D[DI[DI)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_zgeev
  (JNIEnv *env, jclass clazz, jint jobvl, jint jobvr, jint n, jdoubleArray a, jint lda, jdoubleArray w, jdoubleArray vl, jint ldvl, jdoubleArray vr, jint ldvr)
{
    double* a_inout;
    double *w_out;
	double *vl_out, *vr_out;
	int info;

	a_inout = (*env)->GetDoubleArrayElements(env,a,NULL);
    w_out = (*env)->GetDoubleArrayElements(env,w,NULL);
    assert(a_inout && w_out);
	if(jobvl == (int)ClapackJobV) {
	    vl_out = (*env)->GetDoubleArrayElements(env,vl,NULL);
	    assert(vl_out);
	} else {
		vl_out = NULL;
	}
	if(jobvr == (int)ClapackJobV) {
	    vr_out = (*env)->GetDoubleArrayElements(env,vr,NULL);
	    assert(vr_out);
	} else {
		vr_out = NULL;
	}

	info = clapack_zgeev((CLAPACK_JOB)jobvl, (CLAPACK_JOB)jobvr, n, a_inout, lda, w_out, vl_out, ldvl, vr_out, ldvr);

    (*env)->ReleaseDoubleArrayElements(env,a,a_inout,0);
    (*env)->ReleaseDoubleArrayElements(env,w,w_out,0);
	if(jobvl == (int)ClapackJobV) {
        (*env)->ReleaseDoubleArrayElements(env,vl,vl_out,0);
	}
	if(jobvr == (int)ClapackJobV) {
        (*env)->ReleaseDoubleArrayElements(env,vr,vr_out,0);
	}
	return info;
}

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    sgesvd
 * Signature: (IIII[FI[F[FI[FI[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_sgesvd
  (JNIEnv *env, jclass clazz, jint jobu, jint jobvt, jint m, jint n, jfloatArray a, jint lda, jfloatArray s, jfloatArray u, jint ldu, jfloatArray vt, jint ldvt, jfloatArray sd)
{
    float* a_inout;
    float *s_out;
	float *u_out, *vt_out;
    float *sd_out;
	int info;

	a_inout = (*env)->GetFloatArrayElements(env,a,NULL);
    s_out = (*env)->GetFloatArrayElements(env,s,NULL);
    sd_out = (*env)->GetFloatArrayElements(env,sd,NULL);
    assert(a_inout && s_out && sd_out);
	if(jobu == (int)ClapackJobA || jobu == (int)ClapackJobS) {
	    u_out = (*env)->GetFloatArrayElements(env,u,NULL);
	    assert(u_out);
	} else {
		u_out = NULL;
	}
	if(jobvt == (int)ClapackJobA || jobvt == (int)ClapackJobS) {
	    vt_out = (*env)->GetFloatArrayElements(env,vt,NULL);
	    assert(vt_out);
	} else {
		vt_out = NULL;
	}

	info = clapack_sgesvd((CLAPACK_JOB)jobu, (CLAPACK_JOB)jobvt, m, n, a_inout, lda, s_out, u_out, ldu, vt_out, ldvt, sd_out);

	if(jobu == (int)ClapackJobO || jobvt == (int)ClapackJobO) {
        (*env)->ReleaseFloatArrayElements(env,a,a_inout,0);
	} else {
        (*env)->ReleaseFloatArrayElements(env,a,a_inout,JNI_ABORT);
	}
    (*env)->ReleaseFloatArrayElements(env,s,s_out,0);
	if(jobu == (int)ClapackJobA || jobu == (int)ClapackJobS) {
        (*env)->ReleaseFloatArrayElements(env,u,u_out,0);
	}
	if(jobvt == (int)ClapackJobA || jobvt == (int)ClapackJobS) {
        (*env)->ReleaseFloatArrayElements(env,vt,vt_out,0);
	}
	if(info > 0) {
        (*env)->ReleaseFloatArrayElements(env,sd,sd_out,0);
	} else {
        (*env)->ReleaseFloatArrayElements(env,sd,sd_out,JNI_ABORT);
	}
	return info;
}

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    dgesvd
 * Signature: (IIII[DI[D[DI[DI[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_dgesvd
  (JNIEnv *env, jclass clazz, jint jobu, jint jobvt, jint m, jint n, jdoubleArray a, jint lda, jdoubleArray s, jdoubleArray u, jint ldu, jdoubleArray vt, jint ldvt, jdoubleArray sd)
{
    double* a_inout;
    double *s_out;
	double *u_out, *vt_out;
    double *sd_out;
	int info;

	a_inout = (*env)->GetDoubleArrayElements(env,a,NULL);
    s_out = (*env)->GetDoubleArrayElements(env,s,NULL);
    sd_out = (*env)->GetDoubleArrayElements(env,sd,NULL);
    assert(a_inout && s_out && sd_out);
	if(jobu == (int)ClapackJobA || jobu == (int)ClapackJobS) {
	    u_out = (*env)->GetDoubleArrayElements(env,u,NULL);
	    assert(u_out);
	} else {
		u_out = NULL;
	}
	if(jobvt == (int)ClapackJobA || jobvt == (int)ClapackJobS) {
	    vt_out = (*env)->GetDoubleArrayElements(env,vt,NULL);
	    assert(vt_out);
	} else {
		vt_out = NULL;
	}

	info = clapack_dgesvd((CLAPACK_JOB)jobu, (CLAPACK_JOB)jobvt, m, n, a_inout, lda, s_out, u_out, ldu, vt_out, ldvt, sd_out);

	if(jobu == (int)ClapackJobO || jobvt == (int)ClapackJobO) {
        (*env)->ReleaseDoubleArrayElements(env,a,a_inout,0);
	} else {
        (*env)->ReleaseDoubleArrayElements(env,a,a_inout,JNI_ABORT);
	}
    (*env)->ReleaseDoubleArrayElements(env,s,s_out,0);
	if(jobu == (int)ClapackJobA || jobu == (int)ClapackJobS) {
        (*env)->ReleaseDoubleArrayElements(env,u,u_out,0);
	}
	if(jobvt == (int)ClapackJobA || jobvt == (int)ClapackJobS) {
        (*env)->ReleaseDoubleArrayElements(env,vt,vt_out,0);
	}
	if(info > 0) {
        (*env)->ReleaseDoubleArrayElements(env,sd,sd_out,0);
	} else {
        (*env)->ReleaseDoubleArrayElements(env,sd,sd_out,JNI_ABORT);
	}
	return info;
}

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    cgesvd
 * Signature: (IIII[FI[F[FI[FI[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_cgesvd
  (JNIEnv *env, jclass clazz, jint jobu, jint jobvt, jint m, jint n, jfloatArray a, jint lda, jfloatArray s, jfloatArray u, jint ldu, jfloatArray vt, jint ldvt, jfloatArray sd)
{
    float* a_inout;
    float *s_out;
	float *u_out, *vt_out;
    float *sd_out;
	int info;

	a_inout = (*env)->GetFloatArrayElements(env,a,NULL);
    s_out = (*env)->GetFloatArrayElements(env,s,NULL);
    sd_out = (*env)->GetFloatArrayElements(env,sd,NULL);
    assert(a_inout && s_out && sd_out);
	if(jobu == (int)ClapackJobA || jobu == (int)ClapackJobS) {
	    u_out = (*env)->GetFloatArrayElements(env,u,NULL);
	    assert(u_out);
	} else {
		u_out = NULL;
	}
	if(jobvt == (int)ClapackJobA || jobvt == (int)ClapackJobS) {
	    vt_out = (*env)->GetFloatArrayElements(env,vt,NULL);
	    assert(vt_out);
	} else {
		vt_out = NULL;
	}

	info = clapack_cgesvd((CLAPACK_JOB)jobu, (CLAPACK_JOB)jobvt, m, n, a_inout, lda, s_out, u_out, ldu, vt_out, ldvt, sd_out);

	if(jobu == (int)ClapackJobO || jobvt == (int)ClapackJobO) {
        (*env)->ReleaseFloatArrayElements(env,a,a_inout,0);
	} else {
        (*env)->ReleaseFloatArrayElements(env,a,a_inout,JNI_ABORT);
	}
    (*env)->ReleaseFloatArrayElements(env,s,s_out,0);
	if(jobu == (int)ClapackJobA || jobu == (int)ClapackJobS) {
        (*env)->ReleaseFloatArrayElements(env,u,u_out,0);
	}
	if(jobvt == (int)ClapackJobA || jobvt == (int)ClapackJobS) {
        (*env)->ReleaseFloatArrayElements(env,vt,vt_out,0);
	}
	if(info > 0) {
        (*env)->ReleaseFloatArrayElements(env,sd,sd_out,0);
	} else {
        (*env)->ReleaseFloatArrayElements(env,sd,sd_out,JNI_ABORT);
	}
	return info;
}

/*
 * Class:     com_intel_mkl_LAPACKNative
 * Method:    zgesvd
 * Signature: (IIII[DI[D[DI[DI[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_LAPACKNative_zgesvd
  (JNIEnv *env, jclass clazz, jint jobu, jint jobvt, jint m, jint n, jdoubleArray a, jint lda, jdoubleArray s, jdoubleArray u, jint ldu, jdoubleArray vt, jint ldvt, jdoubleArray sd)
{
    double* a_inout;
    double *s_out;
	double *u_out, *vt_out;
    double *sd_out;
	int info;

	a_inout = (*env)->GetDoubleArrayElements(env,a,NULL);
    s_out = (*env)->GetDoubleArrayElements(env,s,NULL);
    sd_out = (*env)->GetDoubleArrayElements(env,sd,NULL);
    assert(a_inout && s_out && sd_out);
	if(jobu == (int)ClapackJobA || jobu == (int)ClapackJobS) {
	    u_out = (*env)->GetDoubleArrayElements(env,u,NULL);
	    assert(u_out);
	} else {
		u_out = NULL;
	}
	if(jobvt == (int)ClapackJobA || jobvt == (int)ClapackJobS) {
	    vt_out = (*env)->GetDoubleArrayElements(env,vt,NULL);
	    assert(vt_out);
	} else {
		vt_out = NULL;
	}

	info = clapack_zgesvd((CLAPACK_JOB)jobu, (CLAPACK_JOB)jobvt, m, n, a_inout, lda, s_out, u_out, ldu, vt_out, ldvt, sd_out);

	if(jobu == (int)ClapackJobO || jobvt == (int)ClapackJobO) {
        (*env)->ReleaseDoubleArrayElements(env,a,a_inout,0);
	} else {
        (*env)->ReleaseDoubleArrayElements(env,a,a_inout,JNI_ABORT);
	}
    (*env)->ReleaseDoubleArrayElements(env,s,s_out,0);
	if(jobu == (int)ClapackJobA || jobu == (int)ClapackJobS) {
        (*env)->ReleaseDoubleArrayElements(env,u,u_out,0);
	}
	if(jobvt == (int)ClapackJobA || jobvt == (int)ClapackJobS) {
        (*env)->ReleaseDoubleArrayElements(env,vt,vt_out,0);
	}
	if(info > 0) {
        (*env)->ReleaseDoubleArrayElements(env,sd,sd_out,0);
	} else {
        (*env)->ReleaseDoubleArrayElements(env,sd,sd_out,JNI_ABORT);
	}
	return info;
}
