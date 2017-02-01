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

#include <jni.h>
#include <assert.h>

/*
// Template for the CBLAS functions:
//
//   cblas_sgemm(Order, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc)
//   cblas_dgemm(Order, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc)
//   cblas_cgemm(Order, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc)
//   cblas_zgemm(Order, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc)
//
*/

#include "mkl_cblas.h"

/*
// One of the following macro must be pre-defined:
//
//   DATAKIND_SINGLE
//   DATAKIND_DOUBLE
//
*/

#ifdef DATAKIND_SINGLE
#define NAME_JNI_REAL     Java_com_intel_mkl_CBLAS_sgemm
#define NAME_JNI_COMPLEX  Java_com_intel_mkl_CBLAS_cgemm
#define NAME_MKL_REAL     cblas_sgemm
#define NAME_MKL_COMPLEX  cblas_cgemm
#define DATA_SCALAR       jfloat
#define DATA_VECTOR       jfloatArray
#define DATA_GET          GetFloatArrayElements
#define DATA_RELEASE      ReleaseFloatArrayElements
#endif

#ifdef DATAKIND_DOUBLE
#define NAME_JNI_REAL     Java_com_intel_mkl_CBLAS_dgemm
#define NAME_JNI_COMPLEX  Java_com_intel_mkl_CBLAS_zgemm
#define NAME_MKL_REAL     cblas_dgemm
#define NAME_MKL_COMPLEX  cblas_zgemm
#define DATA_SCALAR       jdouble
#define DATA_VECTOR       jdoubleArray
#define DATA_GET          GetDoubleArrayElements
#define DATA_RELEASE      ReleaseDoubleArrayElements
#endif

/*
 * Class:     com_intel_mkl_CBLAS
 * Method:    sgemm, or dgemm
 */
JNIEXPORT void JNICALL NAME_JNI_REAL(JNIEnv *env, jclass klass,
    jint Order, jint TransA, jint TransB, jint M, jint N, jint K,
    DATA_SCALAR alpha, DATA_VECTOR A, jint lda, DATA_VECTOR B, jint ldb,
    DATA_SCALAR beta,  DATA_VECTOR C, jint ldc)
{
    DATA_SCALAR *aElems, *bElems, *cElems;

    aElems = (*env)->DATA_GET(env,A,NULL);
    bElems = (*env)->DATA_GET(env,B,NULL);
    cElems = (*env)->DATA_GET(env,C,NULL);
    assert(aElems && bElems && cElems);

    NAME_MKL_REAL((CBLAS_ORDER)Order,(CBLAS_TRANSPOSE)TransA,(CBLAS_TRANSPOSE)TransB,
        (int)M,(int)N,(int)K,alpha,aElems,(int)lda,bElems,(int)ldb,beta,cElems,(int)ldc);

    (*env)->DATA_RELEASE(env,C,cElems,0);
    (*env)->DATA_RELEASE(env,B,bElems,JNI_ABORT);
    (*env)->DATA_RELEASE(env,A,aElems,JNI_ABORT);
}

/*
 * Class:     com_intel_mkl_CBLAS
 * Method:    cgemm, or zgemm
 */
JNIEXPORT void JNICALL NAME_JNI_COMPLEX(JNIEnv *env, jclass klass,
    jint Order, jint TransA, jint TransB, jint M, jint N, jint K,
    DATA_VECTOR alpha, DATA_VECTOR A, jint lda, DATA_VECTOR B, jint ldb,
    DATA_VECTOR beta,  DATA_VECTOR C, jint ldc)
{
    DATA_SCALAR *aElems, *bElems, *cElems;
    DATA_SCALAR *alphaElems, *betaElems;

    alphaElems = (*env)->DATA_GET(env,alpha,NULL);
    betaElems  = (*env)->DATA_GET(env,beta ,NULL);
    assert(alphaElems && betaElems);

    aElems = (*env)->DATA_GET(env,A,NULL);
    bElems = (*env)->DATA_GET(env,B,NULL);
    cElems = (*env)->DATA_GET(env,C,NULL);
    assert(aElems && bElems && cElems);

    NAME_MKL_COMPLEX((CBLAS_ORDER)Order,(CBLAS_TRANSPOSE)TransA,(CBLAS_TRANSPOSE)TransB,
        (int)M,(int)N,(int)K,alphaElems,aElems,(int)lda,bElems,(int)ldb,betaElems,cElems,(int)ldc);

    (*env)->DATA_RELEASE(env,C,cElems,0);
    (*env)->DATA_RELEASE(env,B,bElems,JNI_ABORT);
    (*env)->DATA_RELEASE(env,A,aElems,JNI_ABORT);

    (*env)->DATA_RELEASE(env,beta ,betaElems ,JNI_ABORT);
    (*env)->DATA_RELEASE(env,alpha,alphaElems,JNI_ABORT);
}

#undef NAME_JNI_REAL
#undef NAME_JNI_COMPLEX
#undef NAME_MKL_REAL
#undef NAME_MKL_COMPLEX
#undef DATA_SCALAR
#undef DATA_VECTOR
#undef DATA_GET
#undef DATA_RELEASE
