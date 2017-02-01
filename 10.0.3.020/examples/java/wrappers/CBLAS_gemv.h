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
//   cblas_sgemv(order, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY)
//   cblas_dgemv(order, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY)
//   cblas_cgemv(order, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY)
//   cblas_zgemv(order, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY)
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
#define NAME_JNI_REAL     Java_com_intel_mkl_CBLAS_sgemv
#define NAME_JNI_COMPLEX  Java_com_intel_mkl_CBLAS_cgemv
#define NAME_MKL_REAL     cblas_sgemv
#define NAME_MKL_COMPLEX  cblas_cgemv
#define DATA_SCALAR       jfloat
#define DATA_VECTOR       jfloatArray
#define DATA_GET          GetFloatArrayElements
#define DATA_RELEASE      ReleaseFloatArrayElements
#endif

#ifdef DATAKIND_DOUBLE
#define NAME_JNI_REAL     Java_com_intel_mkl_CBLAS_dgemv
#define NAME_JNI_COMPLEX  Java_com_intel_mkl_CBLAS_zgemv
#define NAME_MKL_REAL     cblas_dgemv
#define NAME_MKL_COMPLEX  cblas_zgemv
#define DATA_SCALAR       jdouble
#define DATA_VECTOR       jdoubleArray
#define DATA_GET          GetDoubleArrayElements
#define DATA_RELEASE      ReleaseDoubleArrayElements
#endif

/*
 * Class:     com_intel_mkl_CBLAS
 * Method:    sgemv, or dgemv
 */
JNIEXPORT void JNICALL NAME_JNI_REAL(JNIEnv *env, jclass klass,
    jint order, jint TransA, jint M, jint N,
    DATA_SCALAR alpha, DATA_VECTOR A, jint lda, DATA_VECTOR X, jint incX,
    DATA_SCALAR beta,  DATA_VECTOR Y, jint incY)
{
    DATA_SCALAR *aElems, *xElems, *yElems;

    aElems = (*env)->DATA_GET(env,A,NULL);
    xElems = (*env)->DATA_GET(env,X,NULL);
    yElems = (*env)->DATA_GET(env,Y,NULL);
    assert(aElems && xElems && yElems);

    NAME_MKL_REAL((CBLAS_ORDER)order,(CBLAS_TRANSPOSE)TransA,(int)M,(int)N,
        alpha,aElems,(int)lda,xElems,(int)incX,beta,yElems,(int)incY);

    (*env)->DATA_RELEASE(env,Y,yElems,0);
    (*env)->DATA_RELEASE(env,X,xElems,JNI_ABORT);
    (*env)->DATA_RELEASE(env,A,aElems,JNI_ABORT);
}

/*
 * Class:     com_intel_mkl_CBLAS
 * Method:    cgemv, or zgemv
 */
JNIEXPORT void JNICALL NAME_JNI_COMPLEX(JNIEnv *env, jclass klass,
    jint order, jint TransA, jint M, jint N,
    DATA_VECTOR alpha, DATA_VECTOR A, jint lda, DATA_VECTOR X, jint incX,
    DATA_VECTOR beta,  DATA_VECTOR Y, jint incY)
{
    DATA_SCALAR *aElems, *xElems, *yElems;
    DATA_SCALAR *alphaElems, *betaElems;

    alphaElems = (*env)->DATA_GET(env,alpha,NULL);
    betaElems  = (*env)->DATA_GET(env,beta ,NULL);
    assert(alphaElems && betaElems);

    aElems = (*env)->DATA_GET(env,A,NULL);
    xElems = (*env)->DATA_GET(env,X,NULL);
    yElems = (*env)->DATA_GET(env,Y,NULL);
    assert(aElems && xElems && yElems);

    NAME_MKL_COMPLEX((CBLAS_ORDER)order,(CBLAS_TRANSPOSE)TransA,(int)M,(int)N,
        alphaElems,aElems,(int)lda,xElems,(int)incX,betaElems,yElems,(int)incY);

    (*env)->DATA_RELEASE(env,Y,yElems,0);
    (*env)->DATA_RELEASE(env,X,xElems,JNI_ABORT);
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
