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
//   float  sdot(N, X, incX, Y, incY);
//   double ddot(N, X, incX, Y, incY);
//   void cdotc_sub(N, X, incX, Y, incY, dotc);
//   void zdotc_sub(N, X, incX, Y, incY, dotc);
//   void cdotu_sub(N, X, incX, Y, incY, dotu);
//   void zdotu_sub(N, X, incX, Y, incY, dotu);
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
#define NAME_JNI_REAL       Java_com_intel_mkl_CBLAS_sdot
#define NAME_JNI_COMPLEX_C  Java_com_intel_mkl_CBLAS_cdotc_1sub
#define NAME_JNI_COMPLEX_U  Java_com_intel_mkl_CBLAS_cdotu_1sub
#define NAME_MKL_REAL       cblas_sdot
#define NAME_MKL_COMPLEX_C  cblas_cdotc_sub
#define NAME_MKL_COMPLEX_U  cblas_cdotu_sub
#define DATA_SCALAR         jfloat
#define DATA_VECTOR         jfloatArray
#define DATA_GET            GetFloatArrayElements
#define DATA_RELEASE        ReleaseFloatArrayElements
#endif

#ifdef DATAKIND_DOUBLE
#define NAME_JNI_REAL       Java_com_intel_mkl_CBLAS_ddot
#define NAME_JNI_COMPLEX_C  Java_com_intel_mkl_CBLAS_zdotc_1sub
#define NAME_JNI_COMPLEX_U  Java_com_intel_mkl_CBLAS_zdotu_1sub
#define NAME_MKL_REAL       cblas_ddot
#define NAME_MKL_COMPLEX_C  cblas_zdotc_sub
#define NAME_MKL_COMPLEX_U  cblas_zdotu_sub
#define DATA_SCALAR         jdouble
#define DATA_VECTOR         jdoubleArray
#define DATA_GET            GetDoubleArrayElements
#define DATA_RELEASE        ReleaseDoubleArrayElements
#endif

/*
 * Class:     com_intel_mkl_CBLAS
 * Method:    sdot, or ddot
 */
JNIEXPORT DATA_SCALAR JNICALL NAME_JNI_REAL(JNIEnv *env, jclass klass,
    jint N, DATA_VECTOR X, jint incX, DATA_VECTOR Y, jint incY)
{
    DATA_SCALAR *xElems, *yElems, result;

    xElems = (*env)->DATA_GET(env,X,NULL);
    yElems = (*env)->DATA_GET(env,Y,NULL);
    assert(xElems && yElems);

    result = NAME_MKL_REAL((int)N,xElems,(int)incX,yElems,(int)incY);

    (*env)->DATA_RELEASE(env,Y,yElems,JNI_ABORT);
    (*env)->DATA_RELEASE(env,X,xElems,JNI_ABORT);

    return result;
}

/*
 * Class:     com_intel_mkl_CBLAS
 * Method:    cdotc_sub, or zdotc_sub
 */
JNIEXPORT void JNICALL NAME_JNI_COMPLEX_C(JNIEnv *env, jclass klass,
    jint N, DATA_VECTOR X, jint incX, DATA_VECTOR Y, jint incY, DATA_VECTOR dotc)
{
    DATA_SCALAR *xElems, *yElems, *dotcElems;

    xElems = (*env)->DATA_GET(env,X,NULL);
    yElems = (*env)->DATA_GET(env,Y,NULL);
    dotcElems = (*env)->DATA_GET(env,dotc,NULL);
    assert(xElems && yElems && dotcElems);

    NAME_MKL_COMPLEX_C((int)N,xElems,(int)incX,yElems,(int)incY,dotcElems);

    (*env)->DATA_RELEASE(env,dotc,dotcElems,0);
    (*env)->DATA_RELEASE(env,Y,yElems,JNI_ABORT);
    (*env)->DATA_RELEASE(env,X,xElems,JNI_ABORT);
}

/*
 * Class:     com_intel_mkl_CBLAS
 * Method:    cdotu_sub, or zdotu_sub
 */
JNIEXPORT void JNICALL NAME_JNI_COMPLEX_U(JNIEnv *env, jclass klass,
    jint N, DATA_VECTOR X, jint incX, DATA_VECTOR Y, jint incY, DATA_VECTOR dotu)
{
    DATA_SCALAR *xElems, *yElems, *dotuElems;

    xElems = (*env)->DATA_GET(env,X,NULL);
    yElems = (*env)->DATA_GET(env,Y,NULL);
    dotuElems = (*env)->DATA_GET(env,dotu,NULL);
    assert(xElems && yElems && dotuElems);

    NAME_MKL_COMPLEX_U((int)N,xElems,(int)incX,yElems,(int)incY,dotuElems);

    (*env)->DATA_RELEASE(env,dotu,dotuElems,0);
    (*env)->DATA_RELEASE(env,Y,yElems,JNI_ABORT);
    (*env)->DATA_RELEASE(env,X,xElems,JNI_ABORT);
}

#undef NAME_JNI_REAL
#undef NAME_JNI_COMPLEX_C
#undef NAME_JNI_COMPLEX_U
#undef NAME_MKL_REAL
#undef NAME_MKL_COMPLEX_C
#undef NAME_MKL_COMPLEX_U
#undef DATA_SCALAR
#undef DATA_VECTOR
#undef DATA_GET
#undef DATA_RELEASE
