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

#include "sample_essl.h"
#include "com_intel_mkl_ESSL.h"

#include <assert.h>
#include <stdlib.h>

/************************************************************/

/*
// Convolution/correlation methods via Fast Fourier Transform
*/

/*
// Java wrapper for the VSL sample for the ESSL function sconf();
// convolution, Fourier method, single precision, no decimation.
// NB: the 'aux' parameters are ignored.
*/
JNIEXPORT void JNICALL Java_com_intel_mkl_ESSL_sconf(
    JNIEnv *env, jclass klass,
    jint init,
    jfloatArray h, jint inc1h,
    jfloatArray x, jint inc1x, jint inc2x,
    jfloatArray y, jint inc1y, jint inc2y,
    jint nh, jint nx, jint m, jint iy0, jint ny,
    jfloatArray aux1, jint naux1, jfloatArray aux2, jint naux2)
{
    jfloat *hElems, *xElems, *yElems;

    hElems = (*env)->GetFloatArrayElements(env,h,NULL);
    xElems = (*env)->GetFloatArrayElements(env,x,NULL);
    yElems = (*env)->GetFloatArrayElements(env,y,NULL);
    assert(hElems && xElems && yElems);

    sconf(
        (int)init,
        hElems, (int)inc1h,
        xElems, (int)inc1x, (int)inc2x,
        yElems, (int)inc1y, (int)inc2y,
        (int)nh, (int)nx, (int)m, (int)iy0, (int)ny,
        NULL,0, NULL,0);

    (*env)->ReleaseFloatArrayElements(env,y,yElems,0);
    (*env)->ReleaseFloatArrayElements(env,x,xElems,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,h,hElems,JNI_ABORT);
}

/*
// Java wrapper for the VSL sample for the ESSL function sconr();
// correlation, Fourier method, single precision, no decimation.
// NB: the 'aux' parameters are ignored.
*/
JNIEXPORT void JNICALL Java_com_intel_mkl_ESSL_scorf(
    JNIEnv *env, jclass klass,
    jint init,
    jfloatArray h, jint inc1h,
    jfloatArray x, jint inc1x, jint inc2x,
    jfloatArray y, jint inc1y, jint inc2y,
    jint nh, jint nx, jint m, jint iy0, jint ny,
    jfloatArray aux1, jint naux1, jfloatArray aux2, jint naux2)
{
    jfloat *hElems, *xElems, *yElems;

    hElems = (*env)->GetFloatArrayElements(env,h,NULL);
    xElems = (*env)->GetFloatArrayElements(env,x,NULL);
    yElems = (*env)->GetFloatArrayElements(env,y,NULL);
    assert(hElems && xElems && yElems);

    scorf(
        (int)init,
        hElems, (int)inc1h,
        xElems, (int)inc1x, (int)inc2x,
        yElems, (int)inc1y, (int)inc2y,
        (int)nh, (int)nx, (int)m, (int)iy0, (int)ny,
        NULL,0, NULL,0);

    (*env)->ReleaseFloatArrayElements(env,y,yElems,0);
    (*env)->ReleaseFloatArrayElements(env,x,xElems,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,h,hElems,JNI_ABORT);
}

/************************************************************/

/*
// Direct convolution/correlation methods, single precision,
// without a support for decimation of the output
*/

/*
// Java wrapper for the VSL sample for the ESSL function scond();
// convolution, direct method, single precision, no decimation.
*/
JNIEXPORT void JNICALL Java_com_intel_mkl_ESSL_scond(
    JNIEnv *env, jclass klass,
    jfloatArray h, jint inch,
    jfloatArray x, jint incx,
    jfloatArray y, jint incy,
    jint nh, jint nx, jint iy0, jint ny)
{
    jfloat *hElems, *xElems, *yElems;

    hElems = (*env)->GetFloatArrayElements(env,h,NULL);
    xElems = (*env)->GetFloatArrayElements(env,x,NULL);
    yElems = (*env)->GetFloatArrayElements(env,y,NULL);
    assert(hElems && xElems && yElems);

    scond(
        hElems, (int)inch,
        xElems, (int)incx,
        yElems, (int)incy,
        (int)nh, (int)nx, (int)iy0, (int)ny);

    (*env)->ReleaseFloatArrayElements(env,y,yElems,0);
    (*env)->ReleaseFloatArrayElements(env,x,xElems,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,h,hElems,JNI_ABORT);
}

/*
// Java wrapper for the VSL sample for the ESSL function scord();
// correlation, direct method, single precision, no decimation.
*/
JNIEXPORT void JNICALL Java_com_intel_mkl_ESSL_scord(
    JNIEnv *env, jclass klass,
    jfloatArray h, jint inch,
    jfloatArray x, jint incx,
    jfloatArray y, jint incy,
    jint nh, jint nx, jint iy0, jint ny)
{
    jfloat *hElems, *xElems, *yElems;

    hElems = (*env)->GetFloatArrayElements(env,h,NULL);
    xElems = (*env)->GetFloatArrayElements(env,x,NULL);
    yElems = (*env)->GetFloatArrayElements(env,y,NULL);
    assert(hElems && xElems && yElems);

    scord(
        hElems, (int)inch,
        xElems, (int)incx,
        yElems, (int)incy,
        (int)nh, (int)nx, (int)iy0, (int)ny);

    (*env)->ReleaseFloatArrayElements(env,y,yElems,0);
    (*env)->ReleaseFloatArrayElements(env,x,xElems,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,h,hElems,JNI_ABORT);
}

/************************************************************/

/*
// Direct convolution/correlation methods, single precision,
// with support for decimation of the output
*/

/*
// Java wrapper for the VSL sample for the ESSL function sdcon();
// convolution, direct method, single precision, with decimation.
*/
JNIEXPORT void JNICALL Java_com_intel_mkl_ESSL_sdcon(
    JNIEnv *env, jclass klass,
    jfloatArray h, jint inch,
    jfloatArray x, jint incx,
    jfloatArray y, jint incy,
    jint nh, jint nx, jint iy0, jint ny, jint id)
{
    jfloat *hElems, *xElems, *yElems;

    hElems = (*env)->GetFloatArrayElements(env,h,NULL);
    xElems = (*env)->GetFloatArrayElements(env,x,NULL);
    yElems = (*env)->GetFloatArrayElements(env,y,NULL);
    assert(hElems && xElems && yElems);

    sdcon(
        hElems, (int)inch,
        xElems, (int)incx,
        yElems, (int)incy,
        (int)nh, (int)nx, (int)iy0, (int)ny, (int)id);

    (*env)->ReleaseFloatArrayElements(env,y,yElems,0);
    (*env)->ReleaseFloatArrayElements(env,x,xElems,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,h,hElems,JNI_ABORT);
}

/*
// Java wrapper for the VSL sample for the ESSL function sdcor();
// correlation, direct method, single precision, with decimation.
*/
JNIEXPORT void JNICALL Java_com_intel_mkl_ESSL_sdcor(
    JNIEnv *env, jclass klass,
    jfloatArray h, jint inch,
    jfloatArray x, jint incx,
    jfloatArray y, jint incy,
    jint nh, jint nx, jint iy0, jint ny, jint id)
{
    jfloat *hElems, *xElems, *yElems;

    hElems = (*env)->GetFloatArrayElements(env,h,NULL);
    xElems = (*env)->GetFloatArrayElements(env,x,NULL);
    yElems = (*env)->GetFloatArrayElements(env,y,NULL);
    assert(hElems && xElems && yElems);

    sdcor(
        hElems, (int)inch,
        xElems, (int)incx,
        yElems, (int)incy,
        (int)nh, (int)nx, (int)iy0, (int)ny, (int)id);

    (*env)->ReleaseFloatArrayElements(env,y,yElems,0);
    (*env)->ReleaseFloatArrayElements(env,x,xElems,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,h,hElems,JNI_ABORT);
}

/************************************************************/

/*
// Direct convolution/correlation methods, double precision,
// with support for decimation of the output
*/

/*
// Java wrapper for the VSL sample for the ESSL function ddcon();
// convolution, direct method, double precision, with decimation.
*/
JNIEXPORT void JNICALL Java_com_intel_mkl_ESSL_ddcon(
    JNIEnv *env, jclass klass,
    jdoubleArray h, jint inch,
    jdoubleArray x, jint incx,
    jdoubleArray y, jint incy,
    jint nh, jint nx, jint iy0, jint ny, jint id)
{
    jdouble *hElems, *xElems, *yElems;

    hElems = (*env)->GetDoubleArrayElements(env,h,NULL);
    xElems = (*env)->GetDoubleArrayElements(env,x,NULL);
    yElems = (*env)->GetDoubleArrayElements(env,y,NULL);
    assert(hElems && xElems && yElems);

    ddcon(
        hElems, (int)inch,
        xElems, (int)incx,
        yElems, (int)incy,
        (int)nh, (int)nx, (int)iy0, (int)ny, (int)id);

    (*env)->ReleaseDoubleArrayElements(env,y,yElems,0);
    (*env)->ReleaseDoubleArrayElements(env,x,xElems,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,h,hElems,JNI_ABORT);
}

/*
// Java wrapper for the VSL sample for the ESSL function ddcor();
// correlation, direct method, double precision, with decimation.
*/
JNIEXPORT void JNICALL Java_com_intel_mkl_ESSL_ddcor(
    JNIEnv *env, jclass klass,
    jdoubleArray h, jint inch,
    jdoubleArray x, jint incx,
    jdoubleArray y, jint incy,
    jint nh, jint nx, jint iy0, jint ny, jint id)
{
    jdouble *hElems, *xElems, *yElems;

    hElems = (*env)->GetDoubleArrayElements(env,h,NULL);
    xElems = (*env)->GetDoubleArrayElements(env,x,NULL);
    yElems = (*env)->GetDoubleArrayElements(env,y,NULL);
    assert(hElems && xElems && yElems);

    ddcor(
        hElems, (int)inch,
        xElems, (int)incx,
        yElems, (int)incy,
        (int)nh, (int)nx, (int)iy0, (int)ny, (int)id);

    (*env)->ReleaseDoubleArrayElements(env,y,yElems,0);
    (*env)->ReleaseDoubleArrayElements(env,x,xElems,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,h,hElems,JNI_ABORT);
}

/************************************************************/

/*
// Direct convolution/correlation methods, single precision
// data but double precision calculations, output decimation
*/

/*
// Java wrapper for the VSL sample for the ESSL function sddcon();
// convolution, direct method, single precision user data but double
// precision calculations, with support for output decimation.
*/
JNIEXPORT void JNICALL Java_com_intel_mkl_ESSL_sddcon(
    JNIEnv *env, jclass klass,
    jfloatArray h, jint inch,
    jfloatArray x, jint incx,
    jfloatArray y, jint incy,
    jint nh, jint nx, jint iy0, jint ny, jint id)
{
    jfloat *hElems, *xElems, *yElems;

    hElems = (*env)->GetFloatArrayElements(env,h,NULL);
    xElems = (*env)->GetFloatArrayElements(env,x,NULL);
    yElems = (*env)->GetFloatArrayElements(env,y,NULL);
    assert(hElems && xElems && yElems);

    sddcon(
        hElems, (int)inch,
        xElems, (int)incx,
        yElems, (int)incy,
        (int)nh, (int)nx, (int)iy0, (int)ny, (int)id);

    (*env)->ReleaseFloatArrayElements(env,y,yElems,0);
    (*env)->ReleaseFloatArrayElements(env,x,xElems,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,h,hElems,JNI_ABORT);
}

/*
// Java wrapper for the VSL sample for the ESSL function sddcor();
// correlation, direct method, single precision user data but double
// precision calculations, with support for output decimation.
*/
JNIEXPORT void JNICALL Java_com_intel_mkl_ESSL_sddcor(
    JNIEnv *env, jclass klass,
    jfloatArray h, jint inch,
    jfloatArray x, jint incx,
    jfloatArray y, jint incy,
    jint nh, jint nx, jint iy0, jint ny, jint id)
{
    jfloat *hElems, *xElems, *yElems;

    hElems = (*env)->GetFloatArrayElements(env,h,NULL);
    xElems = (*env)->GetFloatArrayElements(env,x,NULL);
    yElems = (*env)->GetFloatArrayElements(env,y,NULL);
    assert(hElems && xElems && yElems);

    sddcor(
        hElems, (int)inch,
        xElems, (int)incx,
        yElems, (int)incy,
        (int)nh, (int)nx, (int)iy0, (int)ny, (int)id);

    (*env)->ReleaseFloatArrayElements(env,y,yElems,0);
    (*env)->ReleaseFloatArrayElements(env,x,xElems,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,h,hElems,JNI_ABORT);
}

/************************************************************/
