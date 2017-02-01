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

#include "com_intel_mkl_VMLNative.h"

#include "mkl_vml.h"

#include <jni.h>

#include <stdlib.h>
#include <assert.h>

/************************************************************/


/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsInv
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsInv
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsInv(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdInv
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdInv
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdInv(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsSqrt
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsSqrt
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsSqrt(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdSqrt
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdSqrt
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdSqrt(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcSqrt
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcSqrt
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcSqrt(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzSqrt
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzSqrt
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzSqrt(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsInvSqrt
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsInvSqrt
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsInvSqrt(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdInvSqrt
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdInvSqrt
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdInvSqrt(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsCbrt
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsCbrt
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsCbrt(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdCbrt
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdCbrt
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdCbrt(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsInvCbrt
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsInvCbrt
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsInvCbrt(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdInvCbrt
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdInvCbrt
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdInvCbrt(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsExp
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsExp
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsExp(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdExp
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdExp
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdExp(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcExp
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcExp
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcExp(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzExp
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzExp
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzExp(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsLn
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsLn
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsLn(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdLn
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdLn
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdLn(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcLn
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcLn
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcLn(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzLn
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzLn
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzLn(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsLog10
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsLog10
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsLog10(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdLog10
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdLog10
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdLog10(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcLog10
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcLog10
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcLog10(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzLog10
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzLog10
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzLog10(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsCos
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsCos
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsCos(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdCos
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdCos
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdCos(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcCos
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcCos
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcCos(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzCos
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzCos
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzCos(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsSin
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsSin
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsSin(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdSin
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdSin
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdSin(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcSin
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcSin
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcSin(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzSin
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzSin
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzSin(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsTan
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsTan
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsTan(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdTan
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdTan
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdTan(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcTan
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcTan
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcTan(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzTan
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzTan
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzTan(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsCosh
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsCosh
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsCosh(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdCosh
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdCosh
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdCosh(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcCosh
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcCosh
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcCosh(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzCosh
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzCosh
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzCosh(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsSinh
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsSinh
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsSinh(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdSinh
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdSinh
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdSinh(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcSinh
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcSinh
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcSinh(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzSinh
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzSinh
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzSinh(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsTanh
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsTanh
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsTanh(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdTanh
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdTanh
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdTanh(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcTanh
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcTanh
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcTanh(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzTanh
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzTanh
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzTanh(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsAcos
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsAcos
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsAcos(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdAcos
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdAcos
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdAcos(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcAcos
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcAcos
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcAcos(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzAcos
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzAcos
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzAcos(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsAsin
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsAsin
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsAsin(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdAsin
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdAsin
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdAsin(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcAsin
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcAsin
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcAsin(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzAsin
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzAsin
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzAsin(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsAtan
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsAtan
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsAtan(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdAtan
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdAtan
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdAtan(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcAtan
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcAtan
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcAtan(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzAtan
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzAtan
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzAtan(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsAcosh
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsAcosh
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsAcosh(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdAcosh
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdAcosh
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdAcosh(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcAcosh
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcAcosh
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcAcosh(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzAcosh
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzAcosh
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzAcosh(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsAsinh
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsAsinh
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsAsinh(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdAsinh
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdAsinh
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdAsinh(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcAsinh
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcAsinh
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcAsinh(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzAsinh
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzAsinh
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzAsinh(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsAtanh
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsAtanh
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsAtanh(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdAtanh
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdAtanh
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdAtanh(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcAtanh
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcAtanh
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vcAtanh(n, (MKL_Complex8*)a_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzAtanh
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzAtanh
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vzAtanh(n, (MKL_Complex16*)a_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsErf
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsErf
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsErf(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdErf
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdErf
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdErf(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsErfInv
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsErfInv
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsErfInv(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdErfInv
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdErfInv
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdErfInv(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsHypot
 * Signature: (I[F[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsHypot
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray b, jfloatArray r)
{
    float* a_in;
    float* b_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    b_in = (*env)->GetFloatArrayElements(env,b,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && b_in && r_out);

    vmlClearErrStatus();
    vsHypot(n, a_in, b_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,b,b_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdHypot
 * Signature: (I[D[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdHypot
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray b, jdoubleArray r)
{
    double* a_in;
    double* b_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    b_in = (*env)->GetDoubleArrayElements(env,b,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && b_in && r_out);

    vmlClearErrStatus();
    vdHypot(n, a_in, b_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,b,b_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsErfc
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsErfc
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsErfc(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdErfc
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdErfc
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdErfc(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsAtan2
 * Signature: (I[F[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsAtan2
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray b, jfloatArray r)
{
    float* a_in;
    float* b_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    b_in = (*env)->GetFloatArrayElements(env,b,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && b_in && r_out);

    vmlClearErrStatus();
    vsAtan2(n, a_in, b_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,b,b_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdAtan2
 * Signature: (I[D[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdAtan2
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray b, jdoubleArray r)
{
    double* a_in;
    double* b_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    b_in = (*env)->GetDoubleArrayElements(env,b,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && b_in && r_out);

    vmlClearErrStatus();
    vdAtan2(n, a_in, b_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,b,b_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsDiv
 * Signature: (I[F[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsDiv
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray b, jfloatArray r)
{
    float* a_in;
    float* b_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    b_in = (*env)->GetFloatArrayElements(env,b,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && b_in && r_out);

    vmlClearErrStatus();
    vsDiv(n, a_in, b_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,b,b_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdDiv
 * Signature: (I[D[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdDiv
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray b, jdoubleArray r)
{
    double* a_in;
    double* b_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    b_in = (*env)->GetDoubleArrayElements(env,b,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && b_in && r_out);

    vmlClearErrStatus();
    vdDiv(n, a_in, b_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,b,b_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsPow
 * Signature: (I[F[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsPow
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray b, jfloatArray r)
{
    float* a_in;
    float* b_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    b_in = (*env)->GetFloatArrayElements(env,b,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && b_in && r_out);

    vmlClearErrStatus();
    vsPow(n, a_in, b_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,b,b_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdPow
 * Signature: (I[D[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdPow
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray b, jdoubleArray r)
{
    double* a_in;
    double* b_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    b_in = (*env)->GetDoubleArrayElements(env,b,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && b_in && r_out);

    vmlClearErrStatus();
    vdPow(n, a_in, b_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,b,b_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcPow
 * Signature: (I[F[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcPow
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray b, jfloatArray r)
{
    float* a_in;
    float* b_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    b_in = (*env)->GetFloatArrayElements(env,b,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && b_in && r_out);

    vmlClearErrStatus();
    vcPow(n, (MKL_Complex8*)a_in, (MKL_Complex8*)b_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,b,b_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzPow
 * Signature: (I[D[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzPow
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray b, jdoubleArray r)
{
    double* a_in;
    double* b_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    b_in = (*env)->GetDoubleArrayElements(env,b,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && b_in && r_out);

    vmlClearErrStatus();
    vzPow(n, (MKL_Complex16*)a_in, (MKL_Complex16*)b_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,b,b_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsPowx
 * Signature: (I[FF[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsPowx
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloat b, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsPowx(n, a_in, b, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdPowx
 * Signature: (I[DD[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdPowx
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdouble b, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdPowx(n, a_in, b, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vcPowx
 * Signature: (I[F[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vcPowx
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray b, jfloatArray r)
{
    float* a_in;
    float* b_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    b_in = (*env)->GetFloatArrayElements(env,b,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && b_in && r_out);

    vmlClearErrStatus();
    vcPowx(n, (MKL_Complex8*)a_in, *(MKL_Complex8*)b_in, (MKL_Complex8*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,b,b_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vzPowx
 * Signature: (I[D[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vzPowx
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray b, jdoubleArray r)
{
    double* a_in;
    double* b_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    b_in = (*env)->GetDoubleArrayElements(env,b,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && b_in && r_out);

    vmlClearErrStatus();
    vzPowx(n, (MKL_Complex16*)a_in, *(MKL_Complex16*)b_in, (MKL_Complex16*)r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,b,b_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsSinCos
 * Signature: (I[F[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsSinCos
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r1, jfloatArray r2)
{
    float* a_in;
    float* r1_out;
    float* r2_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r1_out = (*env)->GetFloatArrayElements(env,r1,NULL);
    r2_out = (*env)->GetFloatArrayElements(env,r2,NULL);
    assert(a_in && r1_out && r2_out);

    vmlClearErrStatus();
    vsSinCos(n, a_in, r1_out, r2_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r1,r1_out,0);
    (*env)->ReleaseFloatArrayElements(env,r2,r2_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdSinCos
 * Signature: (I[D[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdSinCos
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r1, jdoubleArray r2)
{
    double* a_in;
    double* r1_out;
    double* r2_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r1_out = (*env)->GetDoubleArrayElements(env,r1,NULL);
    r2_out = (*env)->GetDoubleArrayElements(env,r2,NULL);
    assert(a_in && r1_out && r2_out);

    vmlClearErrStatus();
    vdSinCos(n, a_in, r1_out, r2_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r1,r1_out,0);
    (*env)->ReleaseDoubleArrayElements(env,r2,r2_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsCeil
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsCeil
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsCeil(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdCeil
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdCeil
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdCeil(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsFloor
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsFloor
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsFloor(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdFloor
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdFloor
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdFloor(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsModf
 * Signature: (I[F[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsModf
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r1, jfloatArray r2)
{
    float* a_in;
    float* r1_out;
    float* r2_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r1_out = (*env)->GetFloatArrayElements(env,r1,NULL);
    r2_out = (*env)->GetFloatArrayElements(env,r2,NULL);
    assert(a_in && r1_out && r2_out);

    vmlClearErrStatus();
    vsModf(n, a_in, r1_out, r2_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r1,r1_out,0);
    (*env)->ReleaseFloatArrayElements(env,r2,r2_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdModf
 * Signature: (I[D[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdModf
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r1, jdoubleArray r2)
{
    double* a_in;
    double* r1_out;
    double* r2_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r1_out = (*env)->GetDoubleArrayElements(env,r1,NULL);
    r2_out = (*env)->GetDoubleArrayElements(env,r2,NULL);
    assert(a_in && r1_out && r2_out);

    vmlClearErrStatus();
    vdModf(n, a_in, r1_out, r2_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r1,r1_out,0);
    (*env)->ReleaseDoubleArrayElements(env,r2,r2_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsNearbyInt
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsNearbyInt
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsNearbyInt(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdNearbyInt
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdNearbyInt
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdNearbyInt(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsRint
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsRint
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsRint(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdRint
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdRint
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdRint(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsRound
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsRound
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsRound(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdRound
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdRound
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdRound(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsTrunc
 * Signature: (I[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsTrunc
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray r)
{
    float* a_in;
    float* r_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vsTrunc(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdTrunc
 * Signature: (I[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdTrunc
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray r)
{
    double* a_in;
    double* r_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(a_in && r_out);

    vmlClearErrStatus();
    vdTrunc(n, a_in, r_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsPackI
 * Signature: (I[FI[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsPackI
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jint incra, jfloatArray y)
{
    float* a_in;
    float* y_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    y_out = (*env)->GetFloatArrayElements(env,y,NULL);
    assert(a_in && y_out);

    vmlClearErrStatus();
    vsPackI(n, a_in, incra, y_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,y,y_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdPackI
 * Signature: (I[DI[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdPackI
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jint incra, jdoubleArray y)
{
    double* a_in;
    double* y_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    y_out = (*env)->GetDoubleArrayElements(env,y,NULL);
    assert(a_in && y_out);

    vmlClearErrStatus();
    vdPackI(n, a_in, incra, y_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,y,y_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsPackV
 * Signature: (I[F[I[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsPackV
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jintArray ia, jfloatArray y)
{
    float* a_in;
    jint* ia_in;
    float* y_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    ia_in = (*env)->GetIntArrayElements(env,ia,NULL);
    y_out = (*env)->GetFloatArrayElements(env,y,NULL);
    assert(a_in && ia_in && y_out);

    vmlClearErrStatus();
    vsPackV(n, a_in, (int *)ia_in, y_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseIntArrayElements(env,ia,ia_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,y,y_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdPackV
 * Signature: (I[D[I[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdPackV
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jintArray ia, jdoubleArray y)
{
    double* a_in;
    jint* ia_in;
    double* y_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    ia_in = (*env)->GetIntArrayElements(env,ia,NULL);
    y_out = (*env)->GetDoubleArrayElements(env,y,NULL);
    assert(a_in && ia_in && y_out);

    vmlClearErrStatus();
    vdPackV(n, a_in, (int *)ia_in, y_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseIntArrayElements(env,ia,ia_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,y,y_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsPackM
 * Signature: (I[F[I[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsPackM
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jintArray ma, jfloatArray y)
{
    float* a_in;
    jint* ma_in;
    float* y_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    ma_in = (*env)->GetIntArrayElements(env,ma,NULL);
    y_out = (*env)->GetFloatArrayElements(env,y,NULL);
    assert(a_in && ma_in && y_out);

    vmlClearErrStatus();
    vsPackM(n, a_in, (int *)ma_in, y_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseIntArrayElements(env,ma,ma_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,y,y_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdPackM
 * Signature: (I[D[I[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdPackM
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jintArray ma, jdoubleArray y)
{
    double* a_in;
    jint* ma_in;
    double* y_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    ma_in = (*env)->GetIntArrayElements(env,ma,NULL);
    y_out = (*env)->GetDoubleArrayElements(env,y,NULL);
    assert(a_in && ma_in && y_out);

    vmlClearErrStatus();
    vdPackM(n, a_in, (int *)ma_in, y_out);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseIntArrayElements(env,ma,ma_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,y,y_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsUnpackI
 * Signature: (I[F[FI)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsUnpackI
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray y, jint incry)
{
    float* a_in;
    float* y_out;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    y_out = (*env)->GetFloatArrayElements(env,y,NULL);
    assert(a_in && y_out);

    vmlClearErrStatus();
    vsUnpackI(n, a_in, y_out, incry);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,y,y_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdUnpackI
 * Signature: (I[D[DI)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdUnpackI
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray y, jint incry)
{
    double* a_in;
    double* y_out;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    y_out = (*env)->GetDoubleArrayElements(env,y,NULL);
    assert(a_in && y_out);

    vmlClearErrStatus();
    vdUnpackI(n, a_in, y_out, incry);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,y,y_out,0);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsUnpackV
 * Signature: (I[F[F[I)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsUnpackV
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray y, jintArray iy)
{
    float* a_in;
    float* y_out;
    jint* iy_in;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    y_out = (*env)->GetFloatArrayElements(env,y,NULL);
    iy_in = (*env)->GetIntArrayElements(env,iy,NULL);
    assert(a_in && y_out && iy_in);

    vmlClearErrStatus();
    vsUnpackV(n, a_in, y_out, (int *)iy_in);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,y,y_out,0);
    (*env)->ReleaseIntArrayElements(env,iy,iy_in,JNI_ABORT);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdUnpackV
 * Signature: (I[D[D[I)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdUnpackV
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray y, jintArray iy)
{
    double* a_in;
    double* y_out;
    jint* iy_in;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    y_out = (*env)->GetDoubleArrayElements(env,y,NULL);
    iy_in = (*env)->GetIntArrayElements(env,iy,NULL);
    assert(a_in && y_out && iy_in);

    vmlClearErrStatus();
    vdUnpackV(n, a_in, y_out, (int *)iy_in);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,y,y_out,0);
    (*env)->ReleaseIntArrayElements(env,iy,iy_in,JNI_ABORT);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vsUnpackM
 * Signature: (I[F[F[I)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vsUnpackM
  (JNIEnv *env, jclass clazz, jint n, jfloatArray a, jfloatArray y, jintArray my)
{
    float* a_in;
    float* y_out;
    jint* my_in;
    int errcode;
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    y_out = (*env)->GetFloatArrayElements(env,y,NULL);
    my_in = (*env)->GetIntArrayElements(env,my,NULL);
    assert(a_in && y_out && my_in);

    vmlClearErrStatus();
    vsUnpackM(n, a_in, y_out, (int *)my_in);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,y,y_out,0);
    (*env)->ReleaseIntArrayElements(env,my,my_in,JNI_ABORT);
    return errcode;
}

/*
 * Class:     com_intel_mkl_VMLNative
 * Method:    vdUnpackM
 * Signature: (I[D[D[I)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VMLNative_vdUnpackM
  (JNIEnv *env, jclass clazz, jint n, jdoubleArray a, jdoubleArray y, jintArray my)
{
    double* a_in;
    double* y_out;
    jint* my_in;
    int errcode;
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    y_out = (*env)->GetDoubleArrayElements(env,y,NULL);
    my_in = (*env)->GetIntArrayElements(env,my,NULL);
    assert(a_in && y_out && my_in);

    vmlClearErrStatus();
    vdUnpackM(n, a_in, y_out, (int *)my_in);
    errcode = vmlGetErrStatus();

    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,y,y_out,0);
    (*env)->ReleaseIntArrayElements(env,my,my_in,JNI_ABORT);
    return errcode;
}

