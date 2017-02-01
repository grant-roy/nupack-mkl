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

#include "com_intel_mkl_VSLStreamStatePtr.h"
//#include "com_intel_mkl_VSL.h"

#include "mkl_vsl.h"

#include <jni.h>

#include <stdlib.h>
#include <assert.h>

/************************************************************/

/*
// Converting between pointers and 64-bit integers.
//
// CAUTION: assuming that void* ocupies 8 bytes or less!
*/

union PtrHack {
    jlong l;
    void* p;
};

static jlong jlong4ptr(void* ptr) {
    union PtrHack hack;
    hack.l = (jlong) 0; // Set bits zero if pointer occupies 4 bytes only!
    hack.p = ptr;
    return hack.l;
}

static void* ptr4jlong(jlong l) {
    union PtrHack hack;
    hack.l = l;
    return hack.p;
}

/************************************************************/

/*
// Auxiliary methods for getting and setting the VSL.handle field.
*/

static VSLStreamStatePtr getHandle(JNIEnv *env, jclass clazz,
    jobject desc_handle)
{
    VSLStreamStatePtr handle;
    jfieldID handle_field;
    jlong handle_hack;

    handle_field = (*env)->GetFieldID(env,clazz,"handle","J");
    if (handle_field == NULL)
        (*env)->FatalError(env,"handle_field == NULL");

    handle_hack = (*env)->GetLongField(env,desc_handle,handle_field);
    handle = ptr4jlong(handle_hack);

    return handle;
}

static void setHandle(JNIEnv *env, jclass clazz,
    jobject desc_handle, VSLStreamStatePtr handle)
{
    jfieldID handle_field;
    jlong handle_hack;

    handle_field = (*env)->GetFieldID(env,clazz,"handle","J");
    if (handle_field == NULL)
        (*env)->FatalError(env,"handle_field == NULL");

    handle_hack = jlong4ptr(handle);
    (*env)->SetLongField(env,desc_handle,handle_field,handle_hack);
}

/************************************************************/


/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vslNewStream
 * Signature: (Lcom/intel/mkl/VSLStreamStatePtr;II)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vslNewStream
  (JNIEnv *env, jclass clazz, jobject stream, jint brng, jint seed)
{
    VSLStreamStatePtr handle;
    int status;

    status = vslNewStream(&handle,brng,seed);
    setHandle(env,clazz,stream,handle);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vslNewStreamEx
 * Signature: (Lcom/intel/mkl/VSLStreamStatePtr;II[I)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vslNewStreamEx
  (JNIEnv *env, jclass clazz, jobject stream, jint brng, jint n, jintArray params)
{
    VSLStreamStatePtr handle;
    int status;
    jint* params_in;
    int i;

    params_in = (*env)->GetIntArrayElements(env,params,NULL);
    assert(params_in);
    status = vslNewStreamEx(&handle,brng,n,(unsigned int *)params_in);
    (*env)->ReleaseIntArrayElements(env,params,params_in,JNI_ABORT);
    setHandle(env,clazz,stream,handle);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vslDeleteStream
 * Signature: (Lcom/intel/mkl/VSLStreamStatePtr;)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vslDeleteStream
  (JNIEnv *env, jclass clazz, jobject stream)
{
    VSLStreamStatePtr handle;
    int status;

    handle = getHandle(env,clazz,stream);
    status = vslDeleteStream(&handle);
    setHandle(env,clazz,stream,handle);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vslCopyStream
 * Signature: (Lcom/intel/mkl/VSLStreamStatePtr;Lcom/intel/mkl/VSLStreamStatePtr;)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vslCopyStream
  (JNIEnv *env, jclass clazz, jobject newstream, jobject srcstream)
{
    VSLStreamStatePtr handle, src_handle;
    int status;

    src_handle = getHandle(env,clazz,srcstream);
    status = vslCopyStream(&handle,src_handle);
    setHandle(env,clazz,newstream,handle);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vslCopyStreamState
 * Signature: (Lcom/intel/mkl/VSLStreamStatePtr;Lcom/intel/mkl/VSLStreamStatePtr;)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vslCopyStreamState
  (JNIEnv *env, jclass clazz, jobject deststream, jobject srcstream)
{
    VSLStreamStatePtr handle, src_handle;
    int status;

    handle = getHandle(env,clazz,deststream);
    src_handle = getHandle(env,clazz,srcstream);
    status = vslCopyStreamState(handle,src_handle);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vslLeapfrogStream
 * Signature: (Lcom/intel/mkl/VSLStreamStatePtr;II)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vslLeapfrogStream
  (JNIEnv *env, jclass clazz, jobject stream, jint k, jint nstreams)
{
    VSLStreamStatePtr handle;
    int status;

    handle = getHandle(env,clazz,stream);
    status = vslLeapfrogStream(handle,k,nstreams);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vdRngCauchy
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[DDD)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vdRngCauchy
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jdoubleArray r, jdouble a, jdouble beta)
{
    VSLStreamStatePtr handle;
    int status;
    double* r_out;

    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vdRngCauchy(method,handle,n,r_out,a,beta);

    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vsRngCauchy
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[FFF)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vsRngCauchy
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jfloatArray r, jfloat a, jfloat beta)
{
    VSLStreamStatePtr handle;
    int status;
    float* r_out;

    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vsRngCauchy(method,handle,n,r_out,a,beta);

    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vdRngUniform
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[DDD)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vdRngUniform
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jdoubleArray r, jdouble a, jdouble b)
{
    VSLStreamStatePtr handle;
    int status;
    double* r_out;

    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vdRngUniform(method,handle,n,r_out,a,b);

    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vsRngUniform
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[FFF)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vsRngUniform
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jfloatArray r, jfloat a, jfloat b)
{
    VSLStreamStatePtr handle;
    int status;
    float* r_out;

    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vsRngUniform(method,handle,n,r_out,a,b);

    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vdRngGaussian
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[DDD)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vdRngGaussian
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jdoubleArray r, jdouble a, jdouble sigma)
{
    VSLStreamStatePtr handle;
    int status;
    double* r_out;

    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vdRngGaussian(method,handle,n,r_out,a,sigma);

    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vsRngGaussian
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[FFF)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vsRngGaussian
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jfloatArray r, jfloat a, jfloat sigma)
{
    VSLStreamStatePtr handle;
    int status;
    float* r_out;

    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vsRngGaussian(method,handle,n,r_out,a,sigma);

    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vdRngGaussianMV
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[DII[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vdRngGaussianMV
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jdoubleArray r, jint dimen, jint mstorage, jdoubleArray a, jdoubleArray t)
{
    VSLStreamStatePtr handle;
    int status;
    double* r_out;
    double* a_in;
    double* t_in;

    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    a_in = (*env)->GetDoubleArrayElements(env,a,NULL);
    t_in = (*env)->GetDoubleArrayElements(env,t,NULL);
    assert(a_in && t_in && r_out);

    handle = getHandle(env,clazz,stream);
    status = vdRngGaussianMV(method,handle,n,r_out,dimen,mstorage,a_in,t_in);

    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);
    (*env)->ReleaseDoubleArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseDoubleArrayElements(env,t,t_in,JNI_ABORT);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vsRngGaussianMV
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[FII[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vsRngGaussianMV
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jfloatArray r, jint dimen, jint mstorage, jfloatArray a, jfloatArray t)
{
    VSLStreamStatePtr handle;
    int status;
    float* r_out;
    float* a_in;
    float* t_in;

    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    a_in = (*env)->GetFloatArrayElements(env,a,NULL);
    t_in = (*env)->GetFloatArrayElements(env,t,NULL);
    assert(a_in && t_in && r_out);

    handle = getHandle(env,clazz,stream);
    status = vsRngGaussianMV(method,handle,n,r_out,dimen,mstorage,a_in,t_in);

    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);
    (*env)->ReleaseFloatArrayElements(env,a,a_in,JNI_ABORT);
    (*env)->ReleaseFloatArrayElements(env,t,t_in,JNI_ABORT);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vdRngExponential
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[DDD)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vdRngExponential
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jdoubleArray r, jdouble a, jdouble beta)
{
    VSLStreamStatePtr handle;
    int status;
    double* r_out;

    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vdRngExponential(method,handle,n,r_out,a,beta);

    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vsRngExponential
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[FFF)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vsRngExponential
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jfloatArray r, jfloat a, jfloat beta)
{
    VSLStreamStatePtr handle;
    int status;
    float* r_out;

    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vsRngExponential(method,handle,n,r_out,a,beta);

    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vdRngLaplace
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[DDD)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vdRngLaplace
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jdoubleArray r, jdouble a, jdouble beta)
{
    VSLStreamStatePtr handle;
    int status;
    double* r_out;

    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vdRngLaplace(method,handle,n,r_out,a,beta);

    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vsRngLaplace
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[FFF)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vsRngLaplace
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jfloatArray r, jfloat a, jfloat beta)
{
    VSLStreamStatePtr handle;
    int status;
    float* r_out;

    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vsRngLaplace(method,handle,n,r_out,a,beta);

    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vdRngWeibull
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[DDDD)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vdRngWeibull
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jdoubleArray r, jdouble alpha, jdouble a, jdouble beta)
{
    VSLStreamStatePtr handle;
    int status;
    double* r_out;

    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vdRngWeibull(method,handle,n,r_out,alpha,a,beta);

    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vsRngWeibull
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[FFFF)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vsRngWeibull
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jfloatArray r, jfloat alpha, jfloat a, jfloat beta)
{
    VSLStreamStatePtr handle;
    int status;
    float* r_out;

    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vsRngWeibull(method,handle,n,r_out,alpha,a,beta);

    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vdRngRayleigh
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[DDD)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vdRngRayleigh
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jdoubleArray r, jdouble a, jdouble beta)
{
    VSLStreamStatePtr handle;
    int status;
    double* r_out;

    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vdRngRayleigh(method,handle,n,r_out,a,beta);

    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vsRngRayleigh
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[FFF)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vsRngRayleigh
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jfloatArray r, jfloat a, jfloat beta)
{
    VSLStreamStatePtr handle;
    int status;
    float* r_out;

    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vsRngRayleigh(method,handle,n,r_out,a,beta);

    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vdRngLognormal
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[DDDDD)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vdRngLognormal
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jdoubleArray r, jdouble a, jdouble sigma, jdouble b, jdouble beta)
{
    VSLStreamStatePtr handle;
    int status;
    double* r_out;

    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vdRngLognormal(method,handle,n,r_out,a,sigma,b,beta);

    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vsRngLognormal
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[FFFFF)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vsRngLognormal
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jfloatArray r, jfloat a, jfloat sigma, jfloat b, jfloat beta)
{
    VSLStreamStatePtr handle;
    int status;
    float* r_out;

    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vsRngLognormal(method,handle,n,r_out,a,sigma,b,beta);

    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vdRngGumbel
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[DDD)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vdRngGumbel
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jdoubleArray r, jdouble a, jdouble beta)
{
    VSLStreamStatePtr handle;
    int status;
    double* r_out;

    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vdRngGumbel(method,handle,n,r_out,a,beta);

    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vsRngGumbel
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[FFF)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vsRngGumbel
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jfloatArray r, jfloat a, jfloat beta)
{
    VSLStreamStatePtr handle;
    int status;
    float* r_out;

    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vsRngGumbel(method,handle,n,r_out,a,beta);

    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vdRngGamma
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[DDDD)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vdRngGamma
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jdoubleArray r, jdouble alpha, jdouble a, jdouble beta)
{
    VSLStreamStatePtr handle;
    int status;
    double* r_out;

    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vdRngGamma(method,handle,n,r_out,alpha,a,beta);

    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vsRngGamma
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[FFFF)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vsRngGamma
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jfloatArray r, jfloat alpha, jfloat a, jfloat beta)
{
    VSLStreamStatePtr handle;
    int status;
    float* r_out;

    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vsRngGamma(method,handle,n,r_out,alpha,a,beta);

    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vdRngBeta
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[DDDDD)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vdRngBeta
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jdoubleArray r, jdouble p, jdouble q, jdouble a, jdouble beta)
{
    VSLStreamStatePtr handle;
    int status;
    double* r_out;

    r_out = (*env)->GetDoubleArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vdRngBeta(method,handle,n,r_out,p,q,a,beta);

    (*env)->ReleaseDoubleArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vsRngBeta
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[FFFFF)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vsRngBeta
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jfloatArray r, jfloat p, jfloat q, jfloat a, jfloat beta)
{
    VSLStreamStatePtr handle;
    int status;
    float* r_out;

    r_out = (*env)->GetFloatArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = vsRngBeta(method,handle,n,r_out,p,q,a,beta);

    (*env)->ReleaseFloatArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    viRngBernoulli
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[ID)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_viRngBernoulli
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jintArray r, jdouble p)
{
    VSLStreamStatePtr handle;
    int status;
    jint* r_out;
    int i;

    r_out = (*env)->GetIntArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = viRngBernoulli(method,handle,n,(int *)r_out,p);

    (*env)->ReleaseIntArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    viRngUniform
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[III)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_viRngUniform
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jintArray r, jint a, jint b)
{
    VSLStreamStatePtr handle;
    int status;
    jint* r_out;
    int i;

    r_out = (*env)->GetIntArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = viRngUniform(method,handle,n,(int *)r_out,a,b);

    (*env)->ReleaseIntArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    viRngUniformBits
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[I)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_viRngUniformBits
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jintArray r)
{
    VSLStreamStatePtr handle;
    int status;
    jint* r_out;
    int i;

    r_out = (*env)->GetIntArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = viRngUniformBits(method,handle,n,(unsigned int *)r_out);

    (*env)->ReleaseIntArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    viRngGeometric
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[ID)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_viRngGeometric
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jintArray r, jdouble p)
{
    VSLStreamStatePtr handle;
    int status;
    jint* r_out;
    int i;

    r_out = (*env)->GetIntArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = viRngGeometric(method,handle,n,(int *)r_out,p);

    (*env)->ReleaseIntArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    viRngBinomial
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[IID)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_viRngBinomial
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jintArray r, jint ntrial, jdouble p)
{
    VSLStreamStatePtr handle;
    int status;
    jint* r_out;
    int i;

    r_out = (*env)->GetIntArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = viRngBinomial(method,handle,n,(int *)r_out,ntrial,p);

    (*env)->ReleaseIntArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    viRngHypergeometric
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[IIII)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_viRngHypergeometric
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jintArray r, jint l, jint s, jint m)
{
    VSLStreamStatePtr handle;
    int status;
    jint* r_out;
    int i;

    r_out = (*env)->GetIntArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = viRngHypergeometric(method,handle,n,(int *)r_out,l,s,m);

    (*env)->ReleaseIntArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    viRngNegbinomial
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[IDD)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_viRngNegbinomial
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jintArray r, jdouble a, jdouble p)
{
    VSLStreamStatePtr handle;
    int status;
    jint* r_out;
    int i;

    r_out = (*env)->GetIntArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = viRngNegbinomial(method,handle,n,(int *)r_out,a,p);

    (*env)->ReleaseIntArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    viRngPoisson
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[ID)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_viRngPoisson
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jintArray r, jdouble lambda)
{
    VSLStreamStatePtr handle;
    int status;
    jint* r_out;
    int i;

    r_out = (*env)->GetIntArrayElements(env,r,NULL);
    assert(r_out);

    handle = getHandle(env,clazz,stream);
    status = viRngPoisson(method,handle,n,(int *)r_out,lambda);

    (*env)->ReleaseIntArrayElements(env,r,r_out,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    viRngPoissonV
 * Signature: (ILcom/intel/mkl/VSLStreamStatePtr;I[I[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_viRngPoissonV
  (JNIEnv *env, jclass clazz, jint method, jobject stream, jint n, jintArray r, jdoubleArray lambda)
{
    VSLStreamStatePtr handle;
    int status;
    jint* r_out;
    int i;
    double* lambda_in;

    r_out = (*env)->GetIntArrayElements(env,r,NULL);
    lambda_in = (*env)->GetDoubleArrayElements(env,lambda,NULL);
    assert(lambda_in && r_out);

    handle = getHandle(env,clazz,stream);
    status = viRngPoissonV(method,handle,n,(int *)r_out,lambda_in);

    (*env)->ReleaseIntArrayElements(env,r,r_out,0);
    (*env)->ReleaseDoubleArrayElements(env,lambda,lambda_in,JNI_ABORT);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vslSkipAheadStream
 * Signature: (Lcom/intel/mkl/VSLStreamStatePtr;I)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vslSkipAheadStream
  (JNIEnv *env, jclass clazz, jobject stream, jint nskip)
{
    VSLStreamStatePtr handle;
    int status;

    handle = getHandle(env,clazz,stream);
    status = vslSkipAheadStream(handle,nskip);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vslGetStreamStateBrng
 * Signature: (Lcom/intel/mkl/VSLStreamStatePtr;)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vslGetStreamStateBrng
  (JNIEnv *env, jclass clazz, jobject stream)
{
    VSLStreamStatePtr handle;
    int brng;

    handle = getHandle(env,clazz,stream);
    brng = vslGetStreamStateBrng(handle);

    return (jint)brng;
}

/*
 * Class:     com_intel_mkl_VSLStreamStatePtr
 * Method:    vslGetNumRegBrngs
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_VSLStreamStatePtr_vslGetNumRegBrngs
  (JNIEnv *env, jclass clazz)
{
    int nregbrngs;

    nregbrngs = vslGetNumRegBrngs();

    return (jint)nregbrngs;
}

