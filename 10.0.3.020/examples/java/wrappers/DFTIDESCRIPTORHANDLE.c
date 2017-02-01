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

#include "com_intel_mkl_DFTIDESCRIPTORHANDLE.h"
#include "com_intel_mkl_DFTI.h"

#include "mkl_dfti.h"

#include <jni.h>

#include <stdlib.h>

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
// Auxiliary methods for getting and setting the DFTI.handle
// field.
*/

static DFTI_DESCRIPTOR_HANDLE getHandle(JNIEnv *env, jclass clazz,
    jobject desc_handle)
{
    DFTI_DESCRIPTOR_HANDLE handle;
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
    jobject desc_handle, DFTI_DESCRIPTOR_HANDLE handle)
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
// Groups of methods:
// . Descriptor Manipulation
// . DFT Computation
// . Descriptor Configuration
// . Status Checking Functions
*/

/************************************************************/

/*
// Descriptor Manipulation
*/

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiCreateDescriptor
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;III[I)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiCreateDescriptor(
    JNIEnv *env, jclass clazz,
    jobject desc_handle, jint precision, jint forward_domain, jint dimension,
    jintArray length)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    jint *lengthElements;
    long lengthLong[7];
    long status;
    int i;

    lengthElements = (*env)->GetIntArrayElements(env,length,NULL);
    if (lengthElements == NULL)
        (*env)->FatalError(env,"lengthElements == NULL");

    if (dimension > 7)
        (*env)->FatalError(env,"wrong dimension");
    for (i=0; i<dimension; i++)
        lengthLong[i] = lengthElements[i];

    (*env)->ReleaseIntArrayElements(env,length,lengthElements,JNI_ABORT);

    if (dimension == 1)
        status = DftiCreateDescriptor(&handle,
            (enum DFTI_CONFIG_VALUE) precision,
            (enum DFTI_CONFIG_VALUE) forward_domain,
            (long)dimension,lengthLong[0]);
    else
        status = DftiCreateDescriptor(&handle,
            (enum DFTI_CONFIG_VALUE) precision,
            (enum DFTI_CONFIG_VALUE) forward_domain,
            (long)dimension,lengthLong);

    setHandle(env,clazz,desc_handle,handle);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiCommitDescriptor
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiCommitDescriptor(
    JNIEnv *env, jclass clazz, jobject desc_handle)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    long status;

    handle = getHandle(env,clazz,desc_handle);

    status = DftiCommitDescriptor(handle);
    return status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiCopyDescriptor
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiCopyDescriptor(
    JNIEnv *env, jclass clazz, jobject desc_handle_original, jobject desc_handle_copy)
{
    DFTI_DESCRIPTOR_HANDLE handle_original;
    DFTI_DESCRIPTOR_HANDLE handle_copy;
    long status;

    handle_original = getHandle(env,clazz,desc_handle_original);
    handle_copy     = getHandle(env,clazz,desc_handle_copy);

/*
    status = DftiCopyDescriptor(handle_original,&handle_copy);
    return status;
*/
    // OOPS: the copy function looks not provided with MKL 9.0
    return (jint)DFTI_UNIMPLEMENTED;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiFreeDescriptor
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiFreeDescriptor(
    JNIEnv *env, jclass clazz, jobject desc_handle)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    long status;

    handle = getHandle(env,clazz,desc_handle);
    status = DftiFreeDescriptor(&handle);
    setHandle(env,clazz,desc_handle,handle);

    return status;
}

/************************************************************/

/*
// Descriptor Configuration
*/

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiSetValue_i
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;II)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiSetValue_1i(
    JNIEnv *env, jclass clazz,
    jobject desc_handle, jint config_param, jint config_val)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    long status;

    handle = getHandle(env,clazz,desc_handle);

    status = DftiSetValue(handle,
        (enum DFTI_CONFIG_PARAM) config_param,
        (enum DFTI_CONFIG_VALUE) config_val);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiSetValue_ivec
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;I[I)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiSetValue_1ivec(
    JNIEnv *env, jclass clazz,
    jobject desc_handle, jint config_param, jintArray config_val)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    jint *val_elements;
    long config_values[8];
    long dimension;
    long status;
    int i,dims;

    val_elements = (*env)->GetIntArrayElements(env,config_val,NULL);
    if (val_elements == NULL)
        (*env)->FatalError(env,"val_elements == NULL");

    handle = getHandle(env,clazz,desc_handle);

    dimension = 0; /* XXX workaround: GetValue gets only 32 bits for dimension */
    status = DftiGetValue(handle,DFTI_DIMENSION,&dimension);
    if (!DftiErrorClass(status,DFTI_NO_ERROR))
        (*env)->FatalError(env,"cannot get transform dimension");
    if (dimension > 7)
        (*env)->FatalError(env,"wrong dimension");

    if (config_param==DFTI_INPUT_STRIDES || config_param==DFTI_OUTPUT_STRIDES)
        dims = dimension + 1; // 1 extra element
    else if (config_param == DFTI_LENGTHS)
        dims = dimension;
    else
        (*env)->FatalError(env,"unknown config_param");

    for (i=0; i<dims; i++)
        config_values[i] = (long) val_elements[i];

    status = DftiSetValue(handle,
        (enum DFTI_CONFIG_PARAM) config_param,
        config_values);

    (*env)->ReleaseIntArrayElements(env,config_val,val_elements,JNI_ABORT);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiSetValue_f
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;IF)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiSetValue_1f(
    JNIEnv *env, jclass clazz,
    jobject desc_handle, jint config_param, jfloat config_val)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    float config_value;
    long status;

    config_value = (float) config_val;

    handle = getHandle(env,clazz,desc_handle);
    status = DftiSetValue(handle,
        (enum DFTI_CONFIG_PARAM) config_param,
        config_value);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiSetValue_d
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;ID)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiSetValue_1d(
    JNIEnv *env, jclass clazz,
    jobject desc_handle, jint config_param, jdouble config_val)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    double config_value;
    long status;

    config_value = (double) config_val;

    handle = getHandle(env,clazz,desc_handle);
    status = DftiSetValue(handle,
        (enum DFTI_CONFIG_PARAM) config_param,
        config_value);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiSetValue_s
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;ILjava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiSetValue_1s(
    JNIEnv *env, jclass clazz,
    jobject desc_handle, jint config_param, jstring config_val)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    const char* valUTFChars;
    long status;

    valUTFChars = (*env)->GetStringUTFChars(env,config_val,NULL);
    if (valUTFChars == NULL)
        (*env)->FatalError(env,"valUTFChars == NULL");

    handle = getHandle(env,clazz,desc_handle);
    status = DftiSetValue(handle,
        (enum DFTI_CONFIG_PARAM) config_param,
        (char*)valUTFChars);

    (*env)->ReleaseStringUTFChars(env,config_val,valUTFChars);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiGetValue_i
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;I[I)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiGetValue_1i(
    JNIEnv *env, jclass clazz,
    jobject desc_handle, jint config_param, jintArray config_val)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    jint *val_elements;
    long config_value;
    long status;

    handle = getHandle(env,clazz,desc_handle);

    val_elements = (*env)->GetIntArrayElements(env,config_val,NULL);
    if (val_elements == NULL)
        (*env)->FatalError(env,"val_elements == NULL");

    if (config_param == DFTI_PRECISION ||
        config_param == DFTI_FORWARD_DOMAIN ||
        config_param == DFTI_PLACEMENT ||
        config_param == DFTI_COMPLEX_STORAGE ||
        config_param == DFTI_REAL_STORAGE ||
        config_param == DFTI_CONJUGATE_EVEN_STORAGE ||
        config_param == DFTI_PACKED_FORMAT ||
        config_param == DFTI_ORDERING ||
        config_param == DFTI_TRANSPOSE ||
        config_param == DFTI_COMMIT_STATUS)
    {
        //
        // Named constant
        //
        enum DFTI_CONFIG_VALUE config_value;
        status = DftiGetValue(handle,
            (enum DFTI_CONFIG_PARAM) config_param,
            &config_value);
        val_elements[0] = (jint) config_value;
    }
    else if (
        config_param == DFTI_DIMENSION ||
        config_param == DFTI_LENGTHS || /* scalar, if dimension is 1 */
        config_param == DFTI_NUMBER_OF_TRANSFORMS ||
        config_param == DFTI_NUMBER_OF_USER_THREADS ||
        config_param == DFTI_INPUT_DISTANCE ||
        config_param == DFTI_OUTPUT_DISTANCE)
    {
        //
        // Integer scalar
        //
        long config_value;
        config_value = 0; /* XXX workaround: GetValue gets only 32 bits for dimension */
        status = DftiGetValue(handle,
            (enum DFTI_CONFIG_PARAM) config_param,
            &config_value);
        val_elements[0] = (jint) config_value;
    }
    else
        (*env)->FatalError(env,"wrong config_param");

    (*env)->ReleaseIntArrayElements(env,config_val,val_elements,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiGetValue_ivec
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;I[I)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiGetValue_1ivec(
    JNIEnv *env, jclass clazz,
    jobject desc_handle, jint config_param, jintArray config_val)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    jint *val_elements;
    long config_values[8];
    long dimension;
    long status;
    int i,dims;

    val_elements = (*env)->GetIntArrayElements(env,config_val,NULL);
    if (val_elements == NULL)
        (*env)->FatalError(env,"val_elements == NULL");

    handle = getHandle(env,clazz,desc_handle);

    dimension = 0; /* XXX workaround: GetValue gets only 32 bits for dimension */
    status = DftiGetValue(handle,DFTI_DIMENSION,&dimension);
    if (!DftiErrorClass(status,DFTI_NO_ERROR))
        (*env)->FatalError(env,"cannot get transform dimension");
    if (dimension > 7)
        (*env)->FatalError(env,"wrong transform dimension");

    status = DftiGetValue(handle,
        (enum DFTI_CONFIG_PARAM) config_param,
        config_values);

    if (config_param==DFTI_INPUT_STRIDES || config_param==DFTI_OUTPUT_STRIDES)
        dims = dimension + 1; // 1 extra element
    else if (config_param == DFTI_LENGTHS)
        dims = dimension;
    else
        (*env)->FatalError(env,"unknown config_param");

    for (i=0; i<dims; i++)
        val_elements[i] = (jint) config_values[i];

    (*env)->ReleaseIntArrayElements(env,config_val,val_elements,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiGetValue_f
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;I[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiGetValue_1f(
    JNIEnv *env, jclass clazz,
    jobject desc_handle, jint config_param, jfloatArray config_val)
{
    jfloat *val_elements;
    DFTI_DESCRIPTOR_HANDLE handle;
    enum DFTI_CONFIG_VALUE precision;
    long status;

    val_elements = (*env)->GetFloatArrayElements(env,config_val,NULL);
    if (val_elements == NULL)
        (*env)->FatalError(env,"val_elements == NULL");

    handle = getHandle(env,clazz,desc_handle);

    if (config_param != com_intel_mkl_DFTI_FORWARD_SCALE &&
        config_param != com_intel_mkl_DFTI_BACKWARD_SCALE)
        (*env)->FatalError(env,"unknown config_param");

    status = DftiGetValue(handle,DFTI_PRECISION,&precision);
    if (!DftiErrorClass(status,DFTI_NO_ERROR))
        (*env)->FatalError(env,"cannot get the transform precision");

    if (precision == DFTI_SINGLE) {
        float config_value;
        status = DftiGetValue(handle,
            (enum DFTI_CONFIG_PARAM) config_param,
            &config_value);
        val_elements[0] = (jfloat) config_value;
    } else if (precision == DFTI_DOUBLE)
        (*env)->FatalError(env,"wrong transform precision");
    else
        (*env)->FatalError(env,"unknown transform precision");

    (*env)->ReleaseFloatArrayElements(env,config_val,val_elements,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiGetValue_d
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;I[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiGetValue_1d(
    JNIEnv *env, jclass clazz,
    jobject desc_handle, jint config_param, jdoubleArray config_val)
{
    jdouble *val_elements;
    DFTI_DESCRIPTOR_HANDLE handle;
    enum DFTI_CONFIG_VALUE precision;
    long status;

    val_elements = (*env)->GetDoubleArrayElements(env,config_val,NULL);
    if (val_elements == NULL)
        (*env)->FatalError(env,"val_elements == NULL");

    handle = getHandle(env,clazz,desc_handle);

    if (config_param != com_intel_mkl_DFTI_FORWARD_SCALE &&
        config_param != com_intel_mkl_DFTI_BACKWARD_SCALE)
        (*env)->FatalError(env,"unknown config_param");

    status = DftiGetValue(handle,DFTI_PRECISION,&precision);
    if (!DftiErrorClass(status,DFTI_NO_ERROR))
        (*env)->FatalError(env,"cannot get the transform precision");

    if (precision == DFTI_SINGLE)
        (*env)->FatalError(env,"wrong transform precision");
    else if (precision == DFTI_DOUBLE) {
        double config_value;
        status = DftiGetValue(handle,
            (enum DFTI_CONFIG_PARAM) config_param,
            &config_value);
        val_elements[0] = (jdouble) config_value;
    } else
        (*env)->FatalError(env,"unknown transform precision");

    (*env)->ReleaseDoubleArrayElements(env,config_val,val_elements,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiGetValue_s
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;ILjava/lang/StringBuffer;)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiGetValue_1s(
    JNIEnv *env, jclass clazz,
    jobject desc_handle, jint config_param, jobject string_buffer)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    jclass string_buffer_clazz;
    jmethodID append_method;
    jbyteArray bytes_array;
    jbyte *byte_elements;
    jsize max_chars;
    jstring string;
    jobject buffer;
    long status;

    //
    // What's the maximal length of the string?
    //
    switch (config_param) {
        case com_intel_mkl_DFTI_DESCRIPTOR_NAME:
            max_chars = com_intel_mkl_DFTI_MAX_NAME_LENGTH;
            break;
        case com_intel_mkl_DFTI_VERSION:
            max_chars = com_intel_mkl_DFTI_VERSION_LENGTH;
            break;
        default:
            (*env)->FatalError(env,"unknown config_param");
    }

    //
    // Get a piece of memory to store the char[] data
    //
    bytes_array = (*env)->NewByteArray(env,max_chars+1);
    if (bytes_array == NULL)
        (*env)->FatalError(env,"bytes_array == NULL");
    byte_elements = (*env)->GetByteArrayElements(env,bytes_array,NULL);
    if (byte_elements == NULL)
        (*env)->FatalError(env,"byte_elements == NULL");

    //
    // Get the resulting string
    //
    handle = getHandle(env,clazz,desc_handle);
    status = DftiGetValue(handle,
        (enum DFTI_CONFIG_PARAM) config_param,
        byte_elements);
    string = (*env)->NewStringUTF(env,(char*)byte_elements);
    if (string == NULL)
        (*env)->FatalError(env,"string == NULL");

    //
    // Let garbage collector to care about the bytes_array
    //
    (*env)->ReleaseByteArrayElements(env,bytes_array,byte_elements,JNI_ABORT);

    //
    // Put the string into the string_buffer
    // CAUTION: assume string_buffer is empty
    //
    if (string_buffer == NULL)
        (*env)->FatalError(env,"string_buffer == NULL");
    string_buffer_clazz = (*env)->GetObjectClass(env,string_buffer);
    append_method = (*env)->GetMethodID(env,string_buffer_clazz,
        "append","(Ljava/lang/String;)Ljava/lang/StringBuffer;");
    if (append_method == NULL)
        (*env)->FatalError(env,"append_method == NULL");
    buffer = (*env)->CallObjectMethod(env,string_buffer,append_method,string);

    //
    // Following check looks redundant, but anyway...
    //
    if (buffer == NULL)
        (*env)->FatalError(env,"buffer == NULL");

    return (jint)status;
}

/************************************************************/

/*
// DFT Computation
*/

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiComputeForward_fi
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiComputeForward_1fi(
    JNIEnv *env, jclass clazz, jobject desc_handle, jfloatArray x_inout)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    float* x_inout_elements;
    long status;

    x_inout_elements = (*env)->GetFloatArrayElements(env,x_inout,NULL);
    if (x_inout_elements == NULL)
        (*env)->FatalError(env,"x_inout_elements == NULL");

    handle = getHandle(env,clazz,desc_handle);
    status = DftiComputeForward(handle,x_inout_elements);

    (*env)->ReleaseFloatArrayElements(env,x_inout,x_inout_elements,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiComputeForward_f
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiComputeForward_1f(
    JNIEnv *env, jclass clazz, jobject desc_handle, jfloatArray x_in, jfloatArray x_out)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    float *x_in_elements, *x_out_elements;
    long status;

    x_in_elements  = (*env)->GetFloatArrayElements(env,x_in ,NULL);
    x_out_elements = (*env)->GetFloatArrayElements(env,x_out,NULL);
    if (x_in_elements==NULL || x_out_elements==NULL)
        (*env)->FatalError(env,"x_in_elements==NULL || x_out_elements==NULL");

    handle = getHandle(env,clazz,desc_handle);
    status = DftiComputeForward(handle,x_in_elements,x_out_elements);

    (*env)->ReleaseFloatArrayElements(env,x_out,x_out_elements,0);
    (*env)->ReleaseFloatArrayElements(env,x_in ,x_in_elements,JNI_ABORT);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiComputeForward_di
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiComputeForward_1di(
    JNIEnv *env, jclass clazz, jobject desc_handle, jdoubleArray x_inout)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    double* x_inout_elements;
    long status;

    x_inout_elements = (*env)->GetDoubleArrayElements(env,x_inout,NULL);
    if (x_inout_elements == NULL)
        (*env)->FatalError(env,"x_inout_elements == NULL");

    handle = getHandle(env,clazz,desc_handle);
    status = DftiComputeForward(handle,x_inout_elements);

    (*env)->ReleaseDoubleArrayElements(env,x_inout,x_inout_elements,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiComputeForward_d
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiComputeForward_1d(
    JNIEnv *env, jclass clazz, jobject desc_handle, jdoubleArray x_in, jdoubleArray x_out)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    double *x_in_elements, *x_out_elements;
    long status;

    x_in_elements  = (*env)->GetDoubleArrayElements(env,x_in ,NULL);
    x_out_elements = (*env)->GetDoubleArrayElements(env,x_out,NULL);
    if (x_in_elements==NULL || x_out_elements==NULL)
        (*env)->FatalError(env,"x_in_elements==NULL || x_out_elements==NULL");

    handle = getHandle(env,clazz,desc_handle);
    status = DftiComputeForward(handle,x_in_elements,x_out_elements);

    (*env)->ReleaseDoubleArrayElements(env,x_out,x_out_elements,0);
    (*env)->ReleaseDoubleArrayElements(env,x_in ,x_in_elements,JNI_ABORT);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiComputeBackward_fi
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiComputeBackward_1fi(
    JNIEnv *env, jclass clazz, jobject desc_handle, jfloatArray x_inout)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    float* x_inout_elements;
    long status;

    x_inout_elements = (*env)->GetFloatArrayElements(env,x_inout,NULL);
    if (x_inout_elements == NULL)
        (*env)->FatalError(env,"x_inout_elements == NULL");

    handle = getHandle(env,clazz,desc_handle);
    status = DftiComputeBackward(handle,x_inout_elements);

    (*env)->ReleaseFloatArrayElements(env,x_inout,x_inout_elements,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiComputeBackward_f
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;[F[F)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiComputeBackward_1f(
    JNIEnv *env, jclass clazz, jobject desc_handle, jfloatArray x_in, jfloatArray x_out)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    float *x_in_elements, *x_out_elements;
    long status;

    x_in_elements  = (*env)->GetFloatArrayElements(env,x_in ,NULL);
    x_out_elements = (*env)->GetFloatArrayElements(env,x_out,NULL);
    if (x_in_elements==NULL || x_out_elements==NULL)
        (*env)->FatalError(env,"x_in_elements==NULL || x_out_elements==NULL");

    handle = getHandle(env,clazz,desc_handle);
    status = DftiComputeBackward(handle,x_in_elements,x_out_elements);

    (*env)->ReleaseFloatArrayElements(env,x_out,x_out_elements,0);
    (*env)->ReleaseFloatArrayElements(env,x_in ,x_in_elements,JNI_ABORT);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiComputeBackward_di
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiComputeBackward_1di(
    JNIEnv *env, jclass clazz, jobject desc_handle, jdoubleArray x_inout)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    double* x_inout_elements;
    long status;

    x_inout_elements = (*env)->GetDoubleArrayElements(env,x_inout,NULL);
    if (x_inout_elements == NULL)
        (*env)->FatalError(env,"x_inout_elements == NULL");

    handle = getHandle(env,clazz,desc_handle);
    status = DftiComputeBackward(handle,x_inout_elements);

    (*env)->ReleaseDoubleArrayElements(env,x_inout,x_inout_elements,0);

    return (jint)status;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiComputeBackward_d
 * Signature: (Lcom/intel/mkl/DFTIDESCRIPTORHANDLE;[D[D)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiComputeBackward_1d(
    JNIEnv *env, jclass clazz, jobject desc_handle, jdoubleArray x_in, jdoubleArray x_out)
{
    DFTI_DESCRIPTOR_HANDLE handle;
    double *x_in_elements, *x_out_elements;
    long status;

    x_in_elements  = (*env)->GetDoubleArrayElements(env,x_in ,NULL);
    x_out_elements = (*env)->GetDoubleArrayElements(env,x_out,NULL);
    if (x_in_elements==NULL || x_out_elements==NULL)
        (*env)->FatalError(env,"x_in_elements==NULL || x_out_elements==NULL");

    handle = getHandle(env,clazz,desc_handle);
    status = DftiComputeBackward(handle,x_in_elements,x_out_elements);

    (*env)->ReleaseDoubleArrayElements(env,x_out,x_out_elements,0);
    (*env)->ReleaseDoubleArrayElements(env,x_in ,x_in_elements,JNI_ABORT);

    return (jint)status;
}

/************************************************************/

/*
// Status Checking Functions
*/

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiErrorClass
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiErrorClass(
    JNIEnv *env, jclass clazz, jint status, jint error_class)
{
    long predicate = DftiErrorClass((long)status,(long)error_class);
    return predicate;
}

/*
 * Class:     com_intel_mkl_DFTIDESCRIPTORHANDLE
 * Method:    DftiErrorMessage
 * Signature: (I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_intel_mkl_DFTIDESCRIPTORHANDLE_DftiErrorMessage(
    JNIEnv *env, jclass clazz, jint status)
{
    char* message = DftiErrorMessage((long)status);
    jstring error_message = (*env)->NewStringUTF(env,message);
    return error_message;
}
