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
 * Java counterpart for DFTI_DESCRIPTOR_HANDLE type of the DFT
 * Interface for C/C++.
 *
 * <p>For the C/C++ interface, a DFTI descriptor handle is just
 * a reference to DFTI descriptor. Instance of this Java class
 * holds such native reference in the form of 64-bits integer
 * (8 bytes must be enough to encode the native address).
 *
 * <p>If copying a handle object, the software also copies the
 * descriptor using DftiCopyDescriptor() function from MKL.
 * So, each DFTI descriptor created via this interface has the
 * unique handler object which holds the reference to it.
 *
 * <p>This correspondence is used for garbage collecting DFTI
 * descriptors. When a handler object is being collected, its
 * finalizer method also calls DftiFreeDescriptor() to destroy
 * the corresponding DFTI descriptor.
 *
 * <p>This allows to treat DFTI descriptors as managed objects;
 * because one do not need to worry about freeing them.
 * However, the DFTI wrapper also provides the FreeDescriptor()
 * method for destroying DFTI descriptors explicitly.
 *
 * @see DFTI
 */
class DFTIDESCRIPTORHANDLE {

    //
    // This class is package private and so covered from users.
    // Public access to its functionality is given by the class
    // DFTI.DESCRIPTOR_HANDLE enclosed into the class DFTI.
    //
    // Please let me explain the reason for hiding this class
    // from users. Actually, there is no reason at all, except of
    // the incompatibility between JDK versions 1.4, 5.0, and 6.0
    // which differently process native interfaces, if class name
    // contains the underscore symbol ('_').
    //
    // That is why there is no underscores in this class name:
    // this allows the same set of native stubs to work with all
    // versions of JDK. And the class DFT.DESCRIPTOR_HANDLE just
    // give more appropriate name to this class - by extending it.
    //

    /** Load the stubs to the native MKL functions. */
    static {
        System.loadLibrary("mkl_java_stubs");
    }

    //////////////////////////////////////////////////////////

    /** Native address encoded as bytes array. */
    private long handle = 0;

    /** Explicit instantiation creates null handle. */
    protected DFTIDESCRIPTORHANDLE() {}

    /** Free the DFTI descriptor being held, if any. */
    protected void finalize() {
        if (handle != 0) {
            // Get the status, but ignore its value:
            int status = DftiFreeDescriptor(this);
            handle = 0;
        }
    }

    //////////////////////////////////////////////////////////

    //
    // The following native methods provide the DFTI functionality.
    // These methods are package private and so covered from user.
    // Public interface to these methods is given by the DFTI class.
    //

    //
    // Groups of methods:
    // . Descriptor Manipulation
    // . DFT Computation
    // . Descriptor Configuration
    // . Status Checking Functions
    //

    //
    // Descriptor Manipulation
    //

    /** Stub for MKL function DftiCreateDescriptor(). */
    native static int DftiCreateDescriptor(DFTIDESCRIPTORHANDLE desc_handle,
        int precision, int forward_domain, int dimension, int[] length);

    /** Stub for MKL function DftiCommitDescriptor() */
    native static int DftiCommitDescriptor(DFTIDESCRIPTORHANDLE desc_handle);

    /** Stub for MKL function DftiCopyDescriptor(). */
    native static int DftiCopyDescriptor(
        DFTIDESCRIPTORHANDLE desc_handle_original,
        DFTIDESCRIPTORHANDLE desc_handle_copy);

    /** Stub for MKL function DftiFreeDescriptor(). */
    native static int DftiFreeDescriptor(DFTIDESCRIPTORHANDLE desc_handle);

    //
    // DFT Computation
    //

    /** Stub for DftiComputeForward(); single precision, inplace. */
    native static int DftiComputeForward_fi(DFTIDESCRIPTORHANDLE desc_handle,
        float[] x_inout);

    /** Stub for DftiComputeForward(); single precision, not inplace. */
    native static int DftiComputeForward_f(DFTIDESCRIPTORHANDLE desc_handle,
        float[] x_in, float[] x_out);

    /** Stub for DftiComputeForward(); double precision, inplace. */
    native static int DftiComputeForward_di(DFTIDESCRIPTORHANDLE desc_handle,
        double[] x_inout);

    /** Stub for DftiComputeForward(); double precision, not inplace. */
    native static int DftiComputeForward_d(DFTIDESCRIPTORHANDLE desc_handle,
        double[] x_in, double[] x_out);

    /** Stub for DftiComputeBackward(); single precision, inplace. */
    native static int DftiComputeBackward_fi(DFTIDESCRIPTORHANDLE desc_handle,
        float[] x_inout);

    /** Stub for DftiComputeBackward(); single precision, not inplace. */
    native static int DftiComputeBackward_f(DFTIDESCRIPTORHANDLE desc_handle,
        float[] x_in, float[] x_out);

    /** Stub for DftiComputeBackward(); double precision, inplace. */
    native static int DftiComputeBackward_di(DFTIDESCRIPTORHANDLE desc_handle,
        double[] x_inout);

    /** Stub for DftiComputeBackward(); double precision, not inplace. */
    native static int DftiComputeBackward_d(DFTIDESCRIPTORHANDLE desc_handle,
        double[] x_in, double[] x_out);

    //
    // Descriptor Configuration
    //

    /** Stub for MKL function DftiSetValue(), integer value. */
    native static int DftiSetValue_i(DFTIDESCRIPTORHANDLE desc_handle,
        int config_param, int config_val);

    /** Stub for MKL function DftiSetValue(), integer vector. */
    native static int DftiSetValue_ivec(DFTIDESCRIPTORHANDLE desc_handle,
        int config_param, int[] config_val);

    /** Stub for MKL function DftiSetValue(), single precision value. */
    native static int DftiSetValue_f(DFTIDESCRIPTORHANDLE desc_handle,
        int config_param, float config_val);

    /** Stub for MKL function DftiSetValue(), double precision value. */
    native static int DftiSetValue_d(DFTIDESCRIPTORHANDLE desc_handle,
        int config_param, double config_val);

    /** Stub for MKL function DftiSetValue(), String value. */
    native static int DftiSetValue_s(DFTIDESCRIPTORHANDLE desc_handle,
        int config_param, String config_val);

    /**
     * Stub for MKL function DftiGetValue(), integer value.
     *
     * <p>Before invoking this method, create dummy int[] array
     * of the length 1 for storing the result:
     * <pre>
     *     int[] config_val = new int[1];
     *     int status = GetValue(desc_handle,config_param,config_val);
     *     ...use the config_val[0] value...
     * </pre>
     */
    native static int DftiGetValue_i(DFTIDESCRIPTORHANDLE desc_handle,
        int config_param, int[] config_val);

    /**
     * Stub for MKL function DftiGetValue(), integer vector.
     *
     * <p>Before invoking this method, create a dummy int[]
     * vector of appropriate length for storing the result:
     * <pre>
     *     int[] config_val = new int[request_dimension];
     *     int status = GetValue(desc_handle,donfig_param,config_val);
     *     ...use the config_val vector...;
     * </pre>
     *
     * <p>If requesting DFTI.LENGTHS, request_dimension must be
     * equal or greater than the dimension of the transtorm.
     * If requesting DFTI.INPUT_STRIDES or DFTI.OUTPUT_STRIDES,
     * allow one more element as dimension+1 to store strides
     * (the data offset is added to the strides array).
     */
    native static int DftiGetValue_ivec(DFTIDESCRIPTORHANDLE desc_handle,
        int config_param, int[] config_val);

    /**
     * Stub for MKL function DftiGetValue(), single precision value.
     *
     * <p>Before invoking this method, create dummy float[] array
     * of the length 1 for storing the result:
     * <pre>
     *     float[] config_val = new float[1];
     *     int status = GetValue(desc_handle,config_param,config_val);
     *     ...use the config_val[0] value...
     * </pre>
     */
    native static int DftiGetValue_f(DFTIDESCRIPTORHANDLE desc_handle,
        int config_param, float[] config_val);

    /**
     * Stub for MKL function DftiGetValue(), double precision value.
     *
     * <p>Before invoking this method, create dummy double[] array
     * of the length 1 for storing the result:
     * <pre>
     *     double[] config_val = new double[1];
     *     int status = GetValue(desc_handle,config_param,config_val);
     *     ...use the config_val[0] value...
     * </pre>
     */
    native static int DftiGetValue_d(DFTIDESCRIPTORHANDLE desc_handle,
        int config_param, double[] config_val);

    /**
     * Stub for MKL function DftiGetValue(), String value.
     *
     * <p>Before invoking this method, create dummy StringBuffer
     * object for storing the result:
     * <pre>
     *     StringBuffer config_val = new StringBuffer();
     *     int status = GetValue(desc_handle,config_param,config_val);
     *     String value = config_val.toString();
     * </pre>
     */
    native static int DftiGetValue_s(DFTIDESCRIPTORHANDLE desc_handle,
        int config_param, StringBuffer config_val);

    //
    // Status Checking Functions
    //

    /** Stub for MKL function DftiErrorClass(). */
    native static int DftiErrorClass(int status, int error_class);

    /** Stub for MKL function DftiErrorMessage(). */
    native static String DftiErrorMessage(int status);
}
