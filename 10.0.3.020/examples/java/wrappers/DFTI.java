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
 * Wrappers to DFT functions from Intel MKL.
 *
 * <p>The FDT Interface (DFTI) for C/C++ is used for accessing
 * to the DFT functions. See MKL Reference Manual, Chapter
 * 11 "Fourier Transform Fnctions", Section "DFT Functions".
 *
 * <p>This is demo wrapper which intends to demonstrate:
 * <ul>
 *   <li>binding MKL/DFT functions with Java
 *   <li>processing DFTI descriptor handles
 *   <li>utilizing exceptions to check status
 *   <li>storing 1- and multi-dimensional data
 *   <li>encoding complex numbers
 * </ul>
 *
 * <p>CAUTION: This demo wrapper does not:
 * <ul>
 *   <li>demonstrate using huge arrays (>2 billion elements)
 *   <li>demonstrate processing arrays in native memory
 *   <li>check correctness of function parameters
 *   <li>demonstrate performance optimizations
 * </ul>
 *
 * <p>These wrappers assume using 1-dimensional Java arrays of
 * the type double[] or float[] to process floating-point data.
 * A multi-dimensional data series is stored into 1-dimensional
 * array as column-major or row-major 1-dimensional data series.
 *
 * <p>A complex number is stored as 2-elements array: real part
 * of the number as the 1st element of the array, and imaginary
 * part as the 2nd.
 *
 * <p>A complex data series of the length N is stored as a real
 * series of the length 2*N. The n-th complex element is stored
 * as the pair of 2*n-th and (2*n+1)-th real elements: the even
 * indexed for real part and the odd indexed for imaginary part.
 *
 * <p>For more details, please see the MKL User's Guide.
 */
public final class DFTI {

    /** Instantiation is disabled. */
    private DFTI() {}

    //////////////////////////////////////////////////////////

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
     */
    public static final class DESCRIPTOR_HANDLE extends DFTIDESCRIPTORHANDLE {
        /** Explicit instantiation creates null handle. */
        public DESCRIPTOR_HANDLE() {}
    }

    //////////////////////////////////////////////////////////

    /**
     * Thrown by DFTI function in case of error.
     *
     * <p>Unlike the native DFTI functions, their Java counterparts
     * do not return status value. Instead, the Java functions throw
     * StatusException in case if the status returned by the native
     * code signals about some error. That is, if:
     * <pre>
     *     DFTI.ErrorClass(status,DFTI.NO_ERROR) ==  false
     * </pre>
     *
     * <p>Given an instance of this exception, use the getStatus()
     * and getFunctionName() methods to see: which function has
     * thrown the exception and why.
     */
    public static final class StatusException extends RuntimeException {
        /**
         * No dummy instantinces.
         */
        private StatusException() {}

        /**
         * Signal about the error status raised by the function.
         */
        public StatusException(int status, String functionName) {
            super(functionName + "(), status=" + status
                + " [ " + DFTI.ErrorMessage(status) + " ]");
            this.functionName = functionName;
            this.status = status;
        }

        /**
         * Which function has thrown this exception?
         */
        public String getFunctionName() {
            return this.functionName;
        }

        /**
         * What status has been thrown?
         */
        public int getStatus() {
            return this.status;
        }

        /**
         * Name of the DFTI function which thrown this exception.
         */
        private String functionName;

        /**
         * The status returned by the function.
         */
        private int status;
    }

    //////////////////////////////////////////////////////////

    //
    // Groups of methods:
    // . Descriptor Manipulation
    // . Descriptor Configuration
    // . DFT Computation
    // . Status Checking Functions
    //

    //////////////////////////////////////////////////////////

    //
    // Descriptor Manipulation
    //

    /**
     * Wrapper for MKL function DftiCreateDescriptor() for
     * 1-, or 2-, or multi-dimensional transform.
     *
     * <p>To follow the underlying C/C++ variant of DFTI, this
     * Java wrapper assumes row-major packaging of matrices or
     * multi-dimensional data sequences. This way, elements of
     * the length[] array are reversed by this constructor, as
     * the underlying DFT implementation is mainly oriented to
     * FORTRAN and so assumes column-major data packaging.
     *
     * @see #CreateDescriptor(int,int,int,int)
     */
    public static DFTI.DESCRIPTOR_HANDLE CreateDescriptor(
        int precision, int forward_domain, int dimension, int[] length)
    {
        DFTI.DESCRIPTOR_HANDLE desc_handle = new DFTI.DESCRIPTOR_HANDLE();
        int status = DFTI.DESCRIPTOR_HANDLE.DftiCreateDescriptor(
            desc_handle,precision,forward_domain,dimension,length);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"CreateDescriptor");
        return desc_handle;
    }

    /**
     * Wrapper for MKL function DftiCreateDescriptor() for
     * 1-dimensional transform.
     *
     * @throws IllegalArgumentException If dimension!=1.
     *
     * @see #CreateDescriptor(int,int,int,int[])
     */
    public static DFTI.DESCRIPTOR_HANDLE CreateDescriptor(
        int precision, int forward_domain, int dimension, int length)
    {
        DFTI.DESCRIPTOR_HANDLE desc_handle = new DFTI.DESCRIPTOR_HANDLE();
        if (dimension != 1)
            throw new IllegalArgumentException("wrong dimension: " + dimension);
        int status = DFTI.DESCRIPTOR_HANDLE.DftiCreateDescriptor(
            desc_handle,precision,forward_domain,dimension, new int[] {length});
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"CreateDescriptor");
        return desc_handle;
    }

    /**
     * Wrapper for MKL function DftiCommitDescriptor().
     */
    public static void CommitDescriptor(DFTI.DESCRIPTOR_HANDLE desc_handle)
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiCommitDescriptor(desc_handle);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"CommitDescriptor");
    }

    /**
     * Wrapper for MKL function DftiCopyDescriptor().
     */
    public static DFTI.DESCRIPTOR_HANDLE CopyDescriptor(
        DFTI.DESCRIPTOR_HANDLE desc_handle_original)
    {
        DFTI.DESCRIPTOR_HANDLE desc_handle_copy = new DFTI.DESCRIPTOR_HANDLE();
        int status = DFTI.DESCRIPTOR_HANDLE.DftiCopyDescriptor(
            desc_handle_original, desc_handle_copy);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"CopyDescriptor");
        return desc_handle_copy;
    }

    /**
     * Wrapper for MKL function DftiFreeDescriptor().
     */
    public static void FreeDescriptor(DFTI.DESCRIPTOR_HANDLE desc_handle) {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiFreeDescriptor(desc_handle);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"FreeDescriptor");
    }

    //////////////////////////////////////////////////////////

    //
    // Descriptor Configuration
    //

    /**
     * Wraper for MKL function DftiSetValue(), integer value.
     *
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,int[])
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,float)
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,double)
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,String)
     */
    public static void SetValue(DFTI.DESCRIPTOR_HANDLE desc_handle,
        int config_param, int config_val)
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiSetValue_i(
            desc_handle,config_param,config_val);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"SetValue");
    }

    /**
     * Wraper for MKL function DftiSetValue(), integer vector.
     *
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,int)
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,float)
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,double)
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,String)
     */
    public static void SetValue(DFTI.DESCRIPTOR_HANDLE desc_handle,
        int config_param, int config_val[])
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiSetValue_ivec(
            desc_handle,config_param,config_val);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"SetValue");
    }

    /**
     * Wraper for MKL function DftiSetValue(), single precision
     * value of real type.
     *
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,int)
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,int[])
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,double)
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,String)
     */
    public static void SetValue(DFTI.DESCRIPTOR_HANDLE desc_handle,
        int config_param, float config_val)
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiSetValue_f(
            desc_handle,config_param,config_val);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"SetValue");
    }

    /**
     * Wraper for MKL function DftiSetValue(), double precision
     * value of real type.
     *
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,int)
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,int[])
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,float)
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,String)
     */
    public static void SetValue(DFTI.DESCRIPTOR_HANDLE desc_handle,
        int config_param, double config_val)
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiSetValue_d(
            desc_handle,config_param,config_val);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"SetValue");
    }

    /**
     * Wraper for MKL function DftiSetValue(), string value.
     *
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,int)
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,int[])
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,float)
     * @see #SetValue(DFTI.DESCRIPTOR_HANDLE,int,double)
     */
    public static void SetValue(DFTI.DESCRIPTOR_HANDLE desc_handle,
        int config_param, String config_val)
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiSetValue_s(
            desc_handle,config_param,config_val);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"SetValue");
    }

    /**
     * Wraper for MKL function DftiGetValue().
     *
     * <p>Depending on the config_param, the function returns Integer,
     * or int[], or Double, or String object. For example:
     * <pre>
     *     String version = (String) DFTI.GetValue(desc_handle,DFTI.VERSION);
     *     Integer dimension = (Integer) DFTI.GetValue(desc_handle,DFTI.DIMENSION);
     *     Double forward_scale = (Double) DFTI.GetValue(desc_handle,DFTI.FORWARD_SCALE);
     * </pre>
     *
     * <p>Like the native DFTI software, this Java interface returns
     * forward or backward scale parameter with the precision which
     * is assigned to the transform. That is, Float object is returned
     * for single-precision transform, or Double object is returned if
     * the transform's precision is double.
     *
     * <p>Like the native DFTI software, this Java interface returns
     * transform lengths as Integer scalar if dimension is 1, or as
     * int[] array if transform dimension is 2 or higher:
     * <pre>
     *     int[] lengths = (int[]) DFTI.GetValue(desc_handle,DFTI.LENGTHS);
     * </pre>
     *
     * <p>If DFTI.LENGTH is requested for multi-dimensional case,
     * length of the returned int[] array equals to dimension of
     * the transform.
     *
     * <p>If requesting DFTI.INPUT_STRIDES or DFTI.OUTPUT_STRIDES,
     * the returned array's length equals to dimension+1, because
     * the "offset" parameter is added to the array as its 0th element.
     *
     * @throws IllegalArgumentException If config_param is unknown.
     */
    public static Object GetValue(DFTI.DESCRIPTOR_HANDLE desc_handle,
        int config_param)
    {
        int status;
        Object result;
        //
        // Type of result depends on config_param
        //
        if (config_param == DFTI.PRECISION ||
            config_param == DFTI.FORWARD_DOMAIN ||
            config_param == DFTI.DIMENSION ||
            config_param == DFTI.NUMBER_OF_TRANSFORMS ||
            config_param == DFTI.PLACEMENT ||
            config_param == DFTI.COMPLEX_STORAGE ||
            config_param == DFTI.REAL_STORAGE ||
            config_param == DFTI.CONJUGATE_EVEN_STORAGE ||
            config_param == DFTI.PACKED_FORMAT ||
            config_param == DFTI.NUMBER_OF_USER_THREADS ||
            config_param == DFTI.INPUT_DISTANCE ||
            config_param == DFTI.OUTPUT_DISTANCE ||
            config_param == DFTI.ORDERING ||
            config_param == DFTI.TRANSPOSE ||
            config_param == DFTI.COMMIT_STATUS)
        {
            //
            // Integer scalar
            //
            int[] config_val = new int[1];
            status = DFTI.DESCRIPTOR_HANDLE.DftiGetValue_i(desc_handle,
                config_param,config_val);
            result = new Integer(config_val[0]);
        }
        else if (config_param == DFTI.INPUT_STRIDES ||
                 config_param == DFTI.OUTPUT_STRIDES)
        {
            //
            // int[] vector
            //
            int dimension =
                ((Integer) GetValue(desc_handle,DFTI.DIMENSION)).intValue();
            int[] config_val = new int [dimension + 1];
            status = DFTI.DESCRIPTOR_HANDLE.DftiGetValue_ivec(desc_handle,
                config_param,config_val);
            result = config_val;
        }
        else if (config_param == DFTI.LENGTHS)
        {
            //
            // Any dimension, return the lengths as int[] vector
            //
            int dimension = ((Integer) GetValue(desc_handle,DFTI.DIMENSION)).intValue();
            int[] config_val = new int [dimension];
            if (dimension == 1) {
                status = DFTI.DESCRIPTOR_HANDLE.DftiGetValue_i(desc_handle,config_param,config_val);
                result = new Integer(config_val[0]);
            } else {
                status = DFTI.DESCRIPTOR_HANDLE.DftiGetValue_ivec(desc_handle,config_param,config_val);
                result = config_val;
            }
        }
        else if (config_param == DFTI.FORWARD_SCALE ||
                 config_param == DFTI.BACKWARD_SCALE)
        {
            //
            // Float or Double scalar
            //
            int precision = ((Integer) GetValue(desc_handle,DFTI.PRECISION)).intValue();
            if (precision == DFTI.SINGLE) {
                float[] config_val = new float[1];
                status = DFTI.DESCRIPTOR_HANDLE.DftiGetValue_f(desc_handle,config_param,config_val);
                result = new Float(config_val[0]);
            } else if (precision == DFTI.DOUBLE) {
                double[] config_val = new double[1];
                status = DFTI.DESCRIPTOR_HANDLE.DftiGetValue_d(desc_handle,config_param,config_val);
                result = new Double(config_val[0]);
            } else
                throw new Error("unknown precision");
        }
        else if (config_param == DFTI.DESCRIPTOR_NAME ||
                 config_param == DFTI.VERSION)
        {
            //
            // String
            //
            StringBuffer config_val = new StringBuffer();
            status = DFTI.DESCRIPTOR_HANDLE.DftiGetValue_s(desc_handle,config_param,config_val);
            result = config_val.toString();
        }
        else
            throw new IllegalArgumentException(
                "unknown config_param: " + config_param);
        //
        // Check status and return
        //
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"GetValue");
        return result;
    }

    //////////////////////////////////////////////////////////

    //
    // DFT Computation
    //

    /**
     * Wrapper for DftiComputeForward(); real or complex data,
     * single precision, inplace transform.
     *
     * @see #ComputeForward(DFTI.DESCRIPTOR_HANDLE,float[],float[])
     * @see #ComputeForward(DFTI.DESCRIPTOR_HANDLE,double[])
     * @see #ComputeForward(DFTI.DESCRIPTOR_HANDLE,double[],double[])
     */
    public static void ComputeForward(DFTI.DESCRIPTOR_HANDLE desc_handle,
        float[] x_inout)
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiComputeForward_fi(
            desc_handle, x_inout);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"ComputeForward");
    }

    /**
     * Wrapper for DftiComputeForward(); real or complex data,
     * single precision, not-inplace transform.
     *
     * @see #ComputeForward(DFTI.DESCRIPTOR_HANDLE,float[])
     * @see #ComputeForward(DFTI.DESCRIPTOR_HANDLE,double[])
     * @see #ComputeForward(DFTI.DESCRIPTOR_HANDLE,double[],double[])
     */
    public static void ComputeForward(DFTI.DESCRIPTOR_HANDLE desc_handle,
        float[] x_in, float[] x_out)
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiComputeForward_f(
            desc_handle, x_in, x_out);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"ComputeForward");
    }

    /**
     * Wrapper for DftiComputeForward(); real or complex data,
     * double precision, inplace transform.
     *
     * @see #ComputeForward(DFTI.DESCRIPTOR_HANDLE,float[])
     * @see #ComputeForward(DFTI.DESCRIPTOR_HANDLE,float[],float[])
     * @see #ComputeForward(DFTI.DESCRIPTOR_HANDLE,double[],double[])
     */
    public static void ComputeForward(DFTI.DESCRIPTOR_HANDLE desc_handle,
        double[] x_inout)
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiComputeForward_di(
            desc_handle, x_inout);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"ComputeForward");
    }

    /**
     * Wrapper for DftiComputeForward(); real or complex data,
     * double precision, not-inplace transform.
     *
     * @see #ComputeForward(DFTI.DESCRIPTOR_HANDLE,float[])
     * @see #ComputeForward(DFTI.DESCRIPTOR_HANDLE,float[],float[])
     * @see #ComputeForward(DFTI.DESCRIPTOR_HANDLE,double[])
     */
    public static void ComputeForward(DFTI.DESCRIPTOR_HANDLE desc_handle,
        double[] x_in, double[] x_out)
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiComputeForward_d(
            desc_handle, x_in, x_out);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"ComputeForward");
    }

    /**
     * Wrapper for DftiComputeBackward(); real or complex data,
     * single precision, inplace transform.
     *
     * @see #ComputeBackward(DFTI.DESCRIPTOR_HANDLE,float[],float[])
     * @see #ComputeBackward(DFTI.DESCRIPTOR_HANDLE,double[])
     * @see #ComputeBackward(DFTI.DESCRIPTOR_HANDLE,double[],double[])
     */
    public static void ComputeBackward(DFTI.DESCRIPTOR_HANDLE desc_handle,
        float[] x_inout)
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiComputeBackward_fi(
            desc_handle, x_inout);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"ComputeBackward");
    }

    /**
     * Wrapper for DftiComputeBackward(); real or complex data,
     * single precision, not-inplace transform.
     *
     * @see #ComputeBackward(DFTI.DESCRIPTOR_HANDLE,float[])
     * @see #ComputeBackward(DFTI.DESCRIPTOR_HANDLE,double[])
     * @see #ComputeBackward(DFTI.DESCRIPTOR_HANDLE,double[],double[])
     */
    public static void ComputeBackward(DFTI.DESCRIPTOR_HANDLE desc_handle,
        float[] x_in, float[] x_out)
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiComputeBackward_f(
            desc_handle, x_in, x_out);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"ComputeBackward");
    }

    /**
     * Wrapper for DftiComputeBackward(); real or complex data,
     * double precision, inplace transform.
     *
     * @see #ComputeBackward(DFTI.DESCRIPTOR_HANDLE,float[])
     * @see #ComputeBackward(DFTI.DESCRIPTOR_HANDLE,float[],float[])
     * @see #ComputeBackward(DFTI.DESCRIPTOR_HANDLE,double[],double[])
     */
    public static void ComputeBackward(DFTI.DESCRIPTOR_HANDLE desc_handle,
        double[] x_inout)
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiComputeBackward_di(
            desc_handle, x_inout);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"ComputeBackward");
    }

    /**
     * Wrapper for DftiComputeBackward(); real or complex data,
     * double precision, not-inplace transform.
     *
     * @see #ComputeBackward(DFTI.DESCRIPTOR_HANDLE,float[])
     * @see #ComputeBackward(DFTI.DESCRIPTOR_HANDLE,float[],float[])
     * @see #ComputeBackward(DFTI.DESCRIPTOR_HANDLE,double[])
     */
    public static void ComputeBackward(DFTI.DESCRIPTOR_HANDLE desc_handle,
        double[] x_in, double[] x_out)
    {
        int status = DFTI.DESCRIPTOR_HANDLE.DftiComputeBackward_d(
            desc_handle, x_in, x_out);
        boolean class_error = DFTI.ErrorClass(status,DFTI.NO_ERROR);
        if (!class_error)
            throw new DFTI.StatusException(status,"ComputeBackward");
    }

    //////////////////////////////////////////////////////////

    //
    // Status Checking Functions
    //

    /**
     * Wrapper for MKL function DftiErrorClass().
     */
    public static boolean ErrorClass(int status, int error_class)
    {
        int predicate = DFTI.DESCRIPTOR_HANDLE.DftiErrorClass(status,error_class);
        return predicate != 0;
    }

    /**
     * Wrapper for MKL function DftiErrorMessage().
     */
    public static String ErrorMessage(int status) {
        String error_message = DFTI.DESCRIPTOR_HANDLE.DftiErrorMessage(status);
        return error_message;
    }

    //////////////////////////////////////////////////////////

    //
    // Java counterparts for the DFTI configuration parameters.
    //

    /** Constant for DFTI_PRECISION from "mkl_dfti.h" */
    public final static int PRECISION = 3;

    /** Constant for DFTI_FORWARD_DOMAIN from "mkl_dfti.h" */
    public final static int FORWARD_DOMAIN = 0;

    /** Constant for DFTI_DIMENSION from "mkl_dfti.h" */
    public final static int DIMENSION = 1;

    /** Constant for DFTI_LENGTHS from "mkl_dfti.h" */
    public final static int LENGTHS = 2;

    /** Constant for DFTI_NUMBER_OF_TRANSFORMS from "mkl_dfti.h" */
    public final static int NUMBER_OF_TRANSFORMS = 7;

    /** Constant for DFTI_FORWARD_SCALE from "mkl_dfti.h" */
    public final static int FORWARD_SCALE = 4;

    /** Constant for DFTI_BACKWARD_SCALE from "mkl_dfti.h" */
    public final static int BACKWARD_SCALE = 5;

    /** Constant for DFTI_PLACEMENT from "mkl_dfti.h" */
    public final static int PLACEMENT = 11;

    /** Constant for DFTI_COMPLEX_STORAGE from "mkl_dfti.h" */
    public final static int COMPLEX_STORAGE = 8;

    /** Constant for DFTI_REAL_STORAGE from "mkl_dfti.h" */
    public final static int REAL_STORAGE = 9;

    /** Constant for DFTI_CONJUGATE_EVEN_STORAGE from "mkl_dfti.h" */
    public final static int CONJUGATE_EVEN_STORAGE = 10;

    /** Constant for DFTI_DESCRIPTOR_NAME from "mkl_dfti.h" */
    public final static int DESCRIPTOR_NAME = 20;

    /** Constant for DFTI_PACKED_FORMAT from "mkl_dfti.h" */
    public final static int PACKED_FORMAT = 21;

    /** Constant for DFTI_NUMBER_OF_USER_THREADS from "mkl_dfti.h" */
    public final static int NUMBER_OF_USER_THREADS = 26;

    /** Constant for DFTI_INPUT_DISTANCE from "mkl_dfti.h" */
    public final static int INPUT_DISTANCE = 14;

    /** Constant for DFTI_OUTPUT_DISTANCE from "mkl_dfti.h" */
    public final static int OUTPUT_DISTANCE = 15;

    /** Constant for DFTI_INPUT_STRIDES from "mkl_dfti.h" */
    public final static int INPUT_STRIDES = 12;

    /** Constant for DFTI_OUTPUT_STRIDES from "mkl_dfti.h" */
    public final static int OUTPUT_STRIDES = 13;

    /** Constant for DFTI_ORDERING from "mkl_dfti.h" */
    public final static int ORDERING = 18;

    /** Constant for DFTI_TRANSPOSE from "mkl_dfti.h" */
    public final static int TRANSPOSE = 19;

    /** Constant for DFTI_COMMIT_STATUS from "mkl_dfti.h" */
    public final static int COMMIT_STATUS = 22;

    /** Constant for DFTI_VERSION from "mkl_dfti.h" */
    public final static int VERSION = 23;

    //
    // Java counterparts for the DFTI configuration values:
    //

    /** Constant for DFTI_SINGLE from "mkl_dfti.h" */
    public final static int SINGLE = 35;

    /** Constant for DFTI_DOUBLE from "mkl_dfti.h" */
    public final static int DOUBLE = 36;

    /** Constant for DFTI_COMPLEX from "mkl_dfti.h" */
    public final static int COMPLEX = 32;

    /** Constant for DFTI_REAL from "mkl_dfti.h" */
    public final static int REAL = 33;

    /** Constant for DFTI_INPLACE from "mkl_dfti.h" */
    public final static int INPLACE = 43;

    /** Constant for DFTI_NOT_INPLACE from "mkl_dfti.h" */
    public final static int NOT_INPLACE = 44;

    /** Constant for DFTI_COMPLEX_COMPLEX from "mkl_dfti.h" */
    public final static int COMPLEX_COMPLEX = 39;

    /** Constant for DFTI_REAL_REAL from "mkl_dfti.h" */
    public final static int REAL_REAL = 42;

    /** Constant for DFTI_COMPLEX_REAL from "mkl_dfti.h" */
    public final static int COMPLEX_REAL = 40;

    /** Constant for DFTI_REAL_COMPLEX from "mkl_dfti.h" */
    public final static int REAL_COMPLEX = 41;

    /** Constant for DFTI_COMMITTED from "mkl_dfti.h" */
    public final static int COMMITTED = 30;

    /** Constant for DFTI_UNCOMMITTED from "mkl_dfti.h" */
    public final static int UNCOMMITTED = 31;

    /** Constant for DFTI_ORDERED from "mkl_dfti.h" */
    public final static int ORDERED = 48;

    /** Constant for DFTI_BACKWARD_SCRAMBLED from "mkl_dfti.h" */
    public final static int BACKWARD_SCRAMBLED = 49;

    /** Constant for DFTI_NONE from "mkl_dfti.h" */
    public final static int NONE = 53;

    /** Constant for DFTI_CCS_FORMAT from "mkl_dfti.h" */
    public final static int CCS_FORMAT = 54;

    /** Constant for DFTI_PACK_FORMAT from "mkl_dfti.h" */
    public final static int PACK_FORMAT = 55;

    /** Constant for DFTI_PERM_FORMAT from "mkl_dfti.h" */
    public final static int PERM_FORMAT = 56;

    /** Constant for DFTI_CCE_FORMAT from "mkl_dfti.h" */
    public final static int CCE_FORMAT = 57;

    /** Constant for DFTI_VERSION_LENGTH from "mkl_dfti.h" */
    public final static int VERSION_LENGTH = 198;

    /** Constant for DFTI_MAX_NAME_LENGTH from "mkl_dfti.h" */
    public final static int MAX_NAME_LENGTH = 10;

    /** Constant for DFTI_MAX_MESSAGE_LENGTH from "mkl_dfti.h" */
    public final static int MAX_MESSAGE_LENGTH = 40;

    //
    // Java counterparts for DFTI predefined error classes:
    //

    /** Constant for DFTI_NO_ERROR from "mkl_dfti.h" */
    public final static int NO_ERROR = 0;

    /** Constant for DFTI_MEMORY_ERROR from "mkl_dfti.h" */
    public final static int MEMORY_ERROR = 1;

    /** Constant for DFTI_INVALID_CONFIGURATION from "mkl_dfti.h" */
    public final static int INVALID_CONFIGURATION = 2;

    /** Constant for DFTI_INCONSISTENT_CONFIGURATION from "mkl_dfti.h" */
    public final static int INCONSISTENT_CONFIGURATION = 3;

    /** Constant for DFTI_NUMBER_OF_THREADS_ERROR from "mkl_dfti.h" */
    public final static int NUMBER_OF_THREADS_ERROR = 8;

    /** Constant for DFTI_MULTITHREADED_ERROR from "mkl_dfti.h" */
    public final static int MULTITHREADED_ERROR = 4;

    /** Constant for DFTI_BAD_DESCRIPTOR from "mkl_dfti.h" */
    public final static int BAD_DESCRIPTOR = 5;

    /** Constant for DFTI_UNIMPLEMENTED from "mkl_dfti.h" */
    public final static int UNIMPLEMENTED = 6;

    /** Constant for DFTI_MKL_INTERNAL_ERROR from "mkl_dfti.h" */
    public final static int MKL_INTERNAL_ERROR = 7;

    /** Constant for DFTI_1D_LENGTH_EXCEEDS_INT32 from "mkl_dfti.h" */
    public final static int LENGTH_EXCEEDS_INT32 = 9;
}
