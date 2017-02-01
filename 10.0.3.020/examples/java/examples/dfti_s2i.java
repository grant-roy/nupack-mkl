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

import com.intel.mkl.*;

/**
 * Example of using MKL DFTI: single precision real type,
 * 2-dimensional case, inplace transform.
 *
 * <p>Small array is transformed; then back Fourier transform
 * is computed. The resulting array is compared to the original
 * one with reasonable tolerance. If the arrays are not equal,
 * the program exits with error status.
 *
 * @see DFTI
 */
public final class dfti_s2i {
    /**
     * Incarnation prohibited.
     */
    private dfti_s2i() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "dfti_s2i";

    /**
     * No command-line options.
     */
    public static void main(String[] args) {
        boolean fault;
        try {
            fault = test();
        } catch (DFTI.StatusException statusException) {
            fault = true;
            statusException.printStackTrace(System.out);
        }
        if (fault) {
            System.out.println("\nTEST FAILED");
            System.exit(1);
        }
        System.out.println("\nTEST PASSED");
    }

    /**
     * Demonstrate the DFTI functions and test them.
     * @return true - If the test has failed.
     */
    public static boolean test() throws DFTI.StatusException
    {
        //
        // Parameters
        //
        final double tolerance = 1.0e-6; // absolute error
        final int precision = DFTI.SINGLE;
        final int forward_domain = DFTI.REAL;
        final int dimension = 2;
        final int[] lengths = new int[] {3,4}; // matrix 3x4, row-major

        //
        // The data to be transformed
        //
        int array_size = lengths[0] * lengths[1];
        float x_inout[] = new float[ array_size ];

        //
        // Create new DFTI descriptor
        //
        DFTI.DESCRIPTOR_HANDLE desc_handle =
            DFTI.CreateDescriptor(precision,forward_domain,dimension,lengths);

        //
        // Make a copy of the DFTI descriptor
        //
        /* OOPS: the copy function is not implemented!
        DFTI.DESCRIPTOR_HANDLE copy_handle = DFTI.CopyDescriptor(desc_handle);
        */

        //
        // Try string SetValue and GetValue functions
        //
        DFTI.SetValue(desc_handle,DFTI.DESCRIPTOR_NAME,test_name);
        String desc_name = (String) DFTI.GetValue(desc_handle,DFTI.DESCRIPTOR_NAME);
        String version   = (String) DFTI.GetValue(desc_handle,DFTI.VERSION);
        System.out.println(version + "\n");
        System.out.println("Test name: " + desc_name);

        //
        // Try integer GetValue functions
        //
        Integer dim = (Integer) DFTI.GetValue(desc_handle,DFTI.DIMENSION);
        int[]   len = (int[])   DFTI.GetValue(desc_handle,DFTI.LENGTHS);
        System.out.println("Dimension: " + dim);
        System.out.print("Lengths (reversed): {");
        for (int i=0; i<len.length; i++)
            System.out.print((i==0? "": ",") + len[i]);
        System.out.println("}");

        //
        // Setup the transform parameters
        //
        DFTI.SetValue(desc_handle,DFTI.PLACEMENT,DFTI.INPLACE);
        DFTI.SetValue(desc_handle,DFTI.PACKED_FORMAT,DFTI.PACK_FORMAT);

        //
        // You do not need to change the default strides if PACK format.
        // However, let's setup the strides using their default values
        // to try the GetValue/SetValue functions for int[] parameter.
        //
        int[] input_strides  = (int[]) DFTI.GetValue(desc_handle,DFTI.INPUT_STRIDES);
        int[] output_strides = (int[]) DFTI.GetValue(desc_handle,DFTI.OUTPUT_STRIDES);
        if (input_strides.length!=3 || output_strides.length!=3)
            throw new Error("input_strides.length!=3 || output_strides.length!=3");
        System.out.println("Input strides: {"+
            input_strides[0]+","+input_strides[1]+","+input_strides[2]+"}");
        System.out.println("Output strides: {"+
            output_strides[0]+","+output_strides[1]+","+output_strides[2]+"}");
        DFTI.SetValue(desc_handle,DFTI.INPUT_STRIDES,input_strides);
        DFTI.SetValue(desc_handle,DFTI.OUTPUT_STRIDES,output_strides);

        //
        // Setup the scale factor
        //
        int transform_size = lengths[0] * lengths[1];
        float scale_factor = 1.0f / transform_size;
        DFTI.SetValue(desc_handle,DFTI.BACKWARD_SCALE,scale_factor);

        //
        // Try floating-point GetValue function
        //
        Float forward_scale = (Float) DFTI.GetValue(desc_handle,DFTI.FORWARD_SCALE);
        Float backward_scale = (Float) DFTI.GetValue(desc_handle,DFTI.BACKWARD_SCALE);
        System.out.println("Forward transform scale: " + forward_scale);
        System.out.println("Backward transform scale: " + backward_scale);

        //
        // Commit the descriptor
        //
        DFTI.CommitDescriptor(desc_handle);

        //
        // Initialize the data array
        //
        System.out.println("Initial data:");
        for (int i=0; i<lengths[0]; i++)
        {
            for (int j=0; j<lengths[1]; j++)
            {
                int k = i*lengths[1] + j; // row-major
                x_inout[k] = k;
                System.out.print("\t" + k);
            }
            System.out.println();
        }

        //
        // Forward, then backward transform
        //
        DFTI.ComputeForward(desc_handle,x_inout);
        DFTI.ComputeBackward(desc_handle,x_inout);

        //
        // Check the data array
        //
        System.out.println("Resulting data:");
        boolean fault = false;
        for (int i=0; i<lengths[0]; i++) {
            for (int j=0; j<lengths[1]; j++) {
                int k = i*lengths[1] + j; // row-major
                System.out.print("\t" + x_inout[k]);
                if (Math.abs(x_inout[k]-k) > tolerance)
                    fault = true;
            }
            System.out.println();
        }

        //
        // You do not necessarily need to free the descriptor,
        // because the descriptor would be garbage collected.
        // However, let's free the descriptor explicitly to try
        // the destructor method.
        //
        DFTI.FreeDescriptor(desc_handle);

        return fault;
    }
}
