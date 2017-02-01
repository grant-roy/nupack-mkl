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
 * Example of using MKL DFTI: single precision complex type,
 * 1-dimensional case, not-inplace transform.
 *
 * <p>Small array is transformed; then back Fourier transform
 * is computed. The resulting array is compared to the original
 * one with reasonable tolerance. If the arrays are not equal,
 * the program exits with error status.
 *
 * @see DFTI
 */
public final class dfti_c1 {
    /**
     * Incarnation prohibited.
     */
    private dfti_c1() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "dfti_c1";

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
        final int forward_domain = DFTI.COMPLEX;
        final int dimension=1, length=6;

        //
        // The data arrays, allow twice space for complex data
        //
        float x_normal[]      = new float [2 * length];
        float x_transformed[] = new float [2 * length];

        //
        // Create new DFTI descriptor and adjust for not-inplace mode
        //
        DFTI.DESCRIPTOR_HANDLE desc_handle =
            DFTI.CreateDescriptor(precision,forward_domain,dimension,length);

        //
        // Make a copy of the DFTI descriptor
        //
        /* OOPS: the copy function is not implemented!
        DFTI.DESCRIPTOR_HANDLE copy_handle = DFTI.CopyDescriptor(desc_handle);
        */

        //
        // Setup the transform parameters
        //
        DFTI.SetValue(desc_handle,DFTI.PLACEMENT,DFTI.NOT_INPLACE);

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
        Integer len = (Integer) DFTI.GetValue(desc_handle,DFTI.LENGTHS);
        System.out.println("Dimension: " + dim);
        System.out.println("Length: " + len);

        //
        // Setup the scale factor
        //
        int transform_size = length;
        float scale_factor = 1.0f / transform_size;
        DFTI.SetValue(desc_handle,DFTI.BACKWARD_SCALE,scale_factor);

        //
        // Try floating-point and GetValue function
        //
        Float backward_scale = (Float) DFTI.GetValue(desc_handle,DFTI.BACKWARD_SCALE);
        System.out.println("Backward transform scale: " + backward_scale);

        //
        // Commit the descriptor
        //
        DFTI.CommitDescriptor(desc_handle);

        //
        // Initialize the data array
        //
        System.out.println("Initial data:");
        for (int i=0; i<length; i++) {
            x_normal[2*i    ] = i;
            x_normal[2*i + 1] = 0;
            System.out.print("\t(" + i + ",0)");
        }
        System.out.println();

        //
        // Forward, then backward transform
        //
        DFTI.ComputeForward(desc_handle,x_normal,x_transformed);
        DFTI.ComputeBackward(desc_handle,x_transformed,x_normal);

        //
        // Check the data array
        //
        System.out.println("Resulting data:");
        boolean fault = false;
        for (int i=0; i<length; i++) {
            System.out.print("\t("+x_normal[2*i]+","+x_normal[2*i+1]+")");
            if (Math.abs(x_normal[2*i]-i)+Math.abs(x_normal[2*i+1]) > tolerance)
                fault = true;
        }
        System.out.println();

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
