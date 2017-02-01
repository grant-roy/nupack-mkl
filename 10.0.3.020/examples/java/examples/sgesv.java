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

public final class sgesv {
    /**
     * Incarnation prohibited.
     */
    private sgesv() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "sgesv";

    /**
     * No command-line options.
     */
    public static void main(String[] args) {
        boolean fault;
        try {
            fault = test();
        } catch (Exception statusException) {
            fault = true;
            statusException.printStackTrace(System.out);
            System.out.println("\nTEST FAILED");
            System.exit(1);
        }
        System.out.println("\nTEST PASSED");
    }

    /**
     * Demonstrate the LAPACK functions and test them.
     * @return true - If the test has failed.
     */
    public static boolean test()
    {
        // Parameters
        final int n = 5;
        final int nrhs = 1;

        int i, j, info;

        float[] A = new float[] { 0, 1, 7, 4, 5,
                                  2, 0, 6, 6, 9,
                                  3, 5, 8, 0, 0,
                                  5, 6, 0, 3, 0,
                                  4, 6, 5, 9, 8 };
        float[] B = new float[] {14, 18, 26, 22, 22};
        int[] ipiv = new int[n];

        System.out.println("The matrix A:");
        for(i=0;i<n;i++) {
            for(j=0;j<n;j++) {
                System.out.print(" " + A[i+j*n]);
            }
	        System.out.println("");
        }
        System.out.print("The matrix B:");
        for(i=0;i<n;i++) {
            System.out.print(" " + B[i]);
        }
        System.out.println("");


        info = LAPACK.sgesv(n,nrhs,A,n,ipiv,B,n);


        System.out.print("B on exit:");
        for(i=0;i<n;i++) {
            System.out.print(" " + B[i]);
        }
        System.out.println("");

        System.out.print("ipiv on exit:");
        for(i=0;i<n;i++) {
            System.out.print(" " + ipiv[i]);
        }
        System.out.println("");

        System.out.print("info on exit: " + info);

        return false;
    }
}
