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

public final class zgeev {
    /**
     * Incarnation prohibited.
     */
    private zgeev() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "zgeev";

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

        int i, j, info;

        double[] A = new double[] { -6,0,   5,0,  8,1,  -1,2, -4,3,
                                    -6,0,   2,0,  6,-1, -4,0, -1,-3,
                                     5,1,  -4,4, -3,1,   3,-4, 4,0,
                                     1,-1, 7,-1, -6,-1,  8,1, -4,-1,
                                    -3,0,  2,-2,  0,2,  -2,4, -6,-2 };

        double[] W = new double[10];
        double[] Vl = new double[2*n*n];
        double[] Vr = new double[2*n*n];

        System.out.println("The matrix A:");
        for(i=0;i<n;i++) {
            for(j=0;j<n;j++) {
                System.out.print(" (" + A[2*(i+j*n)] + "," + A[2*(i+j*n)+1] + ")");
            }
	        System.out.println("");
        }

        info = LAPACK.zgeev(LAPACK.JOB.JobV,LAPACK.JOB.JobV,n,A,n,W,Vl,n,Vr,n);


        System.out.println("A on exit:");
        for(i=0;i<n;i++) {
            for(j=0;j<n;j++) {
                System.out.print(" (" + A[2*(i+j*n)] + "," + A[2*(i+j*n)+1] + ")");
            }
	        System.out.println("");
        }
        System.out.print("W on exit:");
        for(i=0;i<2*n;i+=2) {
            System.out.print(" (" + W[i] + "," + W[i+1] + ")");
        }
        System.out.println("");

        System.out.print("info on exit: " + info);

        return false;
    }
}
