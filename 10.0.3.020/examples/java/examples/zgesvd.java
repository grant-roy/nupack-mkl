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

public final class zgesvd {
    /**
     * Incarnation prohibited.
     */
    private zgesvd() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "zgesvd";

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
        final int m = 5;
        final int n = 3;
        final int minmn = Math.min(m,n);

        int i, j, info;

        double[] A = new double[] { -2,0,  2,0,  6,0,  1,0, -2,0,
                                  -8,0, -5,0,  0,0,  0,0, 11,0,
                                   1,0,  6,0,  2,0,  4,0,  0,0 };

        double[] s = new double[minmn];
        double[] u = new double[2*m*m];
        double[] vt = new double[2*n*n];
        double[] sd = new double[minmn-1];

		double[] AA = new double[2*m*n];
        for(i=0;i<2*m*n;i++) {
			AA[i] = A[i];
        }

        System.out.println("The matrix A:");
        for(i=0;i<m;i++) {
            for(j=0;j<n;j++) {
                System.out.print(" (" + A[2*(i+j*m)] + "," + A[2*(i+j*m)+1] + ")");
            }
	        System.out.println("");
        }


        info = LAPACK.zgesvd(LAPACK.JOB.JobN,LAPACK.JOB.JobN,m,n,A,m,s,null,1,null,1,sd);


        System.out.print("s on exit:");
        for(i=0;i<minmn;i++) {
            System.out.print(" " + s[i]);
        }
        System.out.println("");

        System.out.println("info on exit: " + info);

        info = LAPACK.zgesvd(LAPACK.JOB.JobN,LAPACK.JOB.JobS,m,n,A,m,s,null,1,vt,minmn,sd);

        System.out.print("s on exit:");
        for(i=0;i<minmn;i++) {
            System.out.print(" " + s[i]);
        }
        System.out.println("");

        System.out.println("info on exit: " + info);

        return false;
    }
}
