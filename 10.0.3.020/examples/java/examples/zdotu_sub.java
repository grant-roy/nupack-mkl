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
 * Example: calling MKL function zdotu_sub() via CBLAS interface.
 * Small arrays whose elements are small integer values are
 * processed. The result is compared to the expected value
 * which is also integer. If the result is not expected,
 * the program exits with error status.
 *
 * @see CBLAS
 */
public final class zdotu_sub {
    /** Incarnation prohibited. */
    private zdotu_sub() {}

    /** No command-line options. */
    public static void main(String[] args) {
        //
        // Prepare the matrices and other parameters
        //
        int order = CBLAS.ORDER.RowMajor;
        int TransA = CBLAS.TRANSPOSE.NoTrans;
        int N=3, incX=1, incY=1;
        double[] X = new double[] {1,1, 2,0, 3,-1};
        double[] Y = new double[] {2,0, 1,0, -1,0};
        double[] expected = new double[] {1,3};
        double[] dotu = new double[2];
        //
        // Print the parameters
        //
        printVector("Vector X",X,N);
        printVector("Vector Y",Y,N);
        System.out.println("expected=" + string(expected[0],expected[1]));
        //
        // Compute the function
        //
        CBLAS.zdotu_sub(N,X,incX,Y,incY,dotu);
        //
        // Print the result
        //
        System.out.println("result=" + string(dotu[0],dotu[1]));
        //
        // Check the result:
        //
        boolean error = !(dotu[0]==expected[0] && dotu[1]==expected[1]);
        if (error)
            System.out.println("ERROR: result is unexpected!");
        //
        // Print summary and exit
        //
        if (error) {
            System.out.println("TEST FAILED");
            System.exit(1);
        }
        System.out.println("TEST PASSED");
    }

    /** Print the vector X assuming raw-major order of elements. */
    private static void printVector(String prompt, double[] X, int N) {
        System.out.println(prompt);
        for (int n=0; n<N; n++)
            System.out.print("\t" + string(X[2*n],X[2*n+1]));
        System.out.println();
    }

    /** Shorter string for complex number. */
    private static String string(double re, double im) {
        String s="(";
        if (re == (int)re)
            s += (int)re;
        else
            s += re;
        s += ",";
        if (im == (int)im)
            s += (int)im;
        else
            s += im;
        s += ")";
        return s;
    }
}
