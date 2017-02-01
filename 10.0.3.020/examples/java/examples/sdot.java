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
 * Example: calling MKL function sdot() via CBLAS interface.
 * Small arrays whose elements are small integer values are
 * processed. The result is compared to the expected value
 * which is also integer. If the result is not expected,
 * the program exits with error status.
 *
 * @see CBLAS
 */
public final class sdot {
    /** Incarnation prohibited. */
    private sdot() {}

    /** No command-line options. */
    public static void main(String[] args) {
        //
        // Prepare the matrices and other parameters
        //
        int order = CBLAS.ORDER.RowMajor;
        int TransA = CBLAS.TRANSPOSE.NoTrans;
        int N=3, incX=1, incY=1;
        float[] X = new float[] {1,2,3};
        float[] Y = new float[] {2,1,-1};
        float result, expected=1;
        //
        // Print the parameters
        //
        printVector("Vector X",X,N);
        printVector("Vector Y",Y,N);
        System.out.println("expected=" + string(expected));
        //
        // Compute the function
        //
        result = CBLAS.sdot(N,X,incX,Y,incY);
        //
        // Print the result
        //
        System.out.println("result=" + string(result));
        //
        // Check the result:
        //
        boolean error = !(result == expected);
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
    private static void printVector(String prompt, float[] X, int N) {
        System.out.println(prompt);
        for (int n=0; n<N; n++)
            System.out.print("\t" + string(X[n]));
        System.out.println();
    }

    /** Shorter string for real number. */
    private static String string(float re) {
        String s="";
        if (re == (int)re)
            s += (int)re;
        else
            s += re;
        return s;
    }
}
