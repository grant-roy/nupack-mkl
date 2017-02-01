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
 * Example: calling MKL function zgemv() via CBLAS interface.
 * Small arrays whose elements are small integer values are
 * processed. The initial arrays are especially selected so
 * that the expected result of the operation is zero array.
 * If the result is not zero, the program exits with error.
 *
 * @see CBLAS
 */
public final class zgemv {
    /** Incarnation prohibited. */
    private zgemv() {}

    /** No command-line options. */
    public static void main(String[] args) {
        //
        // Prepare the matrices and other parameters
        //
        int order = CBLAS.ORDER.RowMajor;
        int TransA = CBLAS.TRANSPOSE.NoTrans;
        int M=2, N=3;
        int lda=N, incX=1, incY=1;
        double[] A = new double[] {1,0, 2,0, 3,0,
                                   4,0, 5,0, 6,0};
        double[] X = new double[] {1,-1, 2,0, -1,1};
        double[] Y = new double[] {2,2, 8,2};
        double[] alpha = new double[] {1,0};
        double[] beta = new double[] {-1,0};
        //
        // Print the parameters
        //
        System.out.println("alpha=" + string(alpha[0],alpha[1]));
        System.out.println("beta=" + string(beta[0],beta[1]));
        printMatrix("Matrix A",A,M,N);
        printMatrix("Vector X",X,1,N);
        printMatrix("Initial Y",Y,1,M);
        //
        // Compute the function
        //
        CBLAS.zgemv(order,TransA,M,N,alpha,A,lda,X,incX,beta,Y,incY);
        //
        // Print the result
        //
        printMatrix("Resulting Y",Y,1,M);
        //
        // Check the result:
        //
        boolean error=false;
        for (int m=0; m<M; m++)
            if (Y[m] != 0)
                error=true;
        if (error)
            System.out.println("ERROR: resulting Y must be zero!");
        //
        // Print summary and exit
        //
        if (error) {
            System.out.println("TEST FAILED");
            System.exit(1);
        }
        System.out.println("TEST PASSED");
    }

    /** Print the matrix X assuming raw-major order of elements. */
    private static void printMatrix(String prompt, double[] X, int I, int J) {
        System.out.println(prompt);
        for (int i=0; i<I; i++) {
            for (int j=0; j<J; j++)
                System.out.print("\t" + string(X[2*(i*J+j)],X[2*(i*J+j)+1]));
            System.out.println();
        }
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
