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
 * Example: calling MKL function sgemm() via CBLAS interface.
 * Small matrices whose elements are small integer values are
 * processed. The initial matrices are especially selected so
 * that the expected result of the operation is zero matrix.
 * If the result is not zero, the program exits with error.
 *
 * @see CBLAS
 */
public final class sgemm {
    /** Incarnation prohibited. */
    private sgemm() {}

    /** No command-line options. */
    public static void main(String[] args) {
        //
        // Prepare the matrices and other parameters
        //
        int Order = CBLAS.ORDER.RowMajor;
        int TransA = CBLAS.TRANSPOSE.NoTrans;
        int TransB = CBLAS.TRANSPOSE.NoTrans;
        int M=2, N=4, K=3;
        int lda=K, ldb=N, ldc=N;
        float[] A = new float[] {1,2,3, 4,5,6};
        float[] B = new float[] {0,1,0,1, 1,0,0,1, 1,0,1,0};
        float[] C = new float[] {5,1,3,3, 11,4,6,9};
        float alpha=1, beta=-1;
        //
        // Print the parameters
        //
        System.out.println("alpha=" + string(alpha));
        System.out.println("beta=" + string(beta));
        printMatrix("Matrix A",A,M,K);
        printMatrix("Matrix B",B,K,N);
        printMatrix("Initial C",C,M,N);
        //
        // Compute the function
        //
        CBLAS.sgemm(Order,TransA,TransB,M,N,K,alpha,A,lda,B,ldb,beta,C,ldc);
        //
        // Print the result
        //
        printMatrix("Resulting C",C,M,N);
        //
        // Check the result:
        //
        boolean error=false;
        for (int m=0; m<M; m++)
            for (int n=0; n<N; n++)
                if (C[m*N+n] != 0)
                    error=true;
        if (error)
            System.out.println("ERROR: resulting C must be zero!");
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
    private static void printMatrix(String prompt, float[] X, int I, int J) {
        System.out.println(prompt);
        for (int i=0; i<I; i++) {
            for (int j=0; j<J; j++)
                System.out.print("\t" + string(X[i*J+j]));
            System.out.println();
        }
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
