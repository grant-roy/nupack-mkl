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

public final class vdrngcauchy {
    /**
     * Incarnation prohibited.
     */
    private vdrngcauchy() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "vdrngcauchy";

    /**
     * No command-line options.
     */
    public static void main(String[] args) {
        try {
            test();
        } catch (Exception statusException) {
            statusException.printStackTrace(System.out);
            System.out.println("\nTEST FAILED");
            System.exit(1);
        }
        System.out.println("\nTEST PASSED");
    }

    /**
     * Demonstrate the VSL functions and test them.
     */
    public static void test() throws Exception
    {
		// Parameters
        final int SEED = 777;
        final int BRNG = VSL.BRNG_R250;
        final int METHOD = 0;
        final int N = 1000;
        final int NN = 10;

		double r[] = new double [N];
		int i;
		final double a=0.0,beta=1.0;

		/***** Initialize *****/
		VSLStreamStatePtr stream = new VSLStreamStatePtr();
		int errcode = VSL.vslNewStream(stream,BRNG, SEED);

		/***** Call RNG *****/
		errcode = VSL.vdRngCauchy( METHOD, stream, N, r, a, beta );
		ErrCheck.CheckVslError( errcode );

		/***** Printing results *****/
		System.out.println("");
		System.out.println("Sample of vdRngCauchy.");
		System.out.println("----------------------\n");
		System.out.println("Parameters:");
		System.out.println("    a=" + a);
		System.out.println("    beta=" + beta + "\n");

		System.out.println("Results (first 10 of 1000):");
		System.out.println("---------------------------");
		for(i=0;i<NN;i++) {
			System.out.println("r[" + i + "]=" + r[i]);
		}

		System.out.println("");

		/***** Deinitialize *****/
		errcode = VSL.vslDeleteStream( stream );
		ErrCheck.CheckVslError( errcode );

	}
}