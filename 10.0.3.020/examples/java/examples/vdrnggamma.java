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

public final class vdrnggamma {
    /**
     * Incarnation prohibited.
     */
    private vdrnggamma() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "vdrnggamma";

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
        final int SEED = 7777777;
        final int BRNG = VSL.BRNG_MT19937;
        final int METHOD = 0;
        final int N = 1000;
        final int NN = 10;

		double r[] = new double [N];
		int i;
		final double alpha = 2.0, a=0.0, beta=1.0;

		double tM,tD,tQ,tD2;
		double sM,sD;
		double sum, sum2;
		double n,s;
		double DeltaM,DeltaD;

		/***** Initialize *****/
		VSLStreamStatePtr stream = new VSLStreamStatePtr();
		int errcode = VSL.vslNewStream(stream,BRNG, SEED);

		/***** Call RNG *****/
		errcode = VSL.vdRngGamma( METHOD, stream, N, r, alpha, a, beta );
		ErrCheck.CheckVslError( errcode );

		/***** Theoretical moments *****/
		tM=a+beta * alpha;
		tD=beta * beta * alpha;
		tQ=beta * beta * beta * beta * 3 * alpha * (alpha+2);

		/***** Sample moments *****/
		sum=0.0;
		sum2=0.0;
		for(i=0;i<N;i++) {
			sum+=(double)r[i];
			sum2+=(double)r[i]*(double)r[i];
		}
		sM=sum/((double)N);
		sD=sum2/(double)N-(sM*sM);

		/***** Comparison of theoretical and sample moments *****/
		n=(double)N;
		tD2=tD*tD;
		s=((tQ-tD2)/n)-(2*(tQ-2*tD2)/(n*n))+((tQ-3*tD2)/(n*n*n));

		DeltaM=(tM-sM)/Math.sqrt(tD/n);
		DeltaD=(tD-sD)/Math.sqrt(s);

		/***** Printing results *****/
		System.out.println("");
		System.out.println("Sample of vdRngGamma.");
		System.out.println("----------------------");
		System.out.println("");
		System.out.println("Parameters:");
		System.out.println("    alpha=" + alpha);
		System.out.println("    a=" + a);
		System.out.println("    beta=" + beta);
		System.out.println("");

		System.out.println("Results (first 10 of 1000):");
		System.out.println("---------------------------");
		for(i=0;i<NN;i++) {
			System.out.println("r[" + i + "]=" + r[i]);
		}

		System.out.println("");
		if(DeltaM>3.0 || DeltaD>3.0) {
			System.out.println("Error: sample moments (mean=" + sM + ", variance=" + sD + ") are disagreed with theory (mean=" + tM + ", variance=" + tD + ").");
		} else {
			System.out.println("Sample moments (mean=" + sM + ", variance=" + sD + ") are agreed with theory (mean=" + tM + ", variance=" + tD + ").");
		}

		System.out.println("");

		/***** Deinitialize *****/
		errcode = VSL.vslDeleteStream( stream );
		ErrCheck.CheckVslError( errcode );

	}
}
