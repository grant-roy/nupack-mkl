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

public final class vsrngrayleigh {
    /**
     * Incarnation prohibited.
     */
    private vsrngrayleigh() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "vsrngrayleigh";

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
        final int BRNG = VSL.BRNG_MCG31;
        final int METHOD = 0;
        final int N = 1000;
        final int NN = 10;
        final float _PI = 3.141592653589793238462643383279502884197f;

		float r[] = new float [N];
		int i;
		final float a=0.0f,beta=1.0f;

		float tM,tD,tQ,tD2;
		float sM,sD;
		float sum, sum2;
		float n,s;
		float DeltaM,DeltaD;

		/***** Initialize *****/
		VSLStreamStatePtr stream = new VSLStreamStatePtr();
		int errcode = VSL.vslNewStream(stream,BRNG, SEED);

		/***** Call RNG *****/
		errcode = VSL.vsRngRayleigh( METHOD, stream, N, r, a, beta );
		ErrCheck.CheckVslError( errcode );

		/***** Theoretical moments *****/
		tM=a+beta*(float)Math.sqrt(_PI)*0.5f;
		tD=(1.0f-_PI*0.25f)*beta*beta;
		tQ=(2.0f-0.1875f*_PI*_PI)*beta*beta*beta*beta;

		/***** Sample moments *****/
		sum=0.0f;
		sum2=0.0f;
		for(i=0;i<N;i++) {
			sum+=(float)r[i];
			sum2+=(float)r[i]*(float)r[i];
		}
		sM=sum/((float)N);
		sD=sum2/(float)N-(sM*sM);

		/***** Comparison of theoretical and sample moments *****/
		n=(float)N;
		tD2=tD*tD;
		s=((tQ-tD2)/n)-(2*(tQ-2*tD2)/(n*n))+((tQ-3*tD2)/(n*n*n));

		DeltaM=(tM-sM)/(float)Math.sqrt(tD/n);
		DeltaD=(tD-sD)/(float)Math.sqrt(s);

		/***** Printing results *****/
		System.out.println("");
		System.out.println("Sample of vsRngRayleigh.");
		System.out.println("------------------------");
		System.out.println("");
		System.out.println("Parameters:");
		System.out.println("    a=" + a);
		System.out.println("    beta=" + beta);
		System.out.println("");

		System.out.println("Results (first 10 of 1000):");
		System.out.println("---------------------------");
		for(i=0;i<NN;i++) {
			System.out.println("r[" + i + "]=" + r[i]);
		}

		System.out.println("");
		if(DeltaM>3.0f || DeltaD>3.0f) {
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
