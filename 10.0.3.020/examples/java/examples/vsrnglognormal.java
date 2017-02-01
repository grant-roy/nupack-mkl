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

public final class vsrnglognormal {
    /**
     * Incarnation prohibited.
     */
    private vsrnglognormal() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "vsrnglognormal";

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
        final int SEED = 1;
        final int BRNG = VSL.BRNG_MCG31;
        final int METHOD = 0;
        final int N = 1000;
        final int NN = 10;

		float r[] = new float [N];
		int i;
		final float a=0.0f,sigma=1.0f,b=0.0f,beta=1.0f;

		float tM,tD,tQ,tD2;
		float sM,sD;
		float sum, sum2;
		float n,s;
		float DeltaM,DeltaD;

		/***** Initialize *****/
		VSLStreamStatePtr stream = new VSLStreamStatePtr();
		int errcode = VSL.vslNewStream(stream,BRNG, SEED);

		/***** Call RNG *****/
		errcode = VSL.vsRngLognormal( METHOD, stream, N, r, a, sigma, b, beta );
		ErrCheck.CheckVslError( errcode );

		/***** Theoretical moments *****/
		tM=b+beta*(float)Math.exp(a+sigma*sigma*0.5);
		tD=beta*beta*(float)Math.exp(2.0f*a+sigma*sigma)*((float)Math.exp(sigma*sigma)-1.0f);
		tQ=beta*beta*beta*beta*(float)Math.exp(4.0f*a+2.0f*sigma*sigma)*((float)Math.exp(6.0f*sigma*sigma)-4.0f*(float)Math.exp(3.0f*sigma*sigma)+6.0f*(float)Math.exp(sigma*sigma)-3.0f);

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
		System.out.println("Sample of vsRngLognormal.");
		System.out.println("-------------------------");
		System.out.println("");
		System.out.println("Parameters:");
		System.out.println("    a=" + a);
		System.out.println("    sigma=" + sigma);
		System.out.println("    b=" + b);
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
