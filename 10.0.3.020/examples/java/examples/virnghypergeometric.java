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

public final class virnghypergeometric {
    /**
     * Incarnation prohibited.
     */
    private virnghypergeometric() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "virnghypergeometric";

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

		int r[] = new int [N];
		int i;
		int l=100,ss=10,m=30;

		double tM,tD,tQ,tD2;
		double sM,sD;
		double sum, sum2;
		double n,s;
		double DeltaM,DeltaD;
		double K, L2, L3, L4, L5, L6, KL, KL4, S2, S3, S4, M2, M3, M4;

		/***** Initialize *****/
		VSLStreamStatePtr stream = new VSLStreamStatePtr();
		int errcode = VSL.vslNewStream(stream,BRNG, SEED);

		/***** Call RNG *****/
		errcode = VSL.viRngHypergeometric( METHOD, stream, N, r, l, ss, m );
		ErrCheck.CheckVslError( errcode );

		/***** Theoretical moments *****/
		K = (l-1)*(l-2)*(l-3);
		L2 = l*l;
		L3 = L2*l;
		L4 = L2*L2;
		L5 = L3*L2;
		L6 = L3*L3;
		KL = K*l;
		KL4 = K*L4;
		S2 = ss*ss;
		S3 = S2*ss;
		S4 = S2*S2;
		M2 = m*m;
		M3 = M2*m;
		M4 = M2*M2;

		tM=(double)m*(double)ss/(double)l;
		tD=(double)(m*ss*(l-m)*(l-ss))/(double)(l*l*(l-1));
		tQ=( (3*l+18)    *S4/KL4 - (6*L2+36*l)  *S3/KL4 + (3*L3+24*L2)   *S2/KL4 - 6        *ss/KL  ) * M4 +
		   ( (-6*L2-36*l)*S4/KL4 + (12*L3+72*L2)*S3/KL4 - (6*L4+38*L3)   *S2/KL4 + 12       *ss/K   ) * M3 +
		   ( (3*L3+24*L2)*S4/KL4 - (6*L4+48*L3) *S3/KL4 + (31*L4+3*L5+L3)*S2/KL4 - (L4+7*L5)*ss/KL4 ) * M2 +
		   ( -6          *S4/KL  + 12           *S3/K   - (4*L4+7*L5)    *S2/KL4 + (L6+L5)  *ss/KL4 ) * m;

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
		System.out.println("Sample of viRngHypergeometric.");
		System.out.println("------------------------------");
		System.out.println("");
		System.out.println("Parameters:");
		System.out.println("    l=" + l);
		System.out.println("    s=" + ss);
		System.out.println("    m=" + m);
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
