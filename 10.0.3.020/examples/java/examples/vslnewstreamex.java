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

public final class vslnewstreamex {
    /**
     * Incarnation prohibited.
     */
    private vslnewstreamex() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "vslnewstreamex";

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
        final int BRNG = VSL.BRNG_MRG32K3A;
        final int METHOD = 0;
        final int N = 1000;
        final int NSEED = 6;

		int seedEx[] = new int [NSEED];
		int r[] = new int [N];
		int rEx[] = new int [N];
		int i, err = 0;

		/***** Initialize seeds *****/
		seedEx[0] = SEED;
		for(i = 1; i < 6; i++){
			seedEx[i] = 1;
		}

		/***** Initialize streams *****/
		VSLStreamStatePtr stream = new VSLStreamStatePtr();
		int errcode = VSL.vslNewStream(stream,BRNG, SEED);
		VSLStreamStatePtr streamEx = new VSLStreamStatePtr();
		errcode = VSL.vslNewStreamEx(streamEx, BRNG, NSEED, seedEx);
		ErrCheck.CheckVslError( errcode );

		/***** Call RNGs *****/
		errcode = VSL.viRngUniformBits( METHOD, stream, N, r );
		ErrCheck.CheckVslError( errcode );
		errcode = VSL.viRngUniformBits( METHOD, streamEx, N, rEx );
		ErrCheck.CheckVslError( errcode );

		/***** Compare results *****/
		for(i = 0; i < N; i++){
			if(r[i] != rEx[i]) err++;
		}

		/***** Printing results *****/
		System.out.println("");
		System.out.println(" Sample of vslNewStreamEx");
		System.out.println(" ------------------------");
		System.out.println("");
		System.out.println(" Parameters:");
		System.out.println("    seed   =   " + SEED);
		System.out.print("    seedEx = {");
		for(i = 0; i < NSEED; i++){
			System.out.print(" " + seedEx[i]);
		}
		System.out.println(" }");
		System.out.println("");


		System.out.println(" Results (first 10 of 1000):");
		System.out.println(" ---------------------------");
		for(i=0;i<10;i++) {
			System.out.println("r[" + i + "]=" + Integer.toHexString(r[i]) + " rEx[" + i + "]=" + Integer.toHexString(rEx[i]));
		}

		System.out.println("");
		if(err > 0) {
			System.out.println("Error: " + err + " values are incorrect!");
		} else {
			System.out.println(" Results of ordinary and extended NewStream functions are identical.");
		}

		System.out.println("");

		/***** Deinitialize *****/
		errcode = VSL.vslDeleteStream( stream );
		ErrCheck.CheckVslError( errcode );
		errcode = VSL.vslDeleteStream( streamEx );
		ErrCheck.CheckVslError( errcode );

	}
}
