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

public final class vslcopystreamstate {
    /**
     * Incarnation prohibited.
     */
    private vslcopystreamstate() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "vslcopystreamstate";

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
        final int NSEED = 6;
        final int METHOD = 0;
        final int N = 1000;

        int seedCpy;
		int seed[] = new int [NSEED];
		int r[] = new int [N];
		int rCpy[] = new int [N];
		int i, err = 0;

		/***** Initialize seeds *****/
		seedCpy = 1;
		seed[0] = SEED;
		for(i = 1; i < NSEED; i++){
			seed[i] = seed[i-1]+11;
		}

		/***** Initialize streams *****/
		VSLStreamStatePtr stream = new VSLStreamStatePtr();
		int errcode = VSL.vslNewStreamEx(stream, BRNG, NSEED, seed);
		VSLStreamStatePtr streamCpy = new VSLStreamStatePtr();
		errcode = VSL.vslNewStream(streamCpy, BRNG, seedCpy);
		ErrCheck.CheckVslError( errcode );
		errcode = VSL.vslCopyStreamState ( streamCpy, stream );
		ErrCheck.CheckVslError( errcode );

		/***** Call RNGs *****/
		errcode = VSL.viRngUniformBits( METHOD, stream,   N, r );
		ErrCheck.CheckVslError( errcode );
		errcode = VSL.viRngUniformBits( METHOD, streamCpy,   N, rCpy );
		ErrCheck.CheckVslError( errcode );

		/***** Compare results *****/
		for(i = 0; i < N; i++){
			if(r[i] != rCpy[i]) err++;
		}

		/***** Printing results *****/
		System.out.println("");
		System.out.println(" Sample of vslCopyStreamState");
		System.out.println(" ----------------------------");
		System.out.println("");
		System.out.println(" Parameters:");
		System.out.print("    seed    = {");
		for(i = 0; i < NSEED; i++){
			System.out.print(" " + seed[i]);
		}
		System.out.println(" }");
		System.out.println("    seedCpy =   " + seedCpy);
		System.out.println("");


		System.out.println(" Results (first 10 of 1000):");
		System.out.println(" ---------------------------");
		for(i=0;i<10;i++) {
			System.out.println("r[" + i + "]=" + Integer.toHexString(r[i]) + " rCpy[" + i + "]=" + Integer.toHexString(rCpy[i]));
		}

		System.out.println("");
		if(err > 0) {
			System.out.println("Error: " + err + " values are incorrect!");
		} else {
			System.out.println(" Results of original stream and its copy are identical.");
		}

		System.out.println("");

		/***** Deinitialize *****/
		errcode = VSL.vslDeleteStream( stream );
		ErrCheck.CheckVslError( errcode );
		errcode = VSL.vslDeleteStream( streamCpy );
		ErrCheck.CheckVslError( errcode );

	}
}
