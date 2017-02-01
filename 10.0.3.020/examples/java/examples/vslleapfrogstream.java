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

public final class vslleapfrogstream {
    /**
     * Incarnation prohibited.
     */
    private vslleapfrogstream() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "vslleapfrogstream";

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
        final int BRNG = VSL.BRNG_MCG31;
        final int METHOD = 0;
        final int N = 1000;
        final int S = 10;
        final int NS = 100;

		int r[] = new int [N];
		int rL[] = new int [N];
		int i, j, k, err = 0;

		VSLStreamStatePtr streamL[] = new VSLStreamStatePtr[S];

		/****** Creat main stream *********/
		VSLStreamStatePtr stream = new VSLStreamStatePtr();
		int errcode = VSL.vslNewStream(stream,BRNG, SEED);
		/* Create leapfrog streams as copies of the main one */
		for(i=0;i<S;i++) {
			streamL[i] = new VSLStreamStatePtr();
			errcode = VSL.vslCopyStream( streamL[i], stream );
			ErrCheck.CheckVslError( errcode );
			errcode = VSL.vslLeapfrogStream( streamL[i], i, S );
			ErrCheck.CheckVslError( errcode );
		}

		/**** Generate random numbers for main stream  ****/
		errcode = VSL.viRngUniformBits( METHOD, stream, N, r );
		ErrCheck.CheckVslError( errcode );
		/* Generate random numbers for leapfrog streams  */
		for(i=0;i<S;i++) {
			int rTmp[] = new int [N];
			errcode = VSL.viRngUniformBits( METHOD, streamL[i], NS, rTmp );
			ErrCheck.CheckVslError( errcode );
			for(j=0;j<NS;j++) {
				rL[i*NS+j] = rTmp[j];
			}
		}

		/***** Compare results *****/
		for ( j=0, i=0; i<NS; i++ ) {
			for ( k=0; k<S; k++ ) {
				if(r[j++] != rL[k*NS+i]) err++;
			}
		}

		/***** Printing results *****/
		System.out.println("");
		System.out.println(" Sample of vslLeapfrogStream");
		System.out.println(" ---------------------------");
		System.out.println("");
		System.out.println(" Parameters:");
		System.out.println("    seed   =   " + SEED);
		System.out.println("");

		System.out.println(" Results (first 10 of 1000):");
		System.out.println(" ---------------------------");
		for(i=0;i<10;i++) {
			System.out.println("r[" + i + "]=" + r[i] + " rL[" + i*NS + "]=" + rL[i*NS]);
		}

		System.out.println("");
		if(err > 0) {
			System.out.println("Error: " + err + " values are incorrect!");
		} else {
			System.out.println(" Results of ordinary and Leapfrog streams are identical.");
		}

		System.out.println("");

		/***** Deinitialize *****/
		errcode = VSL.vslDeleteStream( stream );
		ErrCheck.CheckVslError( errcode );
		for(i=0;i<S;i++) {
			errcode = VSL.vslDeleteStream( streamL[i] );
			ErrCheck.CheckVslError( errcode );
		}

	}
}
