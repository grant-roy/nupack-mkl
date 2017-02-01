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

public final class vslgetnumregbrngs {
    /**
     * Incarnation prohibited.
     */
    private vslgetnumregbrngs() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "vslgetnumregbrngs";

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
        final int BRNGS = 12;

		/***** Get number of registered BRNGs *****/
		int brngsObt = VSL.vslGetNumRegBrngs ();

		/***** Printing results *****/
		System.out.println("");
		System.out.println("Sample of vslGetNumRegBrngs.");
		System.out.println("----------------------");

		if(brngsObt != BRNGS) {
			System.out.println(" Error: returned value " + brngsObt + " is incorrect (expected " + BRNGS + ")!");
		} else {
			System.out.println(" Returned " + brngsObt + " as expected");
		}

		System.out.println("");

	}
}
