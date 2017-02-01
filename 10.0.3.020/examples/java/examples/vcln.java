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

public final class vcln {
    /**
     * Incarnation prohibited.
     */
    private vcln() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "vcln";

    /**
     * No command-line options.
     */
    public static void main(String[] args) {
        boolean fault;
        try {
            fault = test();
        } catch (Exception statusException) {
            fault = true;
            statusException.printStackTrace(System.out);
            System.out.println("\nTEST FAILED");
            System.exit(1);
        }
        System.out.println("\nTEST PASSED");
    }

    /**
     * Demonstrate the VML functions and test them.
     * @return true - If the test has failed.
     */
    public static boolean test()
    {
        // Parameters
        final int VEC_LEN = 11;
        final float __SLN_BEG = 0.1f;
        final float __SLN_END = 10000.0f;

        float[] cA = new float[VEC_LEN*2];
        float[] cB1 = new float[VEC_LEN*2];

        int i=0,vec_len=VEC_LEN;

        for(i=0;i<vec_len*2;i+=2) {
                cA[i]=(float)(__SLN_BEG+((__SLN_END-__SLN_BEG)*i)/vec_len);
                cA[i+1]=(float)(__SLN_END-((__SLN_END-__SLN_BEG)*i)/vec_len);
                cB1[i]=0.0f;
                cB1[i+1]=0.0f;
        }

        VML.vcLn(vec_len,cA,cB1);

        System.out.println("vcLn test/example program\n");
        System.out.println("           Argument                           vcLn");
        System.out.println("===============================================================================");
        for(i=0;i<vec_len*2;i+=2) {
                System.out.println("   " + cA[i] + " " + cA[i+1] + "*i      " + cB1[i] + " " + cB1[i+1] + "*i");
        }

        return false;
    }
}
