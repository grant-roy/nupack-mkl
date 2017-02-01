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

public final class vssincos {
    /**
     * Incarnation prohibited.
     */
    private vssincos() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "vssincos";

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
        final float DEPS = 0.1f;
        final int VEC_LEN = 11;
        final float __SSINCOS_BEG = -10000.0f;
        final float __SSINCOS_END = 10000.0f;

        float[] fA1 = new float[VEC_LEN];
        float[] fB1 = new float[VEC_LEN];
        float[] fB2 = new float[VEC_LEN];
        float[] fB3 = new float[VEC_LEN];
        float[] fB4 = new float[VEC_LEN];

        int i=0,vec_len=VEC_LEN;
        double CurRMS,MaxRMSsin=0.0f,MaxRMScos=0.0f;

        for(i=0;i<vec_len;i++) {
                fA1[i]=(float)(__SSINCOS_BEG+((__SSINCOS_END-__SSINCOS_BEG)*i)/vec_len);
                fB1[i]=0.0f;
                fB2[i]=0.0f;
                fB3[i]=(float)Math.sin(fA1[i]);
                fB4[i]=(float)Math.cos(fA1[i]);
        }

        VML.vsSinCos(vec_len,fA1,fB1,fB2);

        System.out.println("vsSinCos test/example program\n");
        System.out.println("        Argument         vsSinCos:   Sin        Cos           Sin        Cos");
        System.out.println("===============================================================================");
        for(i=0;i<vec_len;i++) {
                System.out.println("" + fA1[i] + " " + fB1[i] + " " + fB2[i] + " " + fB3[i] + " " + fB4[i] + "");
                CurRMS=(fB1[i]-fB3[i])/(0.5f*(fB1[i]+fB3[i]));
                if(MaxRMSsin<CurRMS) MaxRMSsin=CurRMS;
                CurRMS=(fB2[i]-fB4[i])/(0.5f*(fB2[i]+fB4[i]));
                if(MaxRMScos<CurRMS) MaxRMScos=CurRMS;
        }
        System.out.println("");
        if(MaxRMSsin>=DEPS) {
                System.out.println("Error! Relative accuracy (Math.sin) is " + MaxRMSsin + "");
        }
        else {
                System.out.println("Relative accuracy (Math.sin) is " + MaxRMSsin + "");
        }
        if(MaxRMScos>=DEPS) {
                System.out.println("Error! Relative accuracy (Math.cos) is " + MaxRMScos + "");
        }
        else {
                System.out.println("Relative accuracy (Math.cos) is " + MaxRMScos + "");
        }

        return false;
    }
}
