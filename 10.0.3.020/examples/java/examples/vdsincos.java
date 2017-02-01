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

public final class vdsincos {
    /**
     * Incarnation prohibited.
     */
    private vdsincos() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "vdsincos";

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
        final double DEPS = 0.1;
        final int VEC_LEN = 11;
        final double __DSINCOS_BEG = -10000.0;
        final double __DSINCOS_END = 10000.0;

        double[] dA1 = new double[VEC_LEN];
        double[] dB1 = new double[VEC_LEN];
        double[] dB2 = new double[VEC_LEN];
        double[] dB3 = new double[VEC_LEN];
        double[] dB4 = new double[VEC_LEN];

        int i=0,vec_len=VEC_LEN;
        double CurRMS,MaxRMSsin=0.0,MaxRMScos=0.0;

        for(i=0;i<vec_len;i++) {
                dA1[i]=(double)(__DSINCOS_BEG+((__DSINCOS_END-__DSINCOS_BEG)*i)/vec_len);
                dB1[i]=0.0;
                dB2[i]=0.0;
                dB3[i]=Math.sin(dA1[i]);
                dB4[i]=Math.cos(dA1[i]);
        }

        VML.vdSinCos(vec_len,dA1,dB1,dB2);

        System.out.println("vdSinCos test/example program\n");
        System.out.println("        Argument         vdSinCos:   Sin        Cos           Sin        Cos");
        System.out.println("===============================================================================");
        for(i=0;i<vec_len;i++) {
                System.out.println("" + dA1[i] + " " + dB1[i] + " " + dB2[i] + " " + dB3[i] + " " + dB4[i] + "");
                CurRMS=(dB1[i]-dB3[i])/(0.5*(dB1[i]+dB3[i]));
                if(MaxRMSsin<CurRMS) MaxRMSsin=CurRMS;
                CurRMS=(dB2[i]-dB4[i])/(0.5*(dB2[i]+dB4[i]));
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
