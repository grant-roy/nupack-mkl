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

package com.intel.mkl;

/**
 * This class contains native methods for VML class.
 *
 * @see VML
 */
class VMLNative {

    /** Load native library */
    static { System.loadLibrary( "mkl_java_stubs" ); }

    static native int vsInv(int n, float[] a, float[] r);
    static native int vdInv(int n, double[] a, double[] r);
    static native int vsSqrt(int n, float[] a, float[] r);
    static native int vdSqrt(int n, double[] a, double[] r);
    static native int vcSqrt(int n, float[] a, float[] r);
    static native int vzSqrt(int n, double[] a, double[] r);
    static native int vsInvSqrt(int n, float[] a, float[] r);
    static native int vdInvSqrt(int n, double[] a, double[] r);
    static native int vsCbrt(int n, float[] a, float[] r);
    static native int vdCbrt(int n, double[] a, double[] r);
    static native int vsInvCbrt(int n, float[] a, float[] r);
    static native int vdInvCbrt(int n, double[] a, double[] r);
    static native int vsExp(int n, float[] a, float[] r);
    static native int vdExp(int n, double[] a, double[] r);
    static native int vcExp(int n, float[] a, float[] r);
    static native int vzExp(int n, double[] a, double[] r);
    static native int vsLn(int n, float[] a, float[] r);
    static native int vdLn(int n, double[] a, double[] r);
    static native int vcLn(int n, float[] a, float[] r);
    static native int vzLn(int n, double[] a, double[] r);
    static native int vsLog10(int n, float[] a, float[] r);
    static native int vdLog10(int n, double[] a, double[] r);
    static native int vcLog10(int n, float[] a, float[] r);
    static native int vzLog10(int n, double[] a, double[] r);
    static native int vsCos(int n, float[] a, float[] r);
    static native int vdCos(int n, double[] a, double[] r);
    static native int vcCos(int n, float[] a, float[] r);
    static native int vzCos(int n, double[] a, double[] r);
    static native int vsSin(int n, float[] a, float[] r);
    static native int vdSin(int n, double[] a, double[] r);
    static native int vcSin(int n, float[] a, float[] r);
    static native int vzSin(int n, double[] a, double[] r);
    static native int vsTan(int n, float[] a, float[] r);
    static native int vdTan(int n, double[] a, double[] r);
    static native int vcTan(int n, float[] a, float[] r);
    static native int vzTan(int n, double[] a, double[] r);
    static native int vsCosh(int n, float[] a, float[] r);
    static native int vdCosh(int n, double[] a, double[] r);
    static native int vcCosh(int n, float[] a, float[] r);
    static native int vzCosh(int n, double[] a, double[] r);
    static native int vsSinh(int n, float[] a, float[] r);
    static native int vdSinh(int n, double[] a, double[] r);
    static native int vcSinh(int n, float[] a, float[] r);
    static native int vzSinh(int n, double[] a, double[] r);
    static native int vsTanh(int n, float[] a, float[] r);
    static native int vdTanh(int n, double[] a, double[] r);
    static native int vcTanh(int n, float[] a, float[] r);
    static native int vzTanh(int n, double[] a, double[] r);
    static native int vsAcos(int n, float[] a, float[] r);
    static native int vdAcos(int n, double[] a, double[] r);
    static native int vcAcos(int n, float[] a, float[] r);
    static native int vzAcos(int n, double[] a, double[] r);
    static native int vsAsin(int n, float[] a, float[] r);
    static native int vdAsin(int n, double[] a, double[] r);
    static native int vcAsin(int n, float[] a, float[] r);
    static native int vzAsin(int n, double[] a, double[] r);
    static native int vsAtan(int n, float[] a, float[] r);
    static native int vdAtan(int n, double[] a, double[] r);
    static native int vcAtan(int n, float[] a, float[] r);
    static native int vzAtan(int n, double[] a, double[] r);
    static native int vsAcosh(int n, float[] a, float[] r);
    static native int vdAcosh(int n, double[] a, double[] r);
    static native int vcAcosh(int n, float[] a, float[] r);
    static native int vzAcosh(int n, double[] a, double[] r);
    static native int vsAsinh(int n, float[] a, float[] r);
    static native int vdAsinh(int n, double[] a, double[] r);
    static native int vcAsinh(int n, float[] a, float[] r);
    static native int vzAsinh(int n, double[] a, double[] r);
    static native int vsAtanh(int n, float[] a, float[] r);
    static native int vdAtanh(int n, double[] a, double[] r);
    static native int vcAtanh(int n, float[] a, float[] r);
    static native int vzAtanh(int n, double[] a, double[] r);
    static native int vsErf(int n, float[] a, float[] r);
    static native int vdErf(int n, double[] a, double[] r);
    static native int vsErfInv(int n, float[] a, float[] r);
    static native int vdErfInv(int n, double[] a, double[] r);
    static native int vsHypot(int n, float[] a, float[] b, float[] r);
    static native int vdHypot(int n, double[] a, double[] b, double[] r);
    static native int vsErfc(int n, float[] a, float[] r);
    static native int vdErfc(int n, double[] a, double[] r);
    static native int vsAtan2(int n, float[] a, float[] b, float[] r);
    static native int vdAtan2(int n, double[] a, double[] b, double[] r);
    static native int vsDiv(int n, float[] a, float[] b, float[] r);
    static native int vdDiv(int n, double[] a, double[] b, double[] r);
    static native int vsPow(int n, float[] a, float[] b, float[] r);
    static native int vdPow(int n, double[] a, double[] b, double[] r);
    static native int vcPow(int n, float[] a, float[] b, float[] r);
    static native int vzPow(int n, double[] a, double[] b, double[] r);
    static native int vsPowx(int n, float[] a, float b, float[] r);
    static native int vdPowx(int n, double[] a, double b, double[] r);
    static native int vcPowx(int n, float[] a, float[] b, float[] r);
    static native int vzPowx(int n, double[] a, double[] b, double[] r);
    static native int vsSinCos(int n, float[] a, float[] r1, float[] r2);
    static native int vdSinCos(int n, double[] a, double[] r1, double[] r2);
    static native int vsCeil(int n, float[] a, float[] r);
    static native int vdCeil(int n, double[] a, double[] r);
    static native int vsFloor(int n, float[] a, float[] r);
    static native int vdFloor(int n, double[] a, double[] r);
    static native int vsModf(int n, float[] a, float[] r1, float[] r2);
    static native int vdModf(int n, double[] a, double[] r1, double[] r2);
    static native int vsNearbyInt(int n, float[] a, float[] r);
    static native int vdNearbyInt(int n, double[] a, double[] r);
    static native int vsRint(int n, float[] a, float[] r);
    static native int vdRint(int n, double[] a, double[] r);
    static native int vsRound(int n, float[] a, float[] r);
    static native int vdRound(int n, double[] a, double[] r);
    static native int vsTrunc(int n, float[] a, float[] r);
    static native int vdTrunc(int n, double[] a, double[] r);
    static native int vsPackI(int n, float[] a, int incra, float[] y);
    static native int vdPackI(int n, double[] a, int incra, double[] y);
    static native int vsPackV(int n, float[] a, int[] ia, float[] y);
    static native int vdPackV(int n, double[] a, int[] ia, double[] y);
    static native int vsPackM(int n, float[] a, int[] ma, float[] y);
    static native int vdPackM(int n, double[] a, int[] ma, double[] y);
    static native int vsUnpackI(int n, float[] a, float[] y, int incry);
    static native int vdUnpackI(int n, double[] a, double[] y, int incry);
    static native int vsUnpackV(int n, float[] a, float[] y, int[] iy);
    static native int vdUnpackV(int n, double[] a, double[] y, int[] iy);
    static native int vsUnpackM(int n, float[] a, float[] y, int[] my);
    static native int vdUnpackM(int n, double[] a, double[] y, int[] my);
}
