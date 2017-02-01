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
 * Wrappers to VML functions from Intel MKL.
 *
 * <p>The VML Interface for C/C++ is used for accessing to
 * the VML Mathematical and Pack/Unpack functions. See MKL
 * Reference Manual, Chapter 9 "Vector Mathematical Functions".
 *
 * <p>Both Java and C parts of the wrapper VML demonstrate the
 * straightforward approach similar to CBLAS wrapper. Unlike
 * the last, available checking of arguments are added here.
 * To differentiate native methods from Java ones one more
 * class VMLNative has been added.
 *
 * <p>Unlike the native functions Java methods return errstatus
 * to detect possible problems.
 *
 * <p>These wrappers assume using 1-dimensional Java arrays of
 * the type double[] or float[] to process floating-point data.
 * A multi-dimensional data series is stored into 1-dimensional
 * array as column-major or row-major 1-dimensional data series.
 *
 * <p>A complex number is stored as 2-elements array: real part
 * of the number as the 1st element of the array, and imaginary
 * part as the 2nd.
 *
 * <p>A complex data series of the length N is stored as a real
 * series of the length 2*N. The n-th complex element is stored
 * as the pair of 2*n-th and (2*n+1)-th real elements: the even
 * indexed for real part and the odd indexed for imaginary part.
 *
 * <p>For more details, please see the MKL User's Guide.
 *
 */
public final class VML {

    /** Instantiation is disabled. */
    private VML() {}

    /**
     * Wrapper for MKL function vsInv().
     */
    public static int vsInv(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsInv");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsInv");
        }
        return VMLNative.vsInv(n, a, r);
    }

    /**
     * Wrapper for MKL function vdInv().
     */
    public static int vdInv(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdInv");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdInv");
        }
        return VMLNative.vdInv(n, a, r);
    }

    /**
     * Wrapper for MKL function vsSqrt().
     */
    public static int vsSqrt(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsSqrt");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsSqrt");
        }
        return VMLNative.vsSqrt(n, a, r);
    }

    /**
     * Wrapper for MKL function vdSqrt().
     */
    public static int vdSqrt(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdSqrt");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdSqrt");
        }
        return VMLNative.vdSqrt(n, a, r);
    }

    /**
     * Wrapper for MKL function vcSqrt().
     */
    public static int vcSqrt(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcSqrt");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcSqrt");
        }
        return VMLNative.vcSqrt(n, a, r);
    }

    /**
     * Wrapper for MKL function vzSqrt().
     */
    public static int vzSqrt(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzSqrt");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzSqrt");
        }
        return VMLNative.vzSqrt(n, a, r);
    }

    /**
     * Wrapper for MKL function vsInvSqrt().
     */
    public static int vsInvSqrt(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsInvSqrt");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsInvSqrt");
        }
        return VMLNative.vsInvSqrt(n, a, r);
    }

    /**
     * Wrapper for MKL function vdInvSqrt().
     */
    public static int vdInvSqrt(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdInvSqrt");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdInvSqrt");
        }
        return VMLNative.vdInvSqrt(n, a, r);
    }

    /**
     * Wrapper for MKL function vsCbrt().
     */
    public static int vsCbrt(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsCbrt");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsCbrt");
        }
        return VMLNative.vsCbrt(n, a, r);
    }

    /**
     * Wrapper for MKL function vdCbrt().
     */
    public static int vdCbrt(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdCbrt");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdCbrt");
        }
        return VMLNative.vdCbrt(n, a, r);
    }

    /**
     * Wrapper for MKL function vsInvCbrt().
     */
    public static int vsInvCbrt(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsInvCbrt");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsInvCbrt");
        }
        return VMLNative.vsInvCbrt(n, a, r);
    }

    /**
     * Wrapper for MKL function vdInvCbrt().
     */
    public static int vdInvCbrt(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdInvCbrt");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdInvCbrt");
        }
        return VMLNative.vdInvCbrt(n, a, r);
    }

    /**
     * Wrapper for MKL function vsExp().
     */
    public static int vsExp(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsExp");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsExp");
        }
        return VMLNative.vsExp(n, a, r);
    }

    /**
     * Wrapper for MKL function vdExp().
     */
    public static int vdExp(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdExp");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdExp");
        }
        return VMLNative.vdExp(n, a, r);
    }

    /**
     * Wrapper for MKL function vcExp().
     */
    public static int vcExp(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcExp");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcExp");
        }
        return VMLNative.vcExp(n, a, r);
    }

    /**
     * Wrapper for MKL function vzExp().
     */
    public static int vzExp(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzExp");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzExp");
        }
        return VMLNative.vzExp(n, a, r);
    }

    /**
     * Wrapper for MKL function vsLn().
     */
    public static int vsLn(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsLn");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsLn");
        }
        return VMLNative.vsLn(n, a, r);
    }

    /**
     * Wrapper for MKL function vdLn().
     */
    public static int vdLn(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdLn");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdLn");
        }
        return VMLNative.vdLn(n, a, r);
    }

    /**
     * Wrapper for MKL function vcLn().
     */
    public static int vcLn(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcLn");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcLn");
        }
        return VMLNative.vcLn(n, a, r);
    }

    /**
     * Wrapper for MKL function vzLn().
     */
    public static int vzLn(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzLn");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzLn");
        }
        return VMLNative.vzLn(n, a, r);
    }

    /**
     * Wrapper for MKL function vsLog10().
     */
    public static int vsLog10(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsLog10");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsLog10");
        }
        return VMLNative.vsLog10(n, a, r);
    }

    /**
     * Wrapper for MKL function vdLog10().
     */
    public static int vdLog10(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdLog10");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdLog10");
        }
        return VMLNative.vdLog10(n, a, r);
    }

    /**
     * Wrapper for MKL function vcLog10().
     */
    public static int vcLog10(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcLog10");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcLog10");
        }
        return VMLNative.vcLog10(n, a, r);
    }

    /**
     * Wrapper for MKL function vzLog10().
     */
    public static int vzLog10(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzLog10");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzLog10");
        }
        return VMLNative.vzLog10(n, a, r);
    }

    /**
     * Wrapper for MKL function vsCos().
     */
    public static int vsCos(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsCos");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsCos");
        }
        return VMLNative.vsCos(n, a, r);
    }

    /**
     * Wrapper for MKL function vdCos().
     */
    public static int vdCos(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdCos");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdCos");
        }
        return VMLNative.vdCos(n, a, r);
    }

    /**
     * Wrapper for MKL function vcCos().
     */
    public static int vcCos(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcCos");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcCos");
        }
        return VMLNative.vcCos(n, a, r);
    }

    /**
     * Wrapper for MKL function vzCos().
     */
    public static int vzCos(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzCos");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzCos");
        }
        return VMLNative.vzCos(n, a, r);
    }

    /**
     * Wrapper for MKL function vsSin().
     */
    public static int vsSin(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsSin");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsSin");
        }
        return VMLNative.vsSin(n, a, r);
    }

    /**
     * Wrapper for MKL function vdSin().
     */
    public static int vdSin(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdSin");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdSin");
        }
        return VMLNative.vdSin(n, a, r);
    }

    /**
     * Wrapper for MKL function vcSin().
     */
    public static int vcSin(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcSin");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcSin");
        }
        return VMLNative.vcSin(n, a, r);
    }

    /**
     * Wrapper for MKL function vzSin().
     */
    public static int vzSin(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzSin");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzSin");
        }
        return VMLNative.vzSin(n, a, r);
    }

    /**
     * Wrapper for MKL function vsTan().
     */
    public static int vsTan(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsTan");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsTan");
        }
        return VMLNative.vsTan(n, a, r);
    }

    /**
     * Wrapper for MKL function vdTan().
     */
    public static int vdTan(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdTan");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdTan");
        }
        return VMLNative.vdTan(n, a, r);
    }

    /**
     * Wrapper for MKL function vcTan().
     */
    public static int vcTan(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcTan");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcTan");
        }
        return VMLNative.vcTan(n, a, r);
    }

    /**
     * Wrapper for MKL function vzTan().
     */
    public static int vzTan(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzTan");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzTan");
        }
        return VMLNative.vzTan(n, a, r);
    }

    /**
     * Wrapper for MKL function vsCosh().
     */
    public static int vsCosh(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsCosh");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsCosh");
        }
        return VMLNative.vsCosh(n, a, r);
    }

    /**
     * Wrapper for MKL function vdCosh().
     */
    public static int vdCosh(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdCosh");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdCosh");
        }
        return VMLNative.vdCosh(n, a, r);
    }

    /**
     * Wrapper for MKL function vcCosh().
     */
    public static int vcCosh(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcCosh");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcCosh");
        }
        return VMLNative.vcCosh(n, a, r);
    }

    /**
     * Wrapper for MKL function vzCosh().
     */
    public static int vzCosh(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzCosh");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzCosh");
        }
        return VMLNative.vzCosh(n, a, r);
    }

    /**
     * Wrapper for MKL function vsSinh().
     */
    public static int vsSinh(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsSinh");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsSinh");
        }
        return VMLNative.vsSinh(n, a, r);
    }

    /**
     * Wrapper for MKL function vdSinh().
     */
    public static int vdSinh(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdSinh");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdSinh");
        }
        return VMLNative.vdSinh(n, a, r);
    }

    /**
     * Wrapper for MKL function vcSinh().
     */
    public static int vcSinh(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcSinh");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcSinh");
        }
        return VMLNative.vcSinh(n, a, r);
    }

    /**
     * Wrapper for MKL function vzSinh().
     */
    public static int vzSinh(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzSinh");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzSinh");
        }
        return VMLNative.vzSinh(n, a, r);
    }

    /**
     * Wrapper for MKL function vsTanh().
     */
    public static int vsTanh(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsTanh");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsTanh");
        }
        return VMLNative.vsTanh(n, a, r);
    }

    /**
     * Wrapper for MKL function vdTanh().
     */
    public static int vdTanh(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdTanh");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdTanh");
        }
        return VMLNative.vdTanh(n, a, r);
    }

    /**
     * Wrapper for MKL function vcTanh().
     */
    public static int vcTanh(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcTanh");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcTanh");
        }
        return VMLNative.vcTanh(n, a, r);
    }

    /**
     * Wrapper for MKL function vzTanh().
     */
    public static int vzTanh(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzTanh");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzTanh");
        }
        return VMLNative.vzTanh(n, a, r);
    }

    /**
     * Wrapper for MKL function vsAcos().
     */
    public static int vsAcos(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsAcos");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsAcos");
        }
        return VMLNative.vsAcos(n, a, r);
    }

    /**
     * Wrapper for MKL function vdAcos().
     */
    public static int vdAcos(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdAcos");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdAcos");
        }
        return VMLNative.vdAcos(n, a, r);
    }

    /**
     * Wrapper for MKL function vcAcos().
     */
    public static int vcAcos(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcAcos");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcAcos");
        }
        return VMLNative.vcAcos(n, a, r);
    }

    /**
     * Wrapper for MKL function vzAcos().
     */
    public static int vzAcos(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzAcos");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzAcos");
        }
        return VMLNative.vzAcos(n, a, r);
    }

    /**
     * Wrapper for MKL function vsAsin().
     */
    public static int vsAsin(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsAsin");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsAsin");
        }
        return VMLNative.vsAsin(n, a, r);
    }

    /**
     * Wrapper for MKL function vdAsin().
     */
    public static int vdAsin(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdAsin");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdAsin");
        }
        return VMLNative.vdAsin(n, a, r);
    }

    /**
     * Wrapper for MKL function vcAsin().
     */
    public static int vcAsin(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcAsin");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcAsin");
        }
        return VMLNative.vcAsin(n, a, r);
    }

    /**
     * Wrapper for MKL function vzAsin().
     */
    public static int vzAsin(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzAsin");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzAsin");
        }
        return VMLNative.vzAsin(n, a, r);
    }

    /**
     * Wrapper for MKL function vsAtan().
     */
    public static int vsAtan(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsAtan");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsAtan");
        }
        return VMLNative.vsAtan(n, a, r);
    }

    /**
     * Wrapper for MKL function vdAtan().
     */
    public static int vdAtan(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdAtan");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdAtan");
        }
        return VMLNative.vdAtan(n, a, r);
    }

    /**
     * Wrapper for MKL function vcAtan().
     */
    public static int vcAtan(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcAtan");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcAtan");
        }
        return VMLNative.vcAtan(n, a, r);
    }

    /**
     * Wrapper for MKL function vzAtan().
     */
    public static int vzAtan(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzAtan");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzAtan");
        }
        return VMLNative.vzAtan(n, a, r);
    }

    /**
     * Wrapper for MKL function vsAcosh().
     */
    public static int vsAcosh(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsAcosh");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsAcosh");
        }
        return VMLNative.vsAcosh(n, a, r);
    }

    /**
     * Wrapper for MKL function vdAcosh().
     */
    public static int vdAcosh(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdAcosh");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdAcosh");
        }
        return VMLNative.vdAcosh(n, a, r);
    }

    /**
     * Wrapper for MKL function vcAcosh().
     */
    public static int vcAcosh(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcAcosh");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcAcosh");
        }
        return VMLNative.vcAcosh(n, a, r);
    }

    /**
     * Wrapper for MKL function vzAcosh().
     */
    public static int vzAcosh(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzAcosh");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzAcosh");
        }
        return VMLNative.vzAcosh(n, a, r);
    }

    /**
     * Wrapper for MKL function vsAsinh().
     */
    public static int vsAsinh(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsAsinh");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsAsinh");
        }
        return VMLNative.vsAsinh(n, a, r);
    }

    /**
     * Wrapper for MKL function vdAsinh().
     */
    public static int vdAsinh(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdAsinh");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdAsinh");
        }
        return VMLNative.vdAsinh(n, a, r);
    }

    /**
     * Wrapper for MKL function vcAsinh().
     */
    public static int vcAsinh(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcAsinh");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcAsinh");
        }
        return VMLNative.vcAsinh(n, a, r);
    }

    /**
     * Wrapper for MKL function vzAsinh().
     */
    public static int vzAsinh(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzAsinh");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzAsinh");
        }
        return VMLNative.vzAsinh(n, a, r);
    }

    /**
     * Wrapper for MKL function vsAtanh().
     */
    public static int vsAtanh(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsAtanh");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsAtanh");
        }
        return VMLNative.vsAtanh(n, a, r);
    }

    /**
     * Wrapper for MKL function vdAtanh().
     */
    public static int vdAtanh(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdAtanh");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdAtanh");
        }
        return VMLNative.vdAtanh(n, a, r);
    }

    /**
     * Wrapper for MKL function vcAtanh().
     */
    public static int vcAtanh(int n, float[] a, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcAtanh");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcAtanh");
        }
        return VMLNative.vcAtanh(n, a, r);
    }

    /**
     * Wrapper for MKL function vzAtanh().
     */
    public static int vzAtanh(int n, double[] a, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzAtanh");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzAtanh");
        }
        return VMLNative.vzAtanh(n, a, r);
    }

    /**
     * Wrapper for MKL function vsErf().
     */
    public static int vsErf(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsErf");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsErf");
        }
        return VMLNative.vsErf(n, a, r);
    }

    /**
     * Wrapper for MKL function vdErf().
     */
    public static int vdErf(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdErf");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdErf");
        }
        return VMLNative.vdErf(n, a, r);
    }

    /**
     * Wrapper for MKL function vsErfInv().
     */
    public static int vsErfInv(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsErfInv");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsErfInv");
        }
        return VMLNative.vsErfInv(n, a, r);
    }

    /**
     * Wrapper for MKL function vdErfInv().
     */
    public static int vdErfInv(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdErfInv");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdErfInv");
        }
        return VMLNative.vdErfInv(n, a, r);
    }

    /**
     * Wrapper for MKL function vsHypot().
     */
    public static int vsHypot(int n, float[] a, float[] b, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsHypot");
        }
        if(b.length < n) {
            throw new IllegalArgumentException("Too few elements in array b in vsHypot");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsHypot");
        }
        return VMLNative.vsHypot(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vdHypot().
     */
    public static int vdHypot(int n, double[] a, double[] b, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdHypot");
        }
        if(b.length < n) {
            throw new IllegalArgumentException("Too few elements in array b in vdHypot");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdHypot");
        }
        return VMLNative.vdHypot(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vsErfc().
     */
    public static int vsErfc(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsErfc");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsErfc");
        }
        return VMLNative.vsErfc(n, a, r);
    }

    /**
     * Wrapper for MKL function vdErfc().
     */
    public static int vdErfc(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdErfc");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdErfc");
        }
        return VMLNative.vdErfc(n, a, r);
    }

    /**
     * Wrapper for MKL function vsAtan2().
     */
    public static int vsAtan2(int n, float[] a, float[] b, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsAtan2");
        }
        if(b.length < n) {
            throw new IllegalArgumentException("Too few elements in array b in vsAtan2");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsAtan2");
        }
        return VMLNative.vsAtan2(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vdAtan2().
     */
    public static int vdAtan2(int n, double[] a, double[] b, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdAtan2");
        }
        if(b.length < n) {
            throw new IllegalArgumentException("Too few elements in array b in vdAtan2");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdAtan2");
        }
        return VMLNative.vdAtan2(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vsDiv().
     */
    public static int vsDiv(int n, float[] a, float[] b, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsDiv");
        }
        if(b.length < n) {
            throw new IllegalArgumentException("Too few elements in array b in vsDiv");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsDiv");
        }
        return VMLNative.vsDiv(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vdDiv().
     */
    public static int vdDiv(int n, double[] a, double[] b, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdDiv");
        }
        if(b.length < n) {
            throw new IllegalArgumentException("Too few elements in array b in vdDiv");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdDiv");
        }
        return VMLNative.vdDiv(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vsPow().
     */
    public static int vsPow(int n, float[] a, float[] b, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsPow");
        }
        if(b.length < n) {
            throw new IllegalArgumentException("Too few elements in array b in vsPow");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsPow");
        }
        return VMLNative.vsPow(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vdPow().
     */
    public static int vdPow(int n, double[] a, double[] b, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdPow");
        }
        if(b.length < n) {
            throw new IllegalArgumentException("Too few elements in array b in vdPow");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdPow");
        }
        return VMLNative.vdPow(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vcPow().
     */
    public static int vcPow(int n, float[] a, float[] b, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcPow");
        }
        if(b.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array b in vcPow");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcPow");
        }
        return VMLNative.vcPow(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vzPow().
     */
    public static int vzPow(int n, double[] a, double[] b, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzPow");
        }
        if(b.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array b in vzPow");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzPow");
        }
        return VMLNative.vzPow(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vsPowx().
     */
    public static int vsPowx(int n, float[] a, float b, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsPowx");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsPowx");
        }
        return VMLNative.vsPowx(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vdPowx().
     */
    public static int vdPowx(int n, double[] a, double b, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdPowx");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdPowx");
        }
        return VMLNative.vdPowx(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vcPowx().
     */
    public static int vcPowx(int n, float[] a, float[] b, float[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vcPowx");
        }
        if(b.length < 2) {
            throw new IllegalArgumentException("Too few elements in array b in vcPowx");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vcPowx");
        }
        return VMLNative.vcPowx(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vzPowx().
     */
    public static int vzPowx(int n, double[] a, double[] b, double[] r)
    {
        if(a.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array a in vzPowx");
        }
        if(b.length < 2) {
            throw new IllegalArgumentException("Too few elements in array b in vzPowx");
        }
        if(r.length < 2*n) {
            throw new IllegalArgumentException("Too few elements in array r in vzPowx");
        }
        return VMLNative.vzPowx(n, a, b, r);
    }

    /**
     * Wrapper for MKL function vsSinCos().
     */
    public static int vsSinCos(int n, float[] a, float[] r1, float[] r2)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsSinCos");
        }
        if(r1.length < n) {
            throw new IllegalArgumentException("Too few elements in array r1 in vsSinCos");
        }
        if(r2.length < n) {
            throw new IllegalArgumentException("Too few elements in array r2 in vsSinCos");
        }
        return VMLNative.vsSinCos(n, a, r1, r2);
    }

    /**
     * Wrapper for MKL function vdSinCos().
     */
    public static int vdSinCos(int n, double[] a, double[] r1, double[] r2)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdSinCos");
        }
        if(r1.length < n) {
            throw new IllegalArgumentException("Too few elements in array r1 in vdSinCos");
        }
        if(r2.length < n) {
            throw new IllegalArgumentException("Too few elements in array r2 in vdSinCos");
        }
        return VMLNative.vdSinCos(n, a, r1, r2);
    }

    /**
     * Wrapper for MKL function vsCeil().
     */
    public static int vsCeil(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsCeil");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsCeil");
        }
        return VMLNative.vsCeil(n, a, r);
    }

    /**
     * Wrapper for MKL function vdCeil().
     */
    public static int vdCeil(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdCeil");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdCeil");
        }
        return VMLNative.vdCeil(n, a, r);
    }

    /**
     * Wrapper for MKL function vsFloor().
     */
    public static int vsFloor(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsFloor");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsFloor");
        }
        return VMLNative.vsFloor(n, a, r);
    }

    /**
     * Wrapper for MKL function vdFloor().
     */
    public static int vdFloor(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdFloor");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdFloor");
        }
        return VMLNative.vdFloor(n, a, r);
    }

    /**
     * Wrapper for MKL function vsModf().
     */
    public static int vsModf(int n, float[] a, float[] r1, float[] r2)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsModf");
        }
        if(r1.length < n) {
            throw new IllegalArgumentException("Too few elements in array r1 in vsModf");
        }
        if(r2.length < n) {
            throw new IllegalArgumentException("Too few elements in array r2 in vsModf");
        }
        return VMLNative.vsModf(n, a, r1, r2);
    }

    /**
     * Wrapper for MKL function vdModf().
     */
    public static int vdModf(int n, double[] a, double[] r1, double[] r2)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdModf");
        }
        if(r1.length < n) {
            throw new IllegalArgumentException("Too few elements in array r1 in vdModf");
        }
        if(r2.length < n) {
            throw new IllegalArgumentException("Too few elements in array r2 in vdModf");
        }
        return VMLNative.vdModf(n, a, r1, r2);
    }

    /**
     * Wrapper for MKL function vsNearbyInt().
     */
    public static int vsNearbyInt(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsNearbyInt");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsNearbyInt");
        }
        return VMLNative.vsNearbyInt(n, a, r);
    }

    /**
     * Wrapper for MKL function vdNearbyInt().
     */
    public static int vdNearbyInt(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdNearbyInt");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdNearbyInt");
        }
        return VMLNative.vdNearbyInt(n, a, r);
    }

    /**
     * Wrapper for MKL function vsRint().
     */
    public static int vsRint(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsRint");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsRint");
        }
        return VMLNative.vsRint(n, a, r);
    }

    /**
     * Wrapper for MKL function vdRint().
     */
    public static int vdRint(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdRint");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdRint");
        }
        return VMLNative.vdRint(n, a, r);
    }

    /**
     * Wrapper for MKL function vsRound().
     */
    public static int vsRound(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsRound");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsRound");
        }
        return VMLNative.vsRound(n, a, r);
    }

    /**
     * Wrapper for MKL function vdRound().
     */
    public static int vdRound(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdRound");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdRound");
        }
        return VMLNative.vdRound(n, a, r);
    }

    /**
     * Wrapper for MKL function vsTrunc().
     */
    public static int vsTrunc(int n, float[] a, float[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsTrunc");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vsTrunc");
        }
        return VMLNative.vsTrunc(n, a, r);
    }

    /**
     * Wrapper for MKL function vdTrunc().
     */
    public static int vdTrunc(int n, double[] a, double[] r)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdTrunc");
        }
        if(r.length < n) {
            throw new IllegalArgumentException("Too few elements in array r in vdTrunc");
        }
        return VMLNative.vdTrunc(n, a, r);
    }

    /**
     * Wrapper for MKL function vsPackI().
     */
    public static int vsPackI(int n, float[] a, int incra, float[] y)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsPackI");
        }
        if(y.length < n) {
            throw new IllegalArgumentException("Too few elements in array y in vsPackI");
        }
        return VMLNative.vsPackI(n, a, incra, y);
    }

    /**
     * Wrapper for MKL function vdPackI().
     */
    public static int vdPackI(int n, double[] a, int incra, double[] y)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdPackI");
        }
        if(y.length < n) {
            throw new IllegalArgumentException("Too few elements in array y in vdPackI");
        }
        return VMLNative.vdPackI(n, a, incra, y);
    }

    /**
     * Wrapper for MKL function vsPackV().
     */
    public static int vsPackV(int n, float[] a, int[] ia, float[] y)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsPackV");
        }
        if(ia.length < n) {
            throw new IllegalArgumentException("Too few elements in array ia in vsPackV");
        }
        if(y.length < n) {
            throw new IllegalArgumentException("Too few elements in array y in vsPackV");
        }
        return VMLNative.vsPackV(n, a, ia, y);
    }

    /**
     * Wrapper for MKL function vdPackV().
     */
    public static int vdPackV(int n, double[] a, int[] ia, double[] y)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdPackV");
        }
        if(ia.length < n) {
            throw new IllegalArgumentException("Too few elements in array ia in vdPackV");
        }
        if(y.length < n) {
            throw new IllegalArgumentException("Too few elements in array y in vdPackV");
        }
        return VMLNative.vdPackV(n, a, ia, y);
    }

    /**
     * Wrapper for MKL function vsPackM().
     */
    public static int vsPackM(int n, float[] a, int[] ma, float[] y)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsPackM");
        }
        if(ma.length < n) {
            throw new IllegalArgumentException("Too few elements in array ma in vsPackM");
        }
        if(y.length < n) {
            throw new IllegalArgumentException("Too few elements in array y in vsPackM");
        }
        return VMLNative.vsPackM(n, a, ma, y);
    }

    /**
     * Wrapper for MKL function vdPackM().
     */
    public static int vdPackM(int n, double[] a, int[] ma, double[] y)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdPackM");
        }
        if(ma.length < n) {
            throw new IllegalArgumentException("Too few elements in array ma in vdPackM");
        }
        if(y.length < n) {
            throw new IllegalArgumentException("Too few elements in array y in vdPackM");
        }
        return VMLNative.vdPackM(n, a, ma, y);
    }

    /**
     * Wrapper for MKL function vsUnpackI().
     */
    public static int vsUnpackI(int n, float[] a, float[] y, int incry)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsUnpackI");
        }
        if(y.length < n) {
            throw new IllegalArgumentException("Too few elements in array y in vsUnpackI");
        }
        return VMLNative.vsUnpackI(n, a, y, incry);
    }

    /**
     * Wrapper for MKL function vdUnpackI().
     */
    public static int vdUnpackI(int n, double[] a, double[] y, int incry)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdUnpackI");
        }
        if(y.length < n) {
            throw new IllegalArgumentException("Too few elements in array y in vdUnpackI");
        }
        return VMLNative.vdUnpackI(n, a, y, incry);
    }

    /**
     * Wrapper for MKL function vsUnpackV().
     */
    public static int vsUnpackV(int n, float[] a, float[] y, int[] iy)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsUnpackV");
        }
        if(y.length < n) {
            throw new IllegalArgumentException("Too few elements in array y in vsUnpackV");
        }
        if(iy.length < n) {
            throw new IllegalArgumentException("Too few elements in array iy in vsUnpackV");
        }
        return VMLNative.vsUnpackV(n, a, y, iy);
    }

    /**
     * Wrapper for MKL function vdUnpackV().
     */
    public static int vdUnpackV(int n, double[] a, double[] y, int[] iy)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdUnpackV");
        }
        if(y.length < n) {
            throw new IllegalArgumentException("Too few elements in array y in vdUnpackV");
        }
        if(iy.length < n) {
            throw new IllegalArgumentException("Too few elements in array iy in vdUnpackV");
        }
        return VMLNative.vdUnpackV(n, a, y, iy);
    }

    /**
     * Wrapper for MKL function vsUnpackM().
     */
    public static int vsUnpackM(int n, float[] a, float[] y, int[] my)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vsUnpackM");
        }
        if(y.length < n) {
            throw new IllegalArgumentException("Too few elements in array y in vsUnpackM");
        }
        if(my.length < n) {
            throw new IllegalArgumentException("Too few elements in array my in vsUnpackM");
        }
        return VMLNative.vsUnpackM(n, a, y, my);
    }

    /**
     * Wrapper for MKL function vdUnpackM().
     */
    public static int vdUnpackM(int n, double[] a, double[] y, int[] my)
    {
        if(a.length < n) {
            throw new IllegalArgumentException("Too few elements in array a in vdUnpackM");
        }
        if(y.length < n) {
            throw new IllegalArgumentException("Too few elements in array y in vdUnpackM");
        }
        if(my.length < n) {
            throw new IllegalArgumentException("Too few elements in array my in vdUnpackM");
        }
        return VMLNative.vdUnpackM(n, a, y, my);
    }

    /** Constant for VML_STATUS_OK from "mkl_vml_defines.h" */
    public final static int STATUS_OK = 0;

    /** Constant for VML_STATUS_BADSIZE from "mkl_vml_defines.h" */
    public final static int STATUS_BADSIZE = -1;

    /** Constant for VML_STATUS_BADMEM from "mkl_vml_defines.h" */
    public final static int STATUS_BADMEM = -2;

    /** Constant for VML_STATUS_ERRDOM from "mkl_vml_defines.h" */
    public final static int STATUS_ERRDOM = 1;

    /** Constant for VML_STATUS_SING from "mkl_vml_defines.h" */
    public final static int STATUS_SING = 2;

    /** Constant for VML_STATUS_OVERFLOW from "mkl_vml_defines.h" */
    public final static int STATUS_OVERFLOW = 3;

    /** Constant for VML_STATUS_UNDERFLOW from "mkl_vml_defines.h" */
    public final static int STATUS_UNDERFLOW = 4;

}

