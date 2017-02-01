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
 * Wrappers to the ESSL-like convolution/correlation functions.
 * Namely:
 * <ul>
 *   <li><code>sconf</code>, <code>scorf</code></li>
 *   <li><code>scond</code>, <code>scord</code></li>
 *   <li><code>sdcon</code>, <code>sdcor</code></li>
 *   <li><code>ddcon</code>, <code>ddcor</code></li>
 *   <li><code>sddcon</code>, <code>sddcor</code></li>
 * </ul>
 *
 * <p>C/C++ and FORTRAN wrapeprs for these functions are found
 * under the following directories. Hereby, ${MKL} location of
 * MKL version 8.0 or above:
 * <ul>
 *   <li>${MKL}/examples/vslc/essl/vsl_wrappers</li>
 *   <li>${MKL}/examples/vslf/essl/vsl_wrappers</li>
 * </ul>
 *
 * <p>For representing data sequences and series of sequences
 * with Java arrays, simple FORTRAN-like approach is assumed.
 * The arrays h[], x[], and y[] are used as memory reservoirs
 * for storing the data, and data location inside the arrays
 * is controlled by the parameters like inch, incx, incy -
 * similarly to the C/C++ and FORTRAN interfaces.
 */
final public class ESSL {
    /**
     * Instantiation isn't allowed.
     */
    private ESSL() {}

    /**
     * Bind to the native library.
     */
    static {
        System.loadLibrary("mkl_java_stubs");
    }

    //////////////////////////////////////////////////////////
    //
    // Convolution/correlation methods via Fourier transform:
    // - sconf, scorf: with full set of parameters
    // - sconf, scorf: without needless parameters
    //
    //////////////////////////////////////////////////////////

    /**
     * Interface to the native C/C++ function <code>sconf</code>
     * with all parameters - includine the needless 'aux' ones
     * <code>aux1</code>, <code>naux1</code>, <code>aux2</code>,
     * and <code>naux2</code>. The needless aux'es are ignored;
     * you may use <code>null</code> and 0 to fulfil them.
     *
     * <p>NB: Any call with <code>init</code>!= (initialization)
     * is ignored but still consumes lots of time because of the
     * JNI overhead. You may prefer to ommit such initialization
     * and immediately call with <code>init</code>==0.
     */
    native public static void sconf(int init,
        float h[], int inc1h,
        float x[], int inc1x, int inc2x,
        float y[], int inc1y, int inc2y,
        int nh, int nx, int m, int iy0, int ny,
        float aux1[], int naux1, float aux2[], int naux2);

    /**
     * Interface to the native C/C++ function <code>scorf</code>
     * with all parameters - includine the needless 'aux' ones
     * <code>aux1</code>, <code>naux1</code>, <code>aux2</code>,
     * and <code>naux2</code>. The needless aux'es are ignored;
     * you may use <code>null</code> and 0 to fulfil them.
     *
     * <p>NB: Any call with <code>init</code>!=0 (initialization)
     * is ignored but still consumes lots of time because of the
     * JNI overhead. You may prefer to ommit such initialization
     * and immediately call with <code>init</code>==0.
     */
    native public static void scorf(int init,
        float h[], int inc1h,
        float x[], int inc1x, int inc2x,
        float y[], int inc1y, int inc2y,
        int nh, int nx, int m, int iy0, int ny,
        float aux1[], int naux1, float aux2[], int naux2);

    /**
     * Interface to the native C/C++ function <code>sconf</code>
     * with all parameters - without the needless 'aux' ones
     * <code>aux1</code>, <code>naux1</code>, <code>aux2</code>,
     * and <code>naux2</code>.
     */
    public static void sconf(int init,
        float h[], int inc1h,
        float x[], int inc1x, int inc2x,
        float y[], int inc1y, int inc2y,
        int nh, int nx, int m, int iy0, int ny)
    {
        if (init != 0)
            return; // return immediately to avoid JNI overhead
        sconf(init,h,inc1h,x,inc1x,inc2x,y,inc1y,inc2y,nh,nx,m,iy0,ny,null,0,null,0);
    }

    /**
     * Interface to the native C/C++ function <code>scorf</code>
     * with all parameters - without the needless 'aux' ones
     * <code>aux1</code>, <code>naux1</code>, <code>aux2</code>,
     * and <code>naux2</code>.
     */
    public static void scorf(int init,
        float h[], int inc1h,
        float x[], int inc1x, int inc2x,
        float y[], int inc1y, int inc2y,
        int nh, int nx, int m, int iy0, int ny)
    {
        if (init != 0)
            return; // return immediately to avoid JNI overhead
        scorf(init,h,inc1h,x,inc1x,inc2x,y,inc1y,inc2y,nh,nx,m,iy0,ny,null,0,null,0);
    }

    //////////////////////////////////////////////////////////
    //
    // Convolution/correlation direct methods:
    // - scond, scord: w/o decimation of output
    // - sdcon, sdcor: with decimation of output
    // - ddcon, ddcor: decimation, double precision
    // - sddcon, sddcor: double precision calculations
    //
    //////////////////////////////////////////////////////////

    /**
     * Interface to the native C/C++ function <code>scond</code>.
     */
    native public static void scond(
        float h[], int inch,
        float x[], int incx,
        float y[], int incy,
        int nh, int nx, int iy0, int ny);

    /**
     * Interface to the native C/C++ function <code>scord</code>.
     */
    native public static void scord(
        float h[], int inch,
        float x[], int incx,
        float y[], int incy,
        int nh, int nx, int iy0, int ny);

    /**
     * Interface to the native C/C++ function <code>sdcon</code>.
     */
    native public static void sdcon(
        float h[], int inch,
        float x[], int incx,
        float y[], int incy,
        int nh, int nx, int iy0, int ny, int id);

    /**
     * Interface to the native C/C++ function <code>sdcor</code>.
     */
    native public static void sdcor(
        float h[], int inch,
        float x[], int incx,
        float y[], int incy,
        int nh, int nx, int iy0, int ny, int id);

    /**
     * Interface to the native C/C++ function <code>ddcon</code>.
     */
    native public static void ddcon(
        double h[], int inch,
        double x[], int incx,
        double y[], int incy,
        int nh, int nx, int iy0, int ny, int id);

    /**
     * Interface to the native C/C++ function <code>ddcor</code>.
     */
    native public static void ddcor(
        double h[], int inch,
        double x[], int incx,
        double y[], int incy,
        int nh, int nx, int iy0, int ny, int id);

    /**
     * Interface to the native C/C++ function <code>sddcon</code>.
     */
    native public static void sddcon(
        float h[], int inch,
        float x[], int incx,
        float y[], int incy,
        int nh, int nx, int iy0, int ny, int id);

    /**
     * Interface to the native C/C++ function <code>sddcor</code>.
     */
    native public static void sddcor(
        float h[], int inch,
        float x[], int incx,
        float y[], int incy,
        int nh, int nx, int iy0, int ny, int id);
}
