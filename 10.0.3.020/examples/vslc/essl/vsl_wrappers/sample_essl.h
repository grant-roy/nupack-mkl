/*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 2006-2008 Intel Corporation. All Rights Reserved.
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

#ifndef _SAMPLE_ESSL_H_
#define _SAMPLE_ESSL_H_

#ifdef __cplusplus
extern "C" {
#endif

/*
// (A) Convolution/correlation via Fourier transfrom
*/

void sconf(
    int init,
    float h[], int inc1h,
    float x[], int inc1x, int inc2x,
    float y[], int inc1y, int inc2y,
    int nh, int nx, int m, int iy0, int ny,
    void* aux1, int naux1, void* aux2, int naux2);

void scorf(
    int init,
    float h[], int inc1h,
    float x[], int inc1x, int inc2x,
    float y[], int inc1y, int inc2y,
    int nh, int nx, int m, int iy0, int ny,
    void* aux1, int naux1, void* aux2, int naux2);

/*
// (B) Convolution/correlation via direct method
*/

void scond(
    float h[], int inch,
    float x[], int incx,
    float y[], int incy,
    int nh, int nx, int iy0, int ny);

void scord(
    float h[], int inch,
    float x[], int incx,
    float y[], int incy,
    int nh, int nx, int iy0, int ny);

/*
// (C) Convolution/correlation via direct method
//     with decimation of the output sequence
*/

/* (C.1) single precision */

void sdcon(
    float h[], int inch,
    float x[], int incx,
    float y[], int incy,
    int nh, int nx, int iy0, int ny, int id);

void sdcor(
    float h[], int inch,
    float x[], int incx,
    float y[], int incy,
    int nh, int nx, int iy0, int ny, int id);

/* (C.2) single precision data, but double precision calculations */

void sddcon(
    float h[], int inch,
    float x[], int incx,
    float y[], int incy,
    int nh, int nx, int iy0, int ny, int id);

void sddcor(
    float h[], int inch,
    float x[], int incx,
    float y[], int incy,
    int nh, int nx, int iy0, int ny, int id);

/* (C.3) double precision data and calculations */

void ddcon(
    double h[], int inch,
    double x[], int incx,
    double y[], int incy,
    int nh, int nx, int iy0, int ny, int id);

void ddcor(
    double h[], int inch,
    double x[], int incx,
    double y[], int incy,
    int nh, int nx, int iy0, int ny, int id);

#ifdef __cplusplus
}
#endif

#endif/*_SAMPLE_ESSL_H_*/
