/*******************************************************************************
!                             INTEL CONFIDENTIAL
!  Copyright(C) 2006-2008 Intel Corporation. All Rights Reserved.
!  The source code contained  or  described herein and all documents related to
!  the source code ("Material") are owned by Intel Corporation or its suppliers
!  or licensors.  Title to the  Material remains with  Intel Corporation or its
!  suppliers and licensors. The Material contains trade secrets and proprietary
!  and  confidential  information of  Intel or its suppliers and licensors. The
!  Material  is  protected  by  worldwide  copyright  and trade secret laws and
!  treaty  provisions. No part of the Material may be used, copied, reproduced,
!  modified, published, uploaded, posted, transmitted, distributed or disclosed
!  in any way without Intel's prior express written permission.
!  No license  under any  patent, copyright, trade secret or other intellectual
!  property right is granted to or conferred upon you by disclosure or delivery
!  of the Materials,  either expressly, by implication, inducement, estoppel or
!  otherwise.  Any  license  under  such  intellectual property  rights must be
!  express and approved by Intel in writing.
!
!*******************************************************************************
!  Content:
!    scorf  Example Program Text
!******************************************************************************/

#include "sample_essl.h"

#include <stdio.h>
#include <math.h>

int main()
{
    int    inch, incx, incy;
    int    nh,   nx,   ny;
    int    iy0;
    float  h[3], x[15], y[24];
    float  aux1[31], aux2[31];
    int    init, m,   naux1, naux2;
    int    inc2x, inc2y;
    int    i;
    double r10;
    int    r1[6] = { 2, 5, 8, 11, 14, 5 };
/************* Initialize data *****/
    r10 = 2.0e-5;
    inch = 2;
    incx = 3;
    incy = 4;
    nh   = 2;
    nx   = 5;
    ny   = 6;
    for ( i = 0; i < nh; i++ ) h[i*inch] = (float)(i+1);
    printf("\n");
    for ( i = 0; i < nx; i++ ) x[i*incx] = (float)(i+1);
    m     = 1;
    naux1 = 32;
    naux2 = 32;
    inc2x = 1;
    inc2y = 1;
    iy0   = -1;

    /****** 1-st step of Sample( preliminary ) **********/

    init = 1;

    /***** Call scorf *****/

    scorf( init, h, inch, x, incx, inc2x, y, incy, inc2y, nh, nx, m, iy0, ny, aux1, naux1, aux2, naux2 );

    /****** 2-nd step of Sample( main ) **********/

    init = 0;

    /***** Call scorf *****/

    scorf( init, h, inch, x, incx, inc2x, y, incy, inc2y, nh, nx, m, iy0, ny, aux1, naux1, aux2, naux2 );

    /***** Printing results *****/

    printf(" Sample of scorf.\n");
    printf("----------------------\n");
    printf("Parameters:\n");
    printf("    inch = %4d, incx = %4d, incy = %4d \n", inch, incx, incy );
    printf("    nh   = %4d, nx   = %4d, ny   = %4d \n", nh, nx, ny );
    printf("    iy0  = %4d \n\n", iy0 );
    for( i=0; i < nh; i++ ) printf("h[%3d ] = %4f\n",i*inch,h[i*inch]);
    printf("\n");
    for( i=0; i < nx; i++ ) printf("x[%3d ] = %4f\n",i*incx,x[i*incx]);
    printf("\n");

    printf("Results:\n");
    printf("---------------------------\n");
    for( i=0; i < ny; i++ ) printf("y[%3d ] = %4f\n",i*incy,y[i*incy]);
    for( i = 0; i < 6; i++ ) {
        if(fabs(y[i*incy]-r1[i]) > r10) {
             printf("ERROR: wrong result: i=%d, y[i*incy]=%g\n",i,y[i*incy]);
             printf("---------------------------\n");
             printf(" TEST FAILED\n");
             printf("---------------------------\n");
             return 1;
         }
    }
    printf("\n");
    printf("---------------------------\n");
    printf(" TEST PASSED\n");
    printf("---------------------------\n");

    return 0;
}
