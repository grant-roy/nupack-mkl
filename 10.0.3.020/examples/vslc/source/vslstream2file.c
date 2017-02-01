/*******************************************************************************
!                             INTEL CONFIDENTIAL
!  Copyright(C) 2003-2008 Intel Corporation. All Rights Reserved.
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
!    stream2file functions  Example Program Text
!******************************************************************************/

#include <stdio.h>
#include "mkl_vsl.h"
#include "errcheck.inc"

/* Quantity of random numbers to generate */
#define N 10

static float r_orig[N], r_load[N];

main(void)
{
    VSLStreamStatePtr stream;
    int i, errcode;

    /* Create the original stream to be saved in a file */
    errcode = vslNewStream(&stream, VSL_BRNG_R250, 7777777);
    CheckVslError( errcode );

    /* Save original stream to a file */
    errcode = vslSaveStreamF(stream, "vslstream2file.dat");
    CheckVslError( errcode );

    /* Generate random numbers using original stream */
    errcode = vsRngUniform(0, stream, N, r_orig, 0.0f, 1.0f);
    CheckVslError( errcode );

    /* Delete original stream */
    errcode = vslDeleteStream(&stream);
    CheckVslError( errcode );

    /* Load stream that is saved in a file */
    errcode = vslLoadStreamF(&stream, "vslstream2file.dat");
    CheckVslError( errcode );

    /* Generate random numbers using the stream loaded from file */
    errcode = vsRngUniform(0, stream, N, r_load, 0.0f, 1.0f);
    CheckVslError( errcode );

    /* Delete stream loaded from file */
    errcode = vslDeleteStream(&stream);
    CheckVslError( errcode );

    /* Compare random numbers from original and loaded stream.
       Must be identical */
    for ( i=0; i<N; i++ )
    {
        printf("r_orig[%d]=%f\tr_load[%d]=%f\n", i, r_orig[i], i, r_load[i]);
        if ( r_orig[i] != r_load[i] )
        {
            /* Here if results are not identical */
            printf("Error: Loaded stream differs from original stream.\n");
            return 1;
        }
    }

    /* Here if results are identical */
    printf("PASS: Loaded stream identical with original stream.\n");

    return 0;
}
