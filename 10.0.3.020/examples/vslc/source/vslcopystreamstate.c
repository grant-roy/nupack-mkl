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
!    vslCopyStreamState  Example Program Text
!******************************************************************************/

#include <stdio.h>

#include "mkl_vsl.h"
#include "errcheck.inc"

#define SEED    7777777
#define N       1000

main()
{
    VSLStreamStatePtr stream;
    VSLStreamStatePtr streamCpy;
    unsigned int seedCpy;
    unsigned int seed[6];
    int r[N];
    int rCpy[N];
    int i, err = 0, errcode;

    /***** Initialize seeds *****/
    seedCpy = 1;
    seed[0] = SEED;
    for(i = 1; i < 6; i++){
      seed[i] = seed[i-1]+11;
    }

    /***** Initialize streams *****/
    errcode = vslNewStreamEx( &stream,    VSL_BRNG_MRG32K3A, 6,          seed );
    CheckVslError( errcode );
    errcode = vslNewStream  ( &streamCpy, VSL_BRNG_MRG32K3A,    (MKL_INT)seedCpy );
    CheckVslError( errcode );
    errcode = vslCopyStreamState ( streamCpy, stream );
    CheckVslError( errcode );


    /***** Call RNGs *****/
    errcode = viRngUniformBits( 0, stream,   N, (unsigned int *)(r) );
    CheckVslError( errcode );
    errcode = viRngUniformBits( 0, streamCpy, N, (unsigned int *)(rCpy) );
    CheckVslError( errcode );

    /***** Compare results *****/
    for(i = 0; i < N; i++){
      if(r[i] != rCpy[i])
          err++;
    }

    /***** Printing results *****/
    printf(" Sample of vslCopyStreamState\n");
    printf(" ----------------------------\n\n");
    printf(" Parameters:\n");
    printf("    seed    = { %d %d %d %d %d %d }\n",
      seed[0],seed[1],seed[2],
      seed[3],seed[4],seed[5]);
    printf("    seedCpy =   %d\n\n",seedCpy);


    printf(" Results (first 10 of 1000):\n");
    printf(" ---------------------------\n");
    for(i=0;i<10;i++) {
        printf("r[%d]=0x%08X rCpy[%d]=0x%08X\n",i,r[i],i,rCpy[i]);
    }

    printf("\n");
    if(err) {
        printf("Error: %d values are incorrect!\n", err);
        return 1;
    }
    else {
        printf(" Results of original stream and its copy are identical.\n");
    }

    errcode = vslDeleteStream( &stream );
    CheckVslError( errcode );
    errcode = vslDeleteStream( &streamCpy );
    CheckVslError( errcode );

    return 0;
}
