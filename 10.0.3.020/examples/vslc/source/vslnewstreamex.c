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
!    vslNewStreamEx  Example Program Text
!******************************************************************************/

#include <stdio.h>

#include "mkl_vsl.h"
#include "errcheck.inc"

#define SEED    7777777
#define N       1000

main()
{
    VSLStreamStatePtr stream;
    VSLStreamStatePtr streamEx;
    unsigned int seed;
    unsigned int seedEx[6];
    int r[N];
    int rEx[N];
    int i, err = 0, errcode;

    /***** Initialize seeds *****/
    seed = SEED;
    seedEx[0] = SEED;
    for(i = 1; i < 6; i++){
      seedEx[i] = 1;
    }

    /***** Initialize streams *****/
    errcode = vslNewStream  ( &stream,   VSL_BRNG_MRG32K3A,    (MKL_INT)seed );
    CheckVslError( errcode );
    errcode = vslNewStreamEx( &streamEx, VSL_BRNG_MRG32K3A, 6,          seedEx );
    CheckVslError( errcode );

    /***** Call RNGs *****/
    errcode = viRngUniformBits( 0, stream,   N, (unsigned int *)(r) );
    CheckVslError( errcode );
    errcode = viRngUniformBits( 0, streamEx, N, (unsigned int *)(rEx) );
    CheckVslError( errcode );

    /***** Compare results *****/
    for(i = 0; i < N; i++){
      if(r[i] != rEx[i])
          err++;
    }

    /***** Printing results *****/
    printf(" Sample of vslNewStreamEx\n");
    printf(" ------------------------\n\n");
    printf(" Parameters:\n");
    printf("    seed   =   %d\n",seed);
    printf("    seedEx = { %d %d %d %d %d %d }\n\n",
      seedEx[0],seedEx[1],seedEx[2],
      seedEx[3],seedEx[4],seedEx[5]);


    printf(" Results (first 10 of 1000):\n");
    printf(" ---------------------------\n");
    for(i=0;i<10;i++) {
        printf("r[%d]=0x%08X rEx[%d]=0x%08X\n",i,r[i],i,rEx[i]);
    }

    printf("\n");
    if(err) {
        printf("Error: %d values are incorrect!\n", err);
        return 1;
    }
    else {
        printf(" Results of ordinary and extended NewStream functions are identical.\n");
    }

    errcode = vslDeleteStream( &stream );
    CheckVslError( errcode );
    errcode = vslDeleteStream( &streamEx );
    CheckVslError( errcode );

    return 0;
}
