/*******************************************************************************
!                             INTEL CONFIDENTIAL
!  Copyright(C) 2001-2008 Intel Corporation. All Rights Reserved.
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
!    vdRngGamma  Example Program Text
!******************************************************************************/

#include <stdio.h>

#include "mkl_vsl.h"
#include "errcheck.inc"

#define SEED    7777777
#define BRNG    VSL_BRNG_MT19937
#define METHOD  0
#define N       1000
#define NN      10

main()
{
    double r[N];
    VSLStreamStatePtr stream;
    int i, errcode;
    double alpha = 2.0, a=0.0, beta=1.0;

    double tM,tD,tQ,tD2;
    double sM,sD;
    double sum, sum2;
    double n,s;
    double DeltaM,DeltaD;

    /***** Initialize *****/
    errcode = vslNewStream( &stream, BRNG,  SEED );
    CheckVslError( errcode );

    /***** Call RNG *****/
    errcode = vdRngGamma( METHOD, stream, N, r, alpha, a, beta );
    CheckVslError( errcode );

    /***** Theoretical moments *****/
    tM=a+beta * alpha;
    tD=beta * beta * alpha;
    tQ=beta * beta * beta * beta * 3 * alpha * (alpha+2);

    /***** Sample moments *****/
    sum=0.0;
    sum2=0.0;
    for(i=0;i<N;i++) {
        sum+=(double)r[i];
        sum2+=(double)r[i]*(double)r[i];
    }
    sM=sum/((double)N);
    sD=sum2/(double)N-(sM*sM);

    /***** Comparison of theoretical and sample moments *****/
    n=(double)N;
    tD2=tD*tD;
    s=((tQ-tD2)/n)-(2*(tQ-2*tD2)/(n*n))+((tQ-3*tD2)/(n*n*n));

    DeltaM=(tM-sM)/sqrt(tD/n);
    DeltaD=(tD-sD)/sqrt(s);

    /***** Printing results *****/
    printf("Sample of vdRngGamma.\n");
    printf("----------------------\n\n");
    printf("Parameters:\n");
    printf("    alpha=%.4f\n",alpha);
    printf("    a=%.4f\n",a);
    printf("    beta=%.4f\n\n",beta);

    printf("Results (first 10 of 1000):\n");
    printf("---------------------------\n");
    for(i=0;i<NN;i++) {
        printf("r[%d]=%.4f\n",i,r[i]);
    }

    printf("\n");
    if(DeltaM>3.0 || DeltaD>3.0) {
        printf("Error: sample moments (mean=%.2f, variance=%.2f) are disagreed with theory (mean=%.2f, variance=%.2f).\n",sM,sD,tM,tD);
        return 1;
    }
    else {
        printf("Sample moments (mean=%.2f, variance=%.2f) are agreed with theory (mean=%.2f, variance=%.2f).\n",sM,sD,tM,tD);
    }

    printf("\n");

    /***** Deinitialize *****/
    errcode = vslDeleteStream( &stream );
    CheckVslError( errcode );

    return 0;
}