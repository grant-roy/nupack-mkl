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
!    viRngNegbinomial  Example Program Text
!******************************************************************************/

#include <stdio.h>
#include <math.h>

#include "mkl_vsl.h"
#include "errcheck.inc"

#define SEED    1
#define BRNG    VSL_BRNG_MCG31
#define METHOD  0
#define N       1000
#define NN      10

main()
{
    int r[N];
    VSLStreamStatePtr stream;
    int i, errcode;
    double a=1.0,p=0.75;

    double tM,tD,tQ,tD2;
    double sM,sD;
    double sum, sum2;
    double n,s;
    double DeltaM,DeltaD;

    /***** Initialize *****/
    errcode = vslNewStream( &stream, BRNG,  SEED );
    CheckVslError( errcode );

    /***** Call RNG *****/
    errcode = viRngNegbinomial( METHOD, stream, N, r, a, p );
    CheckVslError( errcode );

    /***** Theoretical moments *****/
    tM=a*(1-p)/p;
    tD=a*(1-p)/(p*p);
    tQ=(a*(1-p)*(p*p-3*a*p-6*p+3*a+6))/(p*p*p*p);

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
    printf("Sample of viRngNegbinomial.\n");
    printf("---------------------------\n\n");
    printf("Parameters:\n");
    printf("    a=%.4f\n",a);
    printf("    p=%.4f\n\n",p);

    printf("Results (first 10 of 1000):\n");
    printf("---------------------------\n");
    for(i=0;i<NN;i++) {
        printf("r[%d]=%d\n",i,r[i]);
    }

    printf("\n");
    if(DeltaM>3.0 || DeltaD>3.0) {
        printf("Error: sample moments (mean=%.2f, variance=%.2f) are disagreed with theory (mean=%.2f, variance=%.2f).\n",sM,sD,tM,tD);
        return 1;
    }
    else {
        printf("Sample moments (mean=%.2f, variance=%.2f) are agreed with theory (mean=%.2f, variance=%.2f).\n",sM,sD,tM,tD);
    }

    /***** Deinitialize *****/
    errcode = vslDeleteStream( &stream );
    CheckVslError( errcode );

    return 0;
}
