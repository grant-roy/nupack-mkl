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
!    vdAcos  Example Program Text
!******************************************************************************/

#include <stdio.h>
#include <math.h>
#include "func_interv.h"

#include "mkl_vml.h"

int main()
{
  double dA[VEC_LEN],dB1[VEC_LEN],dB2[VEC_LEN];

  MKL_INT i=0,vec_len=VEC_LEN;
  double CurRMS,MaxRMS=0.0;

  for(i=0;i<vec_len;i++) {
    dA[i]=(double)(__DACOS_BEG+((__DACOS_END-__DACOS_BEG)*i)/vec_len);
    dB1[i]=0.0;
    dB2[i]=acos(dA[i]);
  }

  vdAcos(vec_len,dA,dB1);

  printf("vdAcos test/example program\n\n");
  printf("           Argument                     vdAcos                      Acos\n");
  printf("===============================================================================\n");
  for(i=0;i<vec_len;i++) {
    printf("% 25.14f % 25.14f % 25.14f\n",dA[i],dB1[i],dB2[i]);
    CurRMS=fabs((dB1[i]-dB2[i])/(0.5*(dB1[i]+dB2[i])));
    if(MaxRMS<CurRMS) MaxRMS=CurRMS;
  }
  printf("\n");
  if(MaxRMS>=DEPS) {
    printf("Error! Relative accuracy is %.2f\n",MaxRMS);
    return 1;
  }
  else {
    printf("Relative accuracy is %.2f\n",MaxRMS);
  }

  return 0;
}
