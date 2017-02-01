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
!    vcPowx  Example Program Text
!******************************************************************************/

#include <stdio.h>
#include <math.h>
#include "func_interv.h"

#include "mkl_vml.h"

int main()
{
  MKL_Complex8 cA1[VEC_LEN],cA2,cB1[VEC_LEN];

  MKL_INT i=0,vec_len=VEC_LEN;

  cA2.real=(float)(__SPOWX_BEG+(__SPOWX_END-__SPOWX_BEG)/2);
  cA2.imag=(float)(__SPOWX_END-(__SPOWX_END-__SPOWX_BEG)/2);
  for(i=0;i<vec_len;i++) {
    cA1[i].real=(float)(__SPOWX_BEG+((__SPOWX_END-__SPOWX_BEG)*i)/vec_len);
    cA1[i].imag=(float)(__SPOWX_END-((__SPOWX_END-__SPOWX_BEG)*i)/vec_len);
    cB1[i].real=0.0;
    cB1[i].imag=0.0;
  }

  vcPowx(vec_len,cA1,cA2,cB1);

  printf("vcPowx test/example program\n\n");
  printf("           Argument                                     vcPowx\n");
  printf("===============================================================================\n");
  for(i=0;i<vec_len;i++) {
    printf("% .4f %+.4f*i  % .4f %+.4f*i      %.10f %+.10f*i\n",cA1[i].real,cA1[i].imag,cA2.real,cA2.imag,cB1[i].real,cB1[i].imag);
  }

  return 0;
}
