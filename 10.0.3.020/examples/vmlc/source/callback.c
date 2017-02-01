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
!    vmlSetErrorCallBack/vmlGetErrorCallBack/vmlClearErrorCallBack  Example
!    Program Text
!******************************************************************************/

#include <stdio.h>
#include <math.h>
#include "func_interv.h"

#include "mkl_vml.h"

int UserCallBack(DefVmlErrorContext* );

int main()
{
  VMLErrorCallBack errcb;
  double dbA = 0.0;
  double dbR;

  printf("Set/Get/Clear CallBack example program\n\n");

  errcb=vmlGetErrorCallBack();
  printf("Initial adress of CallBack function: 0x%p\n",errcb);

  errcb=UserCallBack;
  vmlSetErrorCallBack(errcb);
  errcb=vmlGetErrorCallBack();
  printf("Adress of CallBack function after using Set CallBack: 0x%p\n",errcb);
  printf("Test user callback on vdLn function\n");
  vdLn(1, &dbA, &dbR);


  vmlClearErrorCallBack();
  errcb=vmlGetErrorCallBack();
  printf("Adress of CallBack function after using Clear CallBack: 0x%p\n",errcb);

  return 0;
}

int UserCallBack(DefVmlErrorContext* pdefVmlErrorContext)
{
    printf("In function %s argument a[%d]=%f is wrong.\n",
      pdefVmlErrorContext->cFuncName,
      pdefVmlErrorContext->iIndex,
      pdefVmlErrorContext->dbA1);
    return 0;
}
