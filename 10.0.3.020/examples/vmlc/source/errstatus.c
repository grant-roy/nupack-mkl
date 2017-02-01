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
!    vmlSetErrStatus/vmlGetErrStatus/vmlClearErrStatus example program  Example
!    Program Text
!******************************************************************************/

#include <stdio.h>
#include <math.h>
#include "func_interv.h"

#include "mkl_vml.h"

void PrintTextVmlErrStatus(int );

int main()
{
  MKL_INT errst;

  printf("vmlSetErrStatus/vmlGetErrStatus/vmlClearErrStatus example program\n\n");

  errst=(MKL_INT)vmlGetErrStatus();

  printf("Initial value of vmlErrStatus: ");
  PrintTextVmlErrStatus((int)errst);
  printf(" (0x%x)\n",(int)errst);

  errst=(MKL_INT)(VML_STATUS_BADMEM);
  vmlSetErrStatus(errst);
  errst=(int)vmlGetErrStatus();
  printf("Value of vmlErrStatus after using vmlSetErrStatus: ");
  PrintTextVmlErrStatus((int)errst);
  printf(" (0x%x)\n",(int)errst);

  vmlClearErrStatus();
  errst=(MKL_INT)vmlGetErrStatus();
  printf("Value of vmlErrStatus after using vmlClearErrStatus: ");
  PrintTextVmlErrStatus((int)errst);
  printf(" (0x%x)\n",(int)errst);

  return 0;
}

void PrintTextVmlErrStatus(int errst)
{
  switch(errst) {
    case VML_STATUS_OK: {
                printf("VML_STATUS_OK");
                break;
    }
    case VML_STATUS_BADSIZE: {
                printf("VML_STATUS_BADSIZE");
                break;
    }
    case VML_STATUS_BADMEM: {
                printf("VML_STATUS_BADMEM");
                break;
    }
    case VML_STATUS_ERRDOM: {
                printf("VML_STATUS_ERRDOM");
                break;
    }
    case VML_STATUS_SING: {
                printf("VML_STATUS_SING");
                break;
    }
    case VML_STATUS_OVERFLOW: {
                printf("VML_STATUS_OVERFLOW");
                break;
    }
    case VML_STATUS_UNDERFLOW: {
                printf("VML_STATUS_UNDERFLOW");
                break;
    }
  }
}
