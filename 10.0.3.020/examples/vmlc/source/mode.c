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
!    vmlSetMode/vmlGetMode  Example Program Text
!******************************************************************************/

#include <stdio.h>
#include <math.h>
#include "func_interv.h"

#include "mkl_vml.h"

void PrintTextVmlMode(unsigned int );

int main()
{
  unsigned MKL_INT mode;

  printf("vmlSetMode/vmlGetMode example program\n\n");

  mode=(unsigned MKL_INT)vmlGetMode();
  printf("Default value of vmlMode: ");
  PrintTextVmlMode((unsigned int)mode);
  printf(" (0x%x)\n",(unsigned int)mode);

  mode=(unsigned MKL_INT)(VML_LA|VML_FLOAT_CONSISTENT|VML_ERRMODE_IGNORE);
  vmlSetMode(mode);
  mode=(unsigned MKL_INT)vmlGetMode();
  printf("Value of vmlMode after using vmlSetMode: ");
  PrintTextVmlMode((unsigned int)mode);
  printf(" (0x%x)\n",(unsigned int)mode);

  return 0;
}

void PrintTextVmlMode(unsigned int mode)
{
  switch(mode&VML_ACCURACY_MASK) {
    case 0x001: {
                printf("VML_LA");
                break;
    }
    case 0x002: {
                printf("VML_HA");
                break;
    }
  }
  switch(mode&VML_FPUMODE_MASK) {
    case 0x000: {
                printf(" | VML_DEFAULT_PRECISION");
                break;
    }
    case 0x010: {
                printf(" | VML_FLOAT_CONSISTENT");
                break;
    }
    case 0x020: {
                printf(" | VML_DOUBLE_CONSISTENT");
                break;
    }
    case 0x030: {
                printf(" | VML_RESTORE");
                break;
    }
  }
  if(mode&VML_ERRMODE_IGNORE) {
    printf(" | VML_ERRMODE_IGNORE");
  }
  else {
    if(mode&VML_ERRMODE_ERRNO) {
      printf(" | VML_ERRMODE_ERRNO");
    }
    if(mode&VML_ERRMODE_STDERR) {
      printf(" | VML_ERRMODE_STDERR");
    }
    if(mode&VML_ERRMODE_EXCEPT) {
      printf(" | VML_ERRMODE_EXCEPT");
    }
    if(mode&VML_ERRMODE_CALLBACK) {
      printf(" | VML_ERRMODE_CALLBACK");
    }
  }
}
