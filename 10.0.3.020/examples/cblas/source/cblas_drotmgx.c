/*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 1999-2008 Intel Corporation. All Rights Reserved.
!   The source code contained  or  described herein and all documents related to
!   the source code ("Material") are owned by Intel Corporation or its suppliers
!   or licensors.  Title to the  Material remains with  Intel Corporation or its
!   suppliers and licensors. The Material contains trade secrets and proprietary
!   and  confidential  information of  Intel or its suppliers and licensors. The
!   Material  is  protected  by  worldwide  copyright  and trade secret laws and
!   treaty  provisions. No part of the Material may be used, copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way without Intel's prior express written permission.
!   No license  under any  patent, copyright, trade secret or other intellectual
!   property right is granted to or conferred upon you by disclosure or delivery
!   of the Materials,  either expressly, by implication, inducement, estoppel or
!   otherwise.  Any  license  under  such  intellectual property  rights must be
!   express and approved by Intel in writing.
!
!*******************************************************************************
!  Content:
!      C B L A S _ D R O T M G   Example Program Text ( C Interface )
!******************************************************************************/

#include <stdio.h>

#include "mkl_example.h"

int main(int argc, char *argv[])
{
      FILE *in_file;
      char *in_file_name;

      double     param[5];
      double     dd1, dd2, dx1, dy1;

      param[0] = 0.0;
      param[1] = 0.0;
      param[2] = 0.0;
      param[3] = 0.0;
      param[4] = 0.0;

      printf("\n     C B L A S _ D R O T M G  EXAMPLE PROGRAM\n");

/*       Get input parameters                                  */

      if( argc == 1 ) {
         printf("\n You must specify in_file data file as 1-st parameter");
         return 0;
      }
      in_file_name = argv[1];

/*       Get input data                                       */

      if( (in_file = fopen( in_file_name, "r" )) == NULL ) {
         printf("\n ERROR on OPEN '%s' with mode=\"r\"\n", in_file_name);
         return 0;
      }
      if( GetScalarsD(in_file, &dd1, &dd2, &dx1, &dy1) != 4 ) {
          printf("\n ERROR of dd1, dd2, dx1, dy1 reading\n");
          return 0;
      }
      fclose(in_file);

/*       Print input data                                      */

      printf("\n     INPUT DATA");
      printf("\n       DD1=%5.2f  DD2=%5.2f  DX1=%5.2f  DY1=%5.2f",
               dd1, dd2, dx1, dy1);

/*      Call CBLAS_DROTMG subroutine ( C Interface )            */

      cblas_drotmg(&dd1, &dd2, &dx1, dy1, param);

/*       Print output data                                     */

      printf("\n\n     OUTPUT DATA");
      printf("\n       DD1=%6.3f  DD2=%6.3f  DX1=%6.3f",  dd1, dd2, dx1);
      PrintVectorD(SHORTPRINT, 5, param, 1, "PARAM");

      return 0;
}

