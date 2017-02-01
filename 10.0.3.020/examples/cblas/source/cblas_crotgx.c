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
!      C B L A S _ C R O T G   Example Program Text ( C Interface )
!******************************************************************************/

#include <stdio.h>

#include "mkl_example.h"

int main(int argc, char *argv[])
{
      FILE *in_file;
      char *in_file_name;

      float           c;
      MKL_Complex8    s, ca, cb;

      printf("\n     C B L A S _ C R O T G  EXAMPLE PROGRAM\n");

/*       Get input parameters                                  */

      if( argc == 1 ) {
         printf("\n You must specify in_file data file as 1-st parameter");
         return 0;
      }
      in_file_name = argv[1];

/*       Get input data                                        */

      if( (in_file = fopen( in_file_name, "r" )) == NULL ) {
         printf("\n ERROR on OPEN '%s' with mode=\"r\"\n", in_file_name);
         return 0;
      }
      if( GetScalarsC(in_file, &ca, &cb) != 2 ) {
          printf("\n ERROR of ca, cb reading\n");
          return 0;
      }
      fclose(in_file);

/*       Print input data                                      */

      printf("\n     INPUT DATA");
      printf("\n       CA=(%5.2f,%5.2f )  CB=(%5.2f,%5.2f)",
               ca.real, ca.imag, cb.real, cb.imag);

/*      Call CBLAS_CROTG subroutine ( C Interface )            */

      cblas_crotg(&ca, &cb, &c, &s);

/*       Print output data                                     */

      printf("\n\n     OUTPUT DATA");
      printf("\n       CA=(%6.3f,%6.3f)", ca.real, ca.imag);
      printf("\n        C=%6.3f   S=(%6.3f,%6.3f)", c, s.real,s.imag);

      return 0;
}

