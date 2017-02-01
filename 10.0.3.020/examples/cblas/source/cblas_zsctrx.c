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
!      C B L A S _ Z S C T R  Example Program Text ( C Interface )
!******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "mkl_example.h"

int main(int argc, char *argv[])
{
      FILE *in_file;
      char *in_file_name;

      MKL_INT         n;
      MKL_INT        *indx;
      MKL_Complex16  *x, *y;

      MKL_INT         indmax;

      printf("\n     C B L A S _ Z S C T R  EXAMPLE PROGRAM\n");

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
      if( GetIntegerParameters(in_file, &n) != 1 ) {
          printf("\n ERROR of n reading\n");
          return 0;
      }

      x    = (MKL_Complex16 *)calloc( n, sizeof( MKL_Complex16 ) );
      indx = (MKL_INT *)calloc( n, sizeof( MKL_INT ) );
      if( x == NULL || indx == NULL ) {
          printf("\n Can't allocate memory for arrays\n");
          return 0;
      }
      if( GetVectorZ(in_file, n, x, 1) != n ) {
        printf("\n ERROR of vector X reading\n");
        return 0;
      }
      if( GetVectorI(in_file, n, indx) != n ) {
        printf("\n ERROR of vector INDX reading\n");
        return 0;
      }
      indmax = MaxValue(n, indx)+1;
      y    = (MKL_Complex16 *)calloc( indmax, sizeof( MKL_Complex16 ) );
      if( y == NULL ) {
          printf( "\n Can't allocate memory for arrays\n");
          return 0;
      }
      fclose(in_file);

/*       Print input data                                      */

      printf("\n     INPUT DATA");
      printf("\n       N=%d", n);
      PrintVectorZ(SHORTPRINT, n, x, 1, "X");
      PrintVectorI(n, indx, "INDX");

/*      Call CBLAS_ZSCTR subroutine ( C Interface )            */

      cblas_zsctr(n, x, indx, y);

/*       Print output data                                     */

      printf("\n\n     OUTPUT DATA");
      PrintVectorZ(SHORTPRINT, indmax, y, 1, "Y");

      free(x);
      free(indx);
      free(y);

      return 0;
}

