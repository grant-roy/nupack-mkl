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
!      C B L A S _ D T P S V  Example Program Text ( C Interface )
!******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "mkl_example.h"

int main(int argc, char *argv[])
{
      FILE *in_file;
      char *in_file_name;

      MKL_INT         n, incx;
      MKL_INT         apmax;
      double         *ap, *x;
      CBLAS_ORDER     order;
      CBLAS_UPLO      uplo;
      CBLAS_TRANSPOSE trans;
      CBLAS_DIAG      diag;
      MKL_INT         len_x;

      printf("\n     C B L A S _ D T P S V  EXAMPLE PROGRAM\n");

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
      if( GetIntegerParameters(in_file, &n) != 1 ) {
          printf("\n ERROR of n reading\n");
          return 0;
      }
      if( GetIntegerParameters(in_file, &incx) != 1 ) {
          printf("\n ERROR of incx reading\n");
          return 0;
      }
      if( GetCblasCharParameters(in_file, &uplo, &trans, &diag, &order) != 4 ) {
          printf("\n ERROR of uplo, trans, diag, order reading\n");
          return 0;
      }

      apmax = (n*(n+1))/2;
      len_x = 1+(n-1)*abs(incx);
      ap = (double *)calloc( apmax, sizeof(double) );
      x  = (double *)calloc( len_x, sizeof(double) );
      if( ap == NULL || x == NULL ) {
          printf( "\n Can't allocate memory for arrays\n");
          return 0;
      }
      if( GetVectorD(in_file, n, x, incx) != len_x ) {
        printf("\n ERROR of vector X reading\n");
        return 0;
      }
      if( GetVectorD(in_file, (n*(n+1))/2, ap, 1) != (n*(n+1))/2 ) {
        printf("\n ERROR of vector AP reading\n");
        return 0;
      }
      fclose(in_file);

/*       Print input data                                      */

      printf("\n     INPUT DATA");
      printf("\n       N=%d", n);
      PrintParameters("UPLO, DIAG", uplo, diag);
      PrintParameters("TRANS", trans);
      PrintParameters("ORDER", order);
      PrintVectorD(FULLPRINT, n, x, incx, "X");
      PrintVectorD(SHORTPRINT, (n*(n+1))/2, ap, 1, "AP");

/*      Call CBLAS_DTPSV subroutine ( C Interface )            */

      cblas_dtpsv(order, uplo, trans, diag, n, ap, x, incx);

/*       Print output data                                     */

      printf("\n\n     OUTPUT DATA");
      PrintVectorD(FULLPRINT, n, x, incx, "X");

      free(ap);
      free(x);

      return 0;
}
