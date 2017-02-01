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
!      C B L A S _ S G E R  Example Program Text ( C Interface )
!******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "mkl_example.h"

int main(int argc, char *argv[])
{
      FILE *in_file;
      char *in_file_name;

      MKL_INT         m, n, lda, incx, incy;
      MKL_INT         rmaxa, cmaxa;
      float           alpha;
      float          *a, *x, *y;
      CBLAS_ORDER     order;
      MKL_INT         len_x, len_y;

      printf("\n     C B L A S _ S G E R   EXAMPLE PROGRAM\n");

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
      if( GetIntegerParameters(in_file, &m, &n) != 2 ) {
          printf("\n ERROR of m, n reading\n");
          return 0;
      }
      if( GetIntegerParameters(in_file, &incx, &incy) != 2 ) {
          printf("\n ERROR of incx, incy reading\n");
          return 0;
      }
      if( GetScalarsS(in_file, &alpha) != 1 ) {
          printf("\n ERROR of alpha reading\n");
          return 0;
      }
      if( GetCblasCharParameters(in_file, &order) != 1 ) {
          printf("\n ERROR of order reading\n");
          return 0;
      }

      rmaxa = m + 1;
      cmaxa = n;
      len_x = 1+(m-1)*abs(incx);
      len_y = 1+(n-1)*abs(incy);
      a = (float *)calloc( rmaxa*cmaxa, sizeof(float) );
      x = (float *)calloc( len_x, sizeof(float) );
      y = (float *)calloc( len_y, sizeof(float) );
      if( a == NULL || x == NULL || y == NULL ) {
          printf( "\n Can't allocate memory for arrays\n");
          return 0;
      }
      if( order == CblasRowMajor )
         lda=cmaxa;
      else
         lda=rmaxa;

      if( GetVectorS(in_file, m, x, incx) != len_x ) {
        printf("\n ERROR of vector X reading\n");
        return 0;
      }
      if( GetVectorS(in_file, n, y, incy) != len_y ) {
        printf("\n ERROR of vector Y reading\n");
        return 0;
      }
      if( GetArrayS(in_file, &order, GENERAL_MATRIX, &m,  &n,  a, &lda) != 0 ) {
        printf("\n ERROR of array A reading\n");
        return 0;
      }
      fclose(in_file);

/*       Print input data                                      */

      printf("\n     INPUT DATA");
      printf("\n       M=%d  N=%d", m, n);
      printf("\n       ALPHA=%5.1f", alpha);
      PrintParameters("ORDER", order);
      PrintVectorS(FULLPRINT, m, x, incx, "X");
      PrintVectorS(FULLPRINT, n, y, incy, "Y");
      PrintArrayS(&order, FULLPRINT, GENERAL_MATRIX, &m, &n, a, &lda, "A");

/*      Call CBLAS_SGER subroutine ( C Interface )             */

      cblas_sger(order, m, n, alpha, x, incx, y, incy, a, lda);

/*       Print output data                                     */

      printf("\n\n     OUTPUT DATA");
      PrintArrayS(&order, FULLPRINT, GENERAL_MATRIX, &m, &n, a, &lda, "A");

      free(a);
      free(x);
      free(y);

      return 0;
}

