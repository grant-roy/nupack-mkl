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
!      C B L A S _ D G B M V  Example Program Text ( C Interface )
!******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "mkl_example.h"
#include "mkl_cblas.h"

int main(int argc, char *argv[])
{
      FILE *in_file;
      char *in_file_name;

      MKL_INT         m, n, kl, ku, lda, incx, incy;
      MKL_INT         rmaxa, cmaxa;
      double          alpha, beta;
      double         *a, *x, *y;
      CBLAS_ORDER     order;
      CBLAS_TRANSPOSE trans;
      MKL_INT         nx, ny, len_x, len_y;

      printf("\n     C B L A S _ D G B M V  EXAMPLE PROGRAM\n");

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
      if( GetIntegerParameters(in_file, &m, &n, &kl, &ku) != 4 ) {
          printf("\n ERROR of m, n, kl, ku reading\n");
          return 0;
      }
      if( GetIntegerParameters(in_file, &incx, &incy) != 2 ) {
          printf("\n ERROR of incx, incy reading\n");
          return 0;
      }
      if( GetScalarsD(in_file, &alpha, &beta) != 2 ) {
          printf("\n ERROR of alpha, beta reading\n");
          return 0;
      }
      if( GetCblasCharParameters(in_file, &trans, &order) != 2 ) {
          printf("\n ERROR of trans, order reading\n");
          return 0;
      }

      rmaxa = kl + ku + 1;
      if( order == CblasRowMajor )
          cmaxa = m;
      else
          cmaxa = n;
      a = (double *)calloc( rmaxa*cmaxa, sizeof(double) );
      if( trans == CblasNoTrans ) {
         nx = n;
         ny = m;
      } else {
         nx = m;
         ny = n;
      }
      len_x = 1+(nx-1)*abs(incx);
      len_y = 1+(ny-1)*abs(incy);
      x = (double *)calloc( len_x, sizeof(double) );
      y = (double *)calloc( len_y, sizeof(double) );
      if( a == NULL || x == NULL || y == NULL ) {
          printf( "\n Can't allocate memory for arrays\n");
          return 0;
      }

      if( GetVectorD(in_file, nx, x, incx) != len_x ) {
        printf("\n ERROR of vector X reading\n");
        return 0;
      }
      if( GetVectorD(in_file, ny, y, incy) != len_y ) {
        printf("\n ERROR of vector Y reading\n");
        return 0;
      }
      lda=rmaxa;
      if( GetBandArrayD(in_file, &order, kl, ku, m, n, a, lda) != 0 ) {
        printf("\n ERROR of array A reading\n");
        return 0;
      }
      fclose(in_file);

/*       Print input data                                      */
      printf("\n     INPUT DATA");
      printf("\n       M=%d  N=%d", m, n);
      printf("\n       ALPHA =%4.1f  BETA =%4.1f", alpha, beta);
      PrintParameters("TRANS", trans);
      PrintParameters("ORDER", order);
      PrintVectorD(FULLPRINT, nx, x, incx, "X");
      PrintVectorD(FULLPRINT, ny, y, incy, "Y");
      PrintBandArrayD(&order, FULLPRINT, kl, ku, m, n, a, lda, "A");

/*      Call CBLAS_DGBMV subroutine ( C Interface )            */

      cblas_dgbmv(order, trans, m, n, kl, ku, alpha, a, lda,
                  x, incx, beta, y, incy);

/*       Print output data                                     */

      printf("\n\n     OUTPUT DATA");
      PrintVectorD(FULLPRINT, ny, y, incy, "Y");

      free(a);
      free(x);
      free(y);

      return 0;
}

