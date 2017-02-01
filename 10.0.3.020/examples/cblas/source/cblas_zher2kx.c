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
!      C B L A S _ Z H E R 2 K  Example Program Text ( C Interface )
!******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "mkl_example.h"

int main(int argc, char *argv[])
{
      FILE *in_file;
      char *in_file_name;

      MKL_INT         n, k;
      MKL_INT         lda, ldb, ldc;
      MKL_INT         rmaxa, cmaxa, rmaxb, cmaxb, rmaxc, cmaxc;
      MKL_Complex16   alpha;
      double          beta;
      MKL_Complex16  *a, *b, *c;
      CBLAS_ORDER     order;
      CBLAS_TRANSPOSE trans;
      CBLAS_UPLO      uplo;
      MKL_INT         ma, na, mb, nb, typeC;

      printf("\n     C B L A S _ Z H E R 2 K  EXAMPLE PROGRAM\n");

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
      if( GetIntegerParameters(in_file, &n, &k) != 2 ) {
          printf("\n ERROR of n, k reading\n");
          return 0;
      }
      if( GetScalarsZ(in_file, &alpha) != 1 ) {
          printf("\n ERROR of alpha reading\n");
          return 0;
      }
      if( GetScalarsD(in_file, &beta) != 1 ) {
          printf("\n ERROR of beta reading\n");
          return 0;
      }
      if( GetCblasCharParameters(in_file, &uplo, &trans, &order) != 3 ) {
          printf("\n ERROR of uplo, trans, order reading\n");
          return 0;
      }

      if( trans == CblasNoTrans ) {
          rmaxa = n + 1;
          cmaxa = k;
          rmaxb = n + 1;
          cmaxb = k;
          ma    = n;
          na    = k;
          mb    = n;
          nb    = k;
      } else {
          rmaxa = k + 1;
          cmaxa = n;
          rmaxb = k + 1;
          cmaxb = n;
          ma    = k;
          na    = n;
          mb    = k;
          nb    = n;
      }
      rmaxc = n + 1;
      cmaxc = n;
      a = (MKL_Complex16 *)calloc( rmaxa*cmaxa, sizeof(MKL_Complex16) );
      b = (MKL_Complex16 *)calloc( rmaxb*cmaxb, sizeof(MKL_Complex16) );
      c = (MKL_Complex16 *)calloc( rmaxc*cmaxc, sizeof(MKL_Complex16) );
      if ( a == NULL || b == NULL || c == NULL ) {
          printf("\n Can't allocate memory arrays");
          return 0;
      }
      if( order == CblasRowMajor ) {
         lda=cmaxa;
         ldb=cmaxb;
         ldc=cmaxc;
      } else {
         lda=rmaxa;
         ldb=rmaxb;
         ldc=rmaxc;
      }
      if( GetArrayZ(in_file, &order, 0, &ma, &na, a, &lda) != 0 ) {
        printf("\n ERROR of array A reading\n");
        return 0;
      }
      if( GetArrayZ(in_file, &order, 0, &mb, &nb, b, &ldb) != 0 ) {
        printf("\n ERROR of array B reading\n");
        return 0;
      }
      if (uplo == CblasUpper) typeC = UPPER_MATRIX;
      else                    typeC = LOWER_MATRIX;
      if( GetArrayZ(in_file, &order, typeC, &n, &n, c, &ldc) != 0 ) {
        printf("\n ERROR of array C reading\n");
        return 0;
      }
      fclose(in_file);

/*       Print input data                                      */

      printf("\n     INPUT DATA");
      printf("\n       N=%d  K=%d", n, k);
      printf("\n       ALPHA =(%4.1f,%4.1f)  BETA =%4.1f",
              alpha.real, alpha.imag, beta);
      PrintParameters("UPLO", uplo);
      PrintParameters("TRANS", trans);
      PrintParameters("ORDER", order);
      PrintArrayZ(&order, FULLPRINT, GENERAL_MATRIX, &ma, &na, a, &lda, "A");
      PrintArrayZ(&order, FULLPRINT, GENERAL_MATRIX, &mb, &nb, b, &ldb, "B");
      PrintArrayZ(&order, FULLPRINT, typeC, &n, &n, c, &ldc, "C");

/*      Call CBLAS_ZHER2K subroutine ( C Interface )           */

      cblas_zher2k(order, uplo, trans, n, k, &alpha, a, lda,
                   b, ldb, beta, c, ldc);

/*       Print output data                                     */

      printf("\n\n     OUTPUT DATA");
      PrintArrayZ(&order, FULLPRINT, typeC, &n, &n, c, &ldc, "C");

      free(a);
      free(b);
      free(c);

      return 0;
}

