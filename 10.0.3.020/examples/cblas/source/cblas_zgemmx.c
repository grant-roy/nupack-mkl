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
!      C B L A S _ Z G E M M  Example Program Text ( C Interface )
!******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "mkl_example.h"

int main(int argc, char *argv[])
{
      FILE *in_file;
      char *in_file_name;

      MKL_INT         m, n, k;
      MKL_INT         lda, ldb, ldc;
      MKL_INT         rmaxa, cmaxa, rmaxb, cmaxb, rmaxc, cmaxc;
      MKL_Complex16   alpha, beta;
      MKL_Complex16  *a, *b, *c;
      CBLAS_ORDER     order;
      CBLAS_TRANSPOSE transA, transB;
      MKL_INT         ma, na, mb, nb;

      printf("\n     C B L A S _ Z G E M M  EXAMPLE PROGRAM\n");

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
      if( GetIntegerParameters(in_file, &m, &n, &k) != 3 ) {
          printf("\n ERROR of m, n, k reading\n");
          return 0;
      }
      if( GetScalarsZ(in_file, &alpha, &beta) != 2 ) {
          printf("\n ERROR of alpha, beta reading\n");
          return 0;
      }
      if( GetCblasCharParameters(in_file, &transA, &transB, &order) != 3 ) {
          printf("\n ERROR of transA, transB, order reading\n");
          return 0;
      }

      if( transA == CblasNoTrans ) {
          rmaxa = m + 1;
          cmaxa = k;
          ma    = m;
          na    = k;
      } else {
          rmaxa = k + 1;
          cmaxa = m;
          ma    = k;
          na    = m;
      }
      if( transB == CblasNoTrans ) {
          rmaxb = k + 1;
          cmaxb = n;
          mb    = k;
          nb    = n;
      } else {
          rmaxb = n + 1;
          cmaxb = k;
          mb    = n;
          nb    = k;
      }
      rmaxc = m + 1;
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
      if( GetArrayZ(in_file, &order, GENERAL_MATRIX, &ma, &na, a, &lda) != 0 ) {
        printf("\n ERROR of array A reading\n");
        return 0;
      }
      if( GetArrayZ(in_file, &order, GENERAL_MATRIX, &mb, &nb, b, &ldb) != 0 ) {
        printf("\n ERROR of array B reading\n");
        return 0;
      }
      if( GetArrayZ(in_file, &order, GENERAL_MATRIX, &m,  &n,  c, &ldc) != 0 ) {
        printf("\n ERROR of array C reading\n");
        return 0;
      }
      fclose(in_file);

/*       Print input data                                      */

      printf("\n     INPUT DATA");
      printf("\n       M=%d  N=%d  K=%d", m, n, k);
      printf("\n       ALPHA =(%5.1f,%5.1f )  BETA =(%5.1f,%5.1f )",
              alpha.real, alpha.imag, beta.real, beta.imag);
      PrintParameters("TRANSA, TRANSB", transA, transB);
      PrintParameters("ORDER", order);
      PrintArrayZ(&order, FULLPRINT, GENERAL_MATRIX, &ma, &na, a, &lda, "A");
      PrintArrayZ(&order, FULLPRINT, GENERAL_MATRIX, &mb, &nb, b, &ldb, "B");
      PrintArrayZ(&order, FULLPRINT, GENERAL_MATRIX, &m, &n, c, &ldc, "C");

/*      Call ZGEMM subroutine ( C Interface )                  */

      cblas_zgemm(order, transA, transB, m, n, k, &alpha,
                  a, lda, b, ldb, &beta, c, ldc);

/*       Print output data                                     */

      printf("\n\n     OUTPUT DATA");
      PrintArrayZ(&order, FULLPRINT, GENERAL_MATRIX, &m, &n, c, &ldc, "C");

      free(a);
      free(b);
      free(c);

      return 0;
}

