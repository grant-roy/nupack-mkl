/*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 2006-2008 Intel Corporation. All Rights Reserved.
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
!*******************************************************************************
*/

#include "stdio.h"
#include "mkl_gmp.h"

int main(int argc, char *argv[])
{
/*                                                                   */
/*     mpz_t is the C data type for multiple precision integer       */
/*                                                                   */
	   mpz_t n;
       unsigned long int b;
	   int p;
	   char str2[] = "7400249944258160101211"; /*   = 11^21          */
/*                                                                   */
/*  Initialization of all multiple precision variables ( n )         */
/*         and assignment of data to entrance variables ( n, b )     */
/*                                                                   */
       mpz_init ( n );
/*                                                                   */
/*         call mpz_set_str                                          */
/*              to set n = 7400249944258160101211                    */
/*                                                                   */
	   mpz_set_str ( n, str2, 10 );
       b = 10;
/*                                                                   */
/*          Call mpz_divisible_2exp_p to set p to 1 ( or 0 )         */
/*              if n is exactly divisible by 2^b  ( or not )         */
/*                                                                   */
       p = mpz_divisible_2exp_p( n, b);
/*                                                                   */
/*              Printing of result ( p ) in the form of              */
/* p = 0                                                             */
/*                                                                   */
       printf( " p = %d\n", p );
/*                                                                   */
/*             Free the space occupied by n                          */
/*                                                                   */
       mpz_clear ( n );
       return 0;
}