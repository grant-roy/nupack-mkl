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
       mpz_t g, s, t, a, b;
	   char str1[] = "672749994932560009201";  /*   = 11^20          */
	   char str2[] = "7400249944258160101211"; /*   = 11^21          */
/*                                                                   */
/*  Initialization of all multiple precision variables ( g,s,t,a,b ) */
/*     and assignment of data to entrance variables ( a, b )         */
/*                                                                   */
       mpz_init ( g );
       mpz_init ( s );
       mpz_init ( t );
       mpz_init ( a );
       mpz_init ( b );
/*                                                                   */
/*     Call mpz_set_str to set a = 672749994932560009201             */
/*                      to set b = 7400249944258160101211            */
/*                                                                   */
	   mpz_set_str ( a, str1, 10 );
	   mpz_set_str ( b, str2, 10 );
/*                                                                   */
/*                  Call mpz_gcdext                                  */
/*     to set g to the greatest common divisor of a and b            */
/* and in addition set s and t to coefficients satisfying as+bt = g  */
/*                                                                   */
       mpz_gcdext ( g, s, t, a, b );
/*                                                                   */
/*   Call mpz_get_str to convert ( g, s, t ) to a string of digits   */
/*    in base 10 and printing of result ( g, s, t ) in the form of   */
/* g =  672749994932560009201                                        */
/* s =  1                                                            */
/* t =  0                                                            */
/*                                                                   */
       mpz_get_str ( str1, 10, g );
       printf ( " g = %s\n", str1 );
       mpz_get_str ( str1, 10, s );
       printf ( " s = %s\n", str1 );
       mpz_get_str ( str1, 10, t );
       printf ( " t = %s\n", str1 );
/*                                                                   */
/*             Free the space occupied by rop, op1, op2              */
/*                                                                   */
       mpz_clear ( g );
       mpz_clear ( s );
       mpz_clear ( t );
       mpz_clear ( a );
       mpz_clear ( b );
       return 0;
}
