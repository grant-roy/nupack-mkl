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
       mpz_t fn, fnsub1;
       unsigned long int n;
	   char str1[] = "280571172992510140037611932413038677189525";
/*                                                                   */
/* Initialization of all multiple precision variables ( fn, fnsub1 ) */
/*     and assignment of data to entrance variables ( n )            */
/*                                                                   */
       mpz_init ( fn );
       mpz_init ( fnsub1 );
	   n = 200;
/*                                                                   */
/*     Call mpz_fib2_ui to set fn to F(n), n'th Fibonacci number     */
/*        and to set fnsub1 to F(n-1), (n-1)'th Fibonacci number     */
/*                                                                   */
       mpz_fib2_ui ( fn, fnsub1, n );
/*                                                                   */
/*  Call mpz_get_str to convert fn and fnsub1 to a string of digits  */
/*  in base 10 and printing of result ( fn, fnsub1 ) in the form of  */
/* fn = 280571172992510140037611932413038677189525                   */
/* fnsub1 = 173402521172797813159685037284371942044301               */
/*                                                                   */
       mpz_get_str ( str1, 10, fn );
	   printf ( " fn = %s\n", str1 );
       mpz_get_str ( str1, 10, fnsub1 );
	   printf ( " fnsub1 = %s\n", str1 );
/*                                                                   */
/*             Free the space occupied by fn                         */
/*                                                                   */
 	   mpz_clear ( fn );
 	   mpz_clear ( fnsub1 );
           return 0;
}
