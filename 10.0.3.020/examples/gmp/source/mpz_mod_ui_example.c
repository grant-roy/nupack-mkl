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
	   mpz_t r, n;
       unsigned long int d, p;
	   char str2[] = "7400249944258160101216"; /*   = 11^21 + 5      */
/*                                                                   */
/*  Initialization of all multiple precision variables ( r, n )   */
/*         and assignment of data to entrance variables ( n, d )     */
/*                                                                   */
       mpz_init ( r );
       mpz_init ( n );
/*                                                                   */
/*         call mpz_set_str to set n = 7400249944258160101216        */
/*                                                                   */
	   mpz_set_str ( n, str2, 10 );
       d = 121;
/*                                                                   */
/*     Call mpz_mod_ui to set r and p to n mod d                     */
/*                                                                   */
       p = mpz_mod_ui( r, n, d );
/*                                                                   */
/*  Call mpz_get_str to convert r to a string of digits in base 10   */
/*              and printing of result ( r and p ) in the form of    */
/* r = 5                                                             */
/* p = 5                                                             */
/*                                                                   */
       mpz_get_str ( str2, 10, r );
       printf( " r = %s\n", str2 );
       printf( " p = %d\n", p );
/*                                                                   */
/*             Free the space occupied by r, n                       */
/*                                                                   */
       mpz_clear ( r );
       mpz_clear ( n );
       return 0;
}

