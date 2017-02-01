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
       mpz_t q, r, n, d;
       char str[] = "340282366920938463463374607431768211457";
/*                                                                   */
/* Initialization of all multiple precision variables ( q, r, n, d ) */
/*         and assignment of data to entrance variables ( n, d )     */
/*                                                                   */
       mpz_init ( q );
       mpz_init ( r );
       mpz_init ( n );
       mpz_init ( d );
/*                                                                   */
/*Call mpz_set_str to set n = 340282366920938463463374607431768211457*/
/*                    and d = 18446744073709551616                   */
/*                                                                   */
	   mpz_set_str ( n, str, 10);
       mpz_set_str ( d, "18446744073709551616", 10);
/*                                                                   */
/*  Call mpz_tdiv_qr to divide n by d, forming a quotient  q         */
/*                                         and a remainder r         */
/*                                                                   */
       mpz_tdiv_qr( q, r, n, d);
/*                                                                   */
/*  Call mpz_get_str to convert q and r to a string of digits        */
/*  in base 10 and printing of result ( q and r ) in the form of     */
/* q = 18446744073709551616                                          */
/* r = 1                                                             */
/*                                                                   */
       mpz_get_str ( str, 10, q );
       printf( " q = %s\n", str);
       mpz_get_str ( str, 10, r );
       printf( " r = %s\n", str);
/*                                                                   */
/*             Free the space occupied by q, r, n, d                 */
/*                                                                   */
       mpz_clear ( q );
       mpz_clear ( r );
       mpz_clear ( n );
       mpz_clear ( d );
       return 0;
}

