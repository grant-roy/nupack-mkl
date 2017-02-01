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
        unsigned long int b;
        char str[] = "18446744073709551617";
/*                                                                   */
/* Initialization of all multiple precision variables variables      */
/* (r,n) and assignment of data to entrance variables(n,b)           */
/*                                                                   */
        mpz_init ( r );
	    mpz_init ( n );
/*                                                                   */
/*     Call mpz_set to set n = 18446744073709551617                  */
/*                                                                   */
	    mpz_set_str ( n, str, 10);
        b = 1;
/*                                                             b     */
/*             Call mpz_fdiv_r_2exp to set r to remainder n / 2      */
/*                                                                   */
        mpz_fdiv_r_2exp( r, n, b);
/*                                                                   */
/*  Call mpz_get_str to convert r to a string of digits in base 10   */
/*        and printing of result ( r ) in the form of                */
/*  r = 1                                                            */
/*                                                                   */
        mpz_get_str ( str, 10, r );
        printf( " r = %s\n", str);
/*                                                                   */
/*             Free the space occupied by rop, op1                   */
/*                                                                   */
        mpz_clear ( r );
        mpz_clear ( n );
        return 0;
}
