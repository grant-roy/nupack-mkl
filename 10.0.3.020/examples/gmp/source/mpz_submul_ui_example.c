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
	    mpz_t rop, op1;
        unsigned long int op2;
        char str[] = "36893488147419103232";
/*                                                                   */
/*  Initialization of all multiple precision variables ( rop, op1 )  */
/*      and assignment of data to entrance variables (rop,op1,op2 )  */
/*                                                                   */
        mpz_init ( rop );
	    mpz_init ( op1 );
/*                                                                   */
/*           Call mpz_set_str to set rop = 36893488147419103232      */
/*                  op1 = 18446744073709551616                       */
/*	                                                                 */
		mpz_set_str ( rop, str, 10);
	    mpz_set_str ( op1, "18446744073709551616", 10);
	    op2 = 2;
/*                                                                   */
/*             Call mpz_submul_ui to set rop to rop - op1 * op2      */
/*                                                                   */
        mpz_submul_ui( rop, op1, op2);
/*                                                                   */
/*  Call mpz_get_str to convert rop to a string of digits in base 10 */
/*        and printing of result ( rop ) in the form of              */
/* rop = 0                                                           */
/*                                                                   */
        mpz_get_str ( str, 10, rop );
        printf( " rop = %s\n", str);
/*                                                                   */
/*             Free the space occupied by rop, op1                   */
/*                                                                   */
        mpz_clear ( rop );
        mpz_clear ( op1 );
        return 0;
}

