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
	   mpz_t op1;
       double op2;
	   int n;
	   char str1[] = "-340282366920938463463374607431768211457";
/*                                                                   */
/*   Initialization of all multiple precision variables ( op1 )      */
/*         and assignment of data to entrance variables ( op1, op2 ) */
/*                                                                   */
       mpz_init ( op1 );
/*                                                                   */
/*              Call mpz_set_str                                     */
/*       to set op1 = -340282366920938463463374607431768211457       */
/*                                                                   */
	   mpz_set_str ( op1, str1, 10 );
       op2 = 2.0;
/*                                                                   */
/*    Call mpz_cmpabs_d to compare |op1| and |op2|.                  */
/*  Return a positive value if |op1| > |op2|, zero if |op1| = |op2|, */
/*            or a negative value if |op1| < |op2|                   */
/*                                                                   */
       n = mpz_cmpabs_d( op1, op2 );
/*                                                                   */
/*        Printing of result ( n ) in the form of                    */
/* n = 1                                                             */
/*                                                                   */
       printf( " n = %d\n", n );
/*                                                                   */
/*             Free the space occupied by op1, op2                   */
/*                                                                   */
       mpz_clear ( op1 );
       return 0;
}

