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
	   mpz_t op1, op2;
       unsigned long int n;
	   char str1[] = "11111111111111111111111001";
       char str2[] = "11111111111111111111111111";
/*                                                                   */
/* Initialization of all  multiple precision variables ( op1, op2 )  */
/*    and assignment of data to entrance variables ( op1, op2 )      */
/*                                                                   */
	   mpz_init ( op1 );
	   mpz_init ( op2 );
/*                                                                   */
/*       Call mpz_set_str to set op1 = 17654372174325565422222224    */
/*                           and op2 = 17654372174325565422222223    */
/*                                                                   */
	   mpz_set_str ( op1, str1, 8 );
	   mpz_set_str ( op2, str2, 8 );
/*                                                                   */
/*     Call mpz_hamdist to set                                       */
/* n = the hamming distance between the two operands,                */
/*     which is the number of bit positions                          */
/*     where op1 and op2 have different bit values                   */
/*     if op1 and op2 are both >= 0.                                 */
/* n = the largest possible unsigned long if op < 0                  */
/*                                                                   */
       n = mpz_hamdist ( op1, op2);
/*                                                                   */
/*        Printing of result ( n ) in the form of                    */
/* n = 10000000000000000000000007                                  */
/*                                                                   */
       printf( " n = %d\n", n );
/*                                                                   */
/*             Free the space occupied by op1, op2                   */
/*                                                                   */
       mpz_clear ( op1 );
       mpz_clear ( op2 );
       return 0;
}

