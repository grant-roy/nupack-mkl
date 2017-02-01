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
	   mpz_t rop, op1, op2;
       char str1[] = "17654372174325565422222224";/*number in base 8 */
       char str2[] = "07654372174325565422222223";/*number in base 8 */
/*                                                                   */
/* Initialization of all  multiple precision variables ( rop, op1,   */
/*  op2 )  and assignment of data to entrance variables ( op1, op2 ) */
/*                                                                   */
       mpz_init ( rop );
	   mpz_init ( op1 );
	   mpz_init ( op2 );
/*                                                                   */
/*       Call mpz_set_str to set op1 = 17654372174325565422222224    */
/*                           and op2 = 07654372174325565422222223    */
/*                                                                   */
	   mpz_set_str ( op1, str1, 8 );
	   mpz_set_str ( op2, str2, 8 );
/*                                                                   */
/*             Call mpz_and to set rop to op1 bitwise-and op2        */
/*                                                                   */
       mpz_and( rop, op1, op2);
/*                                                                   */
/*  Call mpz_get_str to convert rop to a string of digits in base 10 */
/*        and printing of result ( rop ) in the form of              */
/* rop = 7654372174325565422222220                                   */
/*                                                                   */
       mpz_get_str ( str1, 8, rop );
       printf( " rop = %s\n", str1);
/*                                                                   */
/*             Free the space occupied by rop, op1, op2              */
/*                                                                   */
       mpz_clear ( rop );
       mpz_clear ( op1 );
       mpz_clear ( op2 );
       return 0;
}

