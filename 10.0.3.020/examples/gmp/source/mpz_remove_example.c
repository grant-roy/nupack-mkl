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
       mpz_t rop, op, f;
       unsigned long int n;
	   char str1[] = "11";
	   char str2[] = "6727499949325600092010";
/*                                                                   */
/*  Initialization of all multiple precision variables (rop, op, f)  */
/*     and assignment of data to entrance variables ( op, f )        */
/*                                                                   */
       mpz_init ( rop );
       mpz_init ( op );
       mpz_init ( f );
/*                                                                   */
/*     Call mpz_set_str to set op = 672749994932560009201            */
/*                      to set f = 11                                */
/*                                                                   */
	   mpz_set_str ( op, str2, 10 );
	   mpz_set_str ( f, str1, 10 );
/*                                                                   */
/*                  Call mpz_remove                                  */
/*      to remove all occurrences of the factor f from op            */
/*      and store the result in rop                                  */
/*  The return value is how many such occurrences were removed ( n ) */
/*                                                                   */
       n = mpz_remove ( rop, op, f );
/*                                                                   */
/*      Call mpz_get_str to convert rop to a string of digits        */
/*     in base 10 and printing of result ( rop ) in the form of      */
/* rop = 10                                                          */
/* n = 20                                                            */
/*                                                                   */
       mpz_get_str ( str2, 10, rop );
       printf ( " rop = %s\n", str2 );
       printf ( " n = %d\n", n );
/*                                                                   */
/*             Free the space occupied by rop, op, f                 */
/*                                                                   */
       mpz_clear ( rop );
       mpz_clear ( op );
       mpz_clear ( f );
       return 0;
}
