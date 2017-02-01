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
       mpz_t a;
       signed long int b;
       int Kronecker_symbol;
	   char str1[] = "672749994932560009201";
/*                                                                   */
/*     Initialization of all multiple precision variables ( a )      */
/*     and assignment of data to entrance variables ( a, b )         */
/*                                                                   */
       mpz_init ( a );
/*                                                                   */
/*     Call mpz_set_str to set a = 672749994932560009201             */
/*                                                                   */
	   mpz_set_str ( a, str1, 10 );
	   b = -3521;
/*                                                                   */
/*  Call mpz_kronecker_si to set Kronecker_symbol                    */
/*                        to the Kronecker symbol (a/b)              */
/*                                                                   */
       Kronecker_symbol = mpz_kronecker_si ( a, b );
/*                                                                   */
/*     Printing of result ( Kronecker symbol ) in the form of        */
/* Kronecker_symbol = 1                                              */
/*                                                                   */
       printf ( " Kronecker_symbol = %d\n", Kronecker_symbol );
/*                                                                   */
/*             Free the space occupied by a                          */
/*                                                                   */
       mpz_clear ( a );
       return 0;
}
