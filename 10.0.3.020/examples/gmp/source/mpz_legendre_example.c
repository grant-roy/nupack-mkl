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
       mpz_t a, p;
       int Legendre_symbol;
	   char str1[] = "672749994932560009201";
	   char str2[] = "3527";
/*                                                                   */
/*     Initialization of all multiple precision variables ( a, p )   */
/*     and assignment of data to entrance variables ( a, p )         */
/*                                                                   */
       mpz_init ( a );
       mpz_init ( p );
/*                                                                   */
/*     Call mpz_set_str to set a = 672749994932560009201             */
/*                             p = 3527                              */
/*                                                                   */
	   mpz_set_str ( a, str1, 10 );
	   mpz_set_str ( p, str2, 10 );
/*                                                                   */
/*     Call mpz_legendre to set Legendre_symbol                      */
/*                                to the Legendre symbol (a/p)       */
/*                                                                   */
       Legendre_symbol = mpz_legendre ( a, p );
/*                                                                   */
/*     Printing of result ( Legendre symbol ) in the form of         */
/* Legendre_symbol = 1                                               */
/*                                                                   */
       printf ( " Legendre_symbol = %d\n", Legendre_symbol );
/*                                                                   */
/*             Free the space occupied by a, p                       */
/*                                                                   */
       mpz_clear ( a );
       mpz_clear ( p );
       return 0;
}
