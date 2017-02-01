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
	   mpz_t rop;
       unsigned long int bit_index;
	   char str1[] = "11111111111111111111111001";
/*                                                                   */
/*     Initialization of all  multiple precision variables ( rop )   */
/* and assignment of data to entrance variables ( rop, bit_index )   */
/*                                                                   */
	   mpz_init ( rop );
/*                                                                   */
/*       Call mpz_set_str to set rop = 11111111111111111111111001    */
/*                                                                   */
	   mpz_set_str ( rop, str1, 2 );
       bit_index = 2;
/*                                                                   */
/*     Call mpz_setbit to set bit bit_index in rop                   */
/*                                                                   */

	   mpz_setbit ( rop, bit_index );
/*                                                                   */
/*                                                                   */
/*  Call mpz_get_str to convert rop to a string of digits in base 2  */
/*        and printing of result ( rop ) in the form of              */
/* rop = 11111111111111111111111101                                                          */
/*                                                                   */
       mpz_get_str ( str1, 2, rop );
       printf( " rop = %s\n", str1 );
/*                                                                   */
/*             Free the space occupied by rop                        */
/*                                                                   */
       mpz_clear ( rop );
       return 0;
}

