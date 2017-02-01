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
	   mpz_t op;
       int n;
	   char str1[] = "65535";
/*                                                                   */
/*     Initialization of all  multiple precision variables ( op )    */
/*            and assignment of data to entrance variables ( op )    */
/*                                                                   */
	   mpz_init ( op );
/*                                                                   */
/*       Call mpz_set_str to set op = 65535                          */
/*                                                                   */
	   mpz_set_str ( op, str1, 10 );
/*                                                                   */
/*     mpz_fits_ushort_p return 1 if the value of op fits in         */
/*                  an unsigned short int. Otherwise, return zero.   */
/*                                                                   */

	   n = mpz_fits_ushort_p ( op );
/*                                                                   */
/*        Printing of result ( n ) in the form of                    */
/* n = 1                                                             */
/*                                                                   */
       printf( " n = %d\n", n );
/*                                                                   */
/*             Free the space occupied by op                         */
/*                                                                   */
       mpz_clear ( op );
       return 0;
}

