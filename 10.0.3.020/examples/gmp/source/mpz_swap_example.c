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
	   mpz_t rop1, rop2;
       char str1[] = "18446744073709551616";
       char str2[] = "23451844674407370955";
/*                                                                   */
/* Initialization of all  multiple precision variables ( rop, op )   */
/*	   and assignment of data to entrance variable ( rop, op )       */
/*                                                                   */
	   mpz_init ( rop1 );
	   mpz_init ( rop2 );
/*                                                                   */
/*             Call mpz_set_str to set op = 18446744073709551616     */
/*                                    rop = 23451844674407370955     */
	   mpz_set_str ( rop1, str1, 10);
	   mpz_set_str ( rop2, str2, 10);
/*                                                                   */
/*             Call mpz_swap to swap values rop1 and rop2            */
/*                                                                   */
       mpz_swap ( rop1, rop2 );
/*                                                                   */
/*  Call mpz_get_str to convert rop1 and rop2 to a string of digits  */
/*        in base 10 and printing of result ( rop ) in the form of   */
/* rop1 = 23451844674407370955                                       */
/* rop2 = 18446744073709551616                                       */
/*                                                                   */
       mpz_get_str ( str1, 10, rop1 );
       printf( " rop1 = %s\n", str1 );
       mpz_get_str ( str2, 10, rop2 );
       printf( " rop2 = %s\n", str2 );
/*                                                                   */
/*             Free the space occupied by rop1, rop2                 */
/*                                                                   */
       mpz_clear ( rop1 );
       mpz_clear ( rop2 );
       return 0;
}

