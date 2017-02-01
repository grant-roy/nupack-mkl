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
       unsigned long int rop;
	   char str1[] = "11110000001011110000111110001111111111111111";
	   char str2[] = "                                            ";
/*                                                                   */
/*     Initialization of multiple precision variable ( op ),         */
/*     and assignment of data to entrance variable ( op )            */
/*                    op = 1111110000011111000011111000111           */
/*                                                                   */
       mpz_init_set_str ( op, str1, 2 );   /*    2 - base            */
/*                                                                   */
/*             Call mpz_get_ui to set op to rop                      */
/*     If op is too big to fit an unsigned long then just the least  */
/*     significant bits that do fit are returned. The sign of op is  */
/*     ignored, only the absolute value is used.                     */
/*                                                                   */
       rop = mpz_get_ui ( op );
/*                                                                   */
/*  Call mpz_get_str to convert op to a string of digits in base 8   */
/*        and printing of op and rop in the form of                  */
/*  op  = 360136076177777
/*  rop =     36076177777     the least 32 bits op fit in rop        */
/*                                                                   */
       mpz_get_str ( str2, 8, op );
       printf( " op  = %s\n", str2 );
       printf( " rop =     %o\n", rop );
/*                                                                   */
/*             Free the space occupied by op                         */
/*                                                                   */
       mpz_clear ( op );
       return 0;
}

