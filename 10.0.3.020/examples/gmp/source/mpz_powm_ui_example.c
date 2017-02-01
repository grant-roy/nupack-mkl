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
       mpz_t rop, base, mod2;
	   unsigned long int exp;
       char str[] = "100000000000000000000";
/*                                                                   */
/*          Initialization of all multiple precision variables       */
/*                ( rop, base, mod2 )                                */
/* and assignment of data to entrance variables ( base, exp, mod2  ) */
/*                                                                   */
       mpz_init ( rop );
       mpz_init ( base );
       mpz_init ( mod2 );
/*                                                                   */
/*Call mpz_set_str to set base = 11                                  */
/*                    and exp = 100                                  */
/*                    and mod2 = 100000000000000000000(20 zeros)     */
/*                                                                   */
	   mpz_set_str ( base, "11", 10 );
       exp = 100;
       mpz_set_str ( mod2, str, 10 );
/*                                                                   */
/*             Call mpz_powm_ui                                      */
/*    to set rop to base to the exp power on the modulo mod2         */
/*                                                                   */
       mpz_powm_ui ( rop, base, exp, mod2 );
/*                                                                   */
/*      Call mpz_get_str to convert rop to a string of digits        */
/*     in base 10 and printing of result ( rop ) in the form of      */
/* rop = 1159497458526446001                                         */
/*                                                                   */
       mpz_get_str ( str, 10, rop );
       printf ( " rop = %s\n", str );
/*                                                                   */
/*             Free the space occupied by rop, base, mod2            */
/*                                                                   */
       mpz_clear ( rop );
       mpz_clear ( base );
       mpz_clear ( mod2 );
       return 0;
}

