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
       gmp_randstate_t state;
       unsigned long int n, seed;
	   char str1[] = "                      672749994932560009201";
	   int i;
/*                                                                   */
/*    Initialization of multiple precision variables ( rop ),        */
/*    state by calling one of the gmp_randinit functions             */
/*    and n.                                                         */
/*                                                                   */
	   mpz_init ( rop );
	   gmp_randinit_default ( state );
       n = 6;
	   seed = 1234;
/*                                                                   */
/*     Call gmp_randseed_ui to set initial seed value into state     */
/*                                                                   */

	   gmp_randseed_ui ( state, seed );
/*                                                                   */
/*     Call mpz_rrandomb to generate a uniform random integer        */
/*                                   n                               */
/*                in the range 0 to 2 - 1, inclusive                 */
/*                                                                   */
       for ( i = 1; i < 20; i++ )
	   {
	        mpz_rrandomb ( rop, state, n );
/*                                                                   */
/*  Call mpz_get_str to convert rop to a string of digits in base 10 */
/*        and printing of result ( rop ) in the form of              */
/* rop = 427338586147715374480462208348                              */
/* rop = 905602502006303407107391825891                              */
/* rop = 259569847638912057221752531320                              */
/* rop = 1036628258810366787201898542263                             */
/* rop = 805083914824151032404804753744                              */
/*                                                                   */
            mpz_get_str ( str1, 2, rop );
            printf( " rop = %s\n", str1);
/*            printf( " seed = %d\n", seed);                         */
/*            gmp_printf (" rop = %Z\n", rop);                       */
	   }
/*                                                                   */
/*             Free the space occupied by rop, state                 */
/*                                                                   */
       mpz_clear ( rop );
       gmp_randclear ( state );
       return 0;
}

