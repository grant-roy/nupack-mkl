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
       mpz_t n;
	   int reps;
	   char str[] = "672749994932560009201";  /*   = 11^20           */
/*                                                                   */
/*     Initialization of all multiple precision variables ( n )      */
/*     and assignment of data to entrance variables ( n )            */
/*                                                                   */
       mpz_init ( n );
/*                                                                   */
/*     Call mpz_set_str to set n = 672749994932560009201             */
/*                                                                   */
	   mpz_set_str ( n, str, 10 );
/*                                                                   */
/*     Call mpz_probab_prime_p to check up, whether n is prime       */
/*                                                                   */
       reps = mpz_probab_prime_p ( n, reps );
/*                                                                   */
/*     printing of result ( reps ) in the form of                    */
/* reps = 2          if n is definitely prime                        */
/* reps = 1          if n is probably prime (without being certain)  */
/* reps = 0          if n is definitly composite. There should be it.*/
/*                                                                   */
       printf ( " reps = %d\n", reps );
/*                                                                   */
/*             Free the space occupied by op                         */
/*                                                                   */
       mpz_clear ( n );
       return 0;
}
