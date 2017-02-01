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
       mpz_t fn;
       unsigned long int n;
	   char str1[] = "280571172992510140037611932413038677189525";
/*                                                                   */
/*     Initialization of all multiple precision variables ( fn )     */
/*     and assignment of data to entrance variables ( n )            */
/*                                                                   */
       mpz_init ( fn );
       n = 200;
/*                                                                   */
/*     Call mpz_fib_ui to set fn to F(n), n'th Fibonacci number      */
/*                                                                   */
       mpz_fib_ui ( fn, n );
/*                                                                   */
/*      Call mpz_get_str to convert fn to a string of digits         */
/*     in base 10 and printing of result ( fn ) in the form of       */
/*  fn = 280571172992510140037611932413038677189525                  */
/*                                                                   */
       mpz_get_str ( str1, 10, fn );
       printf ( " fn = %s\n", str1 );
/*                                                                   */
/*             Free the space occupied by fn                         */
/*                                                                   */
       mpz_clear ( fn );
       return 0;
}
