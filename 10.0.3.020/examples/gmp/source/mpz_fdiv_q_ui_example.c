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
	   mpz_t q, n;
       unsigned long int d, ui_r2;
       char str[] = "36893488147419103233";
/*                                                                   */
/*  Initialization of all multiple precision variables  ( q, n )     */
/*    and assignment of data to entrance variables ( n, d )          */
/*                                                                   */
       mpz_init ( q );
       mpz_init ( n );
/*                                                                   */
/*             Call mpz_set_str to set n = 36893488147419103233      */
/*                                                                   */
	   mpz_set_str ( n, str, 10);
       d = 2;
/*                                                                   */
/*   Call mpz_fdiv_q_ui  to divide n by d, forming a quotient q      */
/*             and ui_r2 = the absolute value of the remainder r     */
/*                                                                   */
       ui_r2 = mpz_fdiv_q_ui( q, n, d);
/*                                                                   */
/*  Call mpz_get_str to convert q to a string of digits in base 10   */
/*        and printing of result ( q, ui_r2 ) in the form of         */
/* q = 18446744073709551616                                          */
/* ui_r2 = 1                                                         */
/*                                                                   */
       mpz_get_str ( str, 10, q );
       printf( " q = %s\n", str);
       printf( " ui_r2 = %d\n", ui_r2);
/*                                                                   */
/*             Free the space occupied by q, n                       */
/*                                                                   */
       mpz_clear ( q );
       mpz_clear ( n );
       return 0;
}

