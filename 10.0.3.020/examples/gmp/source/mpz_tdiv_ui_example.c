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
       unsigned long int d, ui_r;
       char str[] = "36893488147419103233";
/*                                                                   */
/*  Initialization of all multiple precision variables (  n )        */
/*    and assignment of data to entrance variables ( n and d )       */
/*                                                                   */
       mpz_init ( n );
/*                                                                   */
/*             Call mpz_set_str to set n = 36893488147419103233      */
/*                                                                   */
	   mpz_set_str ( n, str, 10);
       d = 2;
/*                                                                   */
/*   Call mpz_tdiv_ui to divide n by d,                              */
/*  returning the absolute value of the remainder(r): ui_r = abs(r)  */
/*                                                                   */
       ui_r = mpz_tdiv_ui( n, d);
/*                                                                   */
/*        Printing of result ( r ) in the form of                    */
/* ui_r = 1                                                          */
/*                                                                   */
	   printf( " ui_r = %d\n", ui_r);
/*                                                                   */
/*             Free the space occupied by r, n                       */
/*                                                                   */
       mpz_clear ( n );
       return 0;
}

