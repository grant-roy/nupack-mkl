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
       unsigned long int op;
       char str1[] = "                         ";
/*                                                                   */
/*     Initialization of  multiple precision variable ( rop )        */
/*	   and assignment of data to entrance variable ( op )            */
/*                                                                   */
	   mpz_init ( rop );
       op = 123456789;
/*                                                                   */
/*             Call mpz_set_ui to set rop to op                      */
/*                                                                   */
       mpz_set_ui ( rop, op );
/*                                                                   */
/*  Call mpz_get_str to convert rop to a string of digits in base 10 */
/*        and printing of result ( rop ) in the form of              */
/* rop = 123456789                                                   */
/*                                                                   */
       mpz_get_str ( str1, 10, rop );
       printf( " rop = %s\n", str1 );
/*                                                                   */
/*             Free the space occupied by rop                        */
/*                                                                   */
       mpz_clear ( rop );
       return 0;
}

