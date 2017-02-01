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
       int base;
       size_t number_bytes_read;
       FILE* stream;
	   char str1[] = "12682271391376756984";
/*                                                                   */
/*  Initialization of all multiple precision variables ( op )        */
/*  and assignment of data to entrance variables ( op, stream, base )*/
/*                                                                   */
       mpz_init ( rop );
/*                                                                   */
/*     Passing a NULL pointer for a stream argument                  */
/*	   will make read integer in rop from stdin                      */
/*     and base of input(the base may be from 2 to 36) base = 10     */
/*     If base is 0, the actual base is determined from the leading  */
/*     characters: if the first two characters are '0x' or 'oX',     */
/*  hexadecimal is assumed, otherwise if the first character is '0', */
/*  octal is assumed, otherwise decimal is assumed.                  */
/*                                                                   */
       stream = NULL;
	   base = 10;
/*                                                                   */
/*                  Call mpz_inp_str                                 */
/*     to input in rop from stdio stream stream,                     */
/*                           as a string of digits in base base      */
/*     Number 12682271391376756984 is entered as follows:            */
/*     after input of significant figures the blank is entered and   */
/*     <ENTER>.                                                      */
/*     Return the number of bytes read,                              */
/*     or if an error occured, return 0 ( in  number_bytes_read ).   */
/*                                                                   */
	   number_bytes_read = mpz_inp_str ( rop, stream, base );
/*                                                                   */
/*     Call mpz_get_str to convert rop to a string of digits         */
/*     in base 10 and printing of result (rop, number_bytes_read)    */
/*     in the form of                                                */
/* rop = 12682271391376756984                                        */
/* number_bytes_written = 20                                         */
/*                                                                   */
       mpz_get_str ( str1, 10, rop );
       printf ( " rop = %s\n", str1 );
       printf (  "\n number_bytes_read = %d\n", number_bytes_read );
/*                                                                   */
/*             Free the space occupied by op                         */
/*                                                                   */
       mpz_clear ( rop );
       return 0;
}
