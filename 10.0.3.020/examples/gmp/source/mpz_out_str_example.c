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
       int base;
       size_t number_bytes_written;
       FILE* stream;
	   char str1[] = "12682271391376756984";
/*                                                                   */
/*  Initialization of all multiple precision variables ( op )        */
/*  and assignment of data to entrance variables ( op, stream, base )*/
/*                                                                   */
       mpz_init ( op );
/*                                                                   */
/*     Call mpz_set_str to set op = 12682271391376756984             */
/*     Passing a NULL pointer for a stream argument
/*                                   will make write op to stdout    */
/*     and base of output(the base may be from 2 to 36) base = 10    */
/*                                                                   */
	   mpz_set_str ( op, str1, 10 );
       stream = NULL;
	   base = 10;
/*                                                                   */
/*                  Call mpz_out_str                                 */
/*     to output op on stdio stream stream,                          */
/*                           as a string of digits in base base      */
/*     Return the number of bytes written,                           */
/*     or if an error occured, return 0 ( in  number_bytes_written ).*/
/*                                                                   */
	   number_bytes_written = mpz_out_str ( stream, base, op );
/*                                                                   */
/*     Printing of result (op, number_bytes_written) in the form of  */
/*12682271391376756984                                               */
/* number_bytes_written = 20                                         */
/*                                                                   */
   printf (  "\n number_bytes_written = %d\n", number_bytes_written );
/*                                                                   */
/*             Free the space occupied by op                         */
/*                                                                   */
       mpz_clear ( op );
       return 0;
}
