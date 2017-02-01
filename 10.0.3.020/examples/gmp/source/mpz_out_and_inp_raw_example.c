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
       mpz_t op,rop;
       size_t number_bytes_written, number_bytes_read;
       FILE* stream;
	   char str1[] = "12682271391376756984";
       char str2[] = "                    ";
/*                                                                   */
/*  Initialization of all multiple precision variables ( op,rop )    */
/*  and assignment of data to entrance variables ( op, stream )      */
/*                                                                   */
       mpz_init ( op );
       mpz_init ( rop );
/*                                                                   */
/*     Call mpz_set_str to set op = 12682271391376756984             */
/*                                                                   */
	   mpz_set_str ( op, str1, 10 );
       if ((stream = fopen("test.dat","w")) == NULL )
	   {
		   printf("Mistake at opening a file for write");
		   exit(1);
	   }
/*                                                                   */
/*                  Call mpz_out_raw                                 */
/*     to output op on stdio stream stream, in raw binary format.    */
/*     Return the number of bytes written,                           */
/*     or if an error occured, return 0 ( in  number_bytes_written ).*/
/*                                                                   */
	   number_bytes_written = mpz_out_raw ( stream, op );
       fclose(stream);
/*                                                                   */
/*                  Call mpz_inp_raw                                 */
/*   to input in rop from stdio stream stream, in raw binary format. */
/*     Return the number of bytes read,                              */
/*     or if an error occured, return 0 ( in  number_bytes_written ).*/
/*                                                                   */
	   if ((stream = fopen("test.dat","r")) == NULL )
	   {
		   printf("Mistake at opening a file for read");
		   exit(2);
	   }
	   number_bytes_read = mpz_inp_raw ( rop, stream );
       fclose(stream);
/*                                                                   */
/* Printing of result (rop, number_bytes_written, number_bytes_read) */
/*                                                 in the form of    */
/* rop = 12682271391376756984                                        */
/* number_bytes_written = 12                                         */
/* number_bytes_read = 12                                            */
/*                                                                   */
       mpz_get_str ( str2, 10, rop );
       printf ( " rop = %s\n", str2 );
	   printf (  " number_bytes_written = %d\n", number_bytes_written );
	   printf (  " number_bytes_read = %d\n", number_bytes_read );
/*                                                                   */
/*             Free the space occupied by op,rop                     */
/*                                                                   */
       mpz_clear ( op );
       mpz_clear ( rop );
       return 0;
}
