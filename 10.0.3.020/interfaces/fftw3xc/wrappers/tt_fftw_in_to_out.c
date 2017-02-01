/*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 2007-2008 Intel Corporation. All Rights Reserved.
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
!
!*******************************************************************************
!   function    : tt_fftw_in_to_out
!   Content     : copy data from in array to out array for further usage with
!                 TT library
!   input data  : double array in
!                 int n - dimension
!  output data  : double array out
!*******************************************************************************
*/

#include <mkl_types.h>

void tt_fftw_in_to_out(double *in, double *out,MKL_INT n)
{
	MKL_INT i;
	if ((double*)in != out)
	{
		for (i=0;i<n;i++) out[i]=in[i];
	}
	return;
}
