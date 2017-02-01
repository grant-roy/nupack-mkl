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
!   function       : tt_fftw_norm_s
!   Content        : prepare data for usage within TT library by adding additional 
!                    zero entry as the first entry to in array (through temporary storage)
!   input data     : single array in
!   output data    : single array out
!				    int ir
!   in/output data : single array dpar
!                    pointer to DFTI handle,
!                    int array ipar
!*******************************************************************************
*/

#include "fftw3.h"
#include "fftw3_mkl.h"
#include "mkl_trig_transforms.h"

void tt_fftw_norm_s(float in[], float out[], DFTI_DESCRIPTOR_HANDLE handle, MKL_INT ipar[], float spar[], MKL_INT *ir)
{
	MKL_INT i,n;
	n = ipar[0];
	if (n>0)
	{
	  spar[n/2+2]=0.0E0;
	  for (i=1; i<n; i++) spar[n/2+2+i]=in[i-1];  
	  s_backward_trig_transform(&spar[n/2+2],&handle,ipar,spar,ir);
	  for (i=1; i<ipar[0]; i++) out[i-1]=spar[n/2+2+i];           
	}
	return;
}
