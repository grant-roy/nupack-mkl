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
!
!*******************************************************************************
!   function    : rfftw3d_f77_create_plan - wrapper for interface FFTW function
!   Content     : wrapper from this function to DFTI MKL initialization functions
!                 for double/float precision real data
!   input data  : parameters for DFT
!   output data : NULL
!*******************************************************************************
*/

#include "rfftw.h"
#include "fftw2_mkl.h"

void rfftw3d_f77_create_plan(rfftwnd_plan* plan, int* nx, int* ny, int* nz,
							 fftw_direction* dir, int* flags) {

    *plan = rfftw3d_create_plan(*nz, *ny, *nx, *dir, *flags);
	return;
}
