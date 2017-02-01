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
!******************************************************************************
!   Content:
!       Wrapper library building. Function fftwnd_mpi.
!       This library allows to use MKL CDFT routines through MPI FFTW interface.
!******************************************************************************/

#include "common.h"

void fftwnd_mpi(fftwnd_mpi_plan p,int n,void *local, void *work, fftwnd_mpi_output_order output_order){

	MKL_LONG err;
	if (output_order!=FFTW_NORMAL_ORDER) goto error2;
	if (work==NULL)
		if (p->dir==FFTW_FORWARD) err=DftiComputeForwardDM(p->h,local); else err=DftiComputeBackwardDM(p->h,local);
	else {
		err=DftiSetValueDM(p->h,CDFT_WORKSPACE,work);
		if (err!=DFTI_NO_ERROR) goto error;
		err=DftiCommitDescriptorDM(p->h);
		if (err!=DFTI_NO_ERROR) goto error;
		if (p->dir==FFTW_FORWARD) err=DftiComputeForwardDM(p->h,local); else err=DftiComputeBackwardDM(p->h,local);
		if (err!=DFTI_NO_ERROR) goto error;
		err=DftiSetValueDM(p->h,CDFT_WORKSPACE,NULL);
		if (err!=DFTI_NO_ERROR) goto error;
		err=DftiCommitDescriptorDM(p->h);
	}
	if (err!=DFTI_NO_ERROR) goto error;
	return;
error:
	fprintf(stderr,"CDFT error %ld in fftwnd_mpi wrapper\n",err);
	return;
error2:
	fprintf(stderr,"CDFT error %ld in fftwnd_mpi wrapper: unknown output_order\n",err);
	return;
}
