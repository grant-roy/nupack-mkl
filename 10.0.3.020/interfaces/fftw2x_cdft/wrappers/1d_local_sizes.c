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
!       Wrapper library building. Function fftw_mpi_local_sizes.
!       This library allows to use MKL CDFT routines through MPI FFTW interface.
!******************************************************************************/
#include "common.h"

void fftw_mpi_local_sizes(fftw_mpi_plan p,int *n,int *start,int *n_out,int *start_out,int *size) {

	MKL_LONG err,v;

	err=DftiGetValueDM(p->h,CDFT_LOCAL_NX,&v);
	if (err!=DFTI_NO_ERROR) goto error;
	*n=v;
	err=DftiGetValueDM(p->h,CDFT_LOCAL_X_START,&v);
	if (err!=DFTI_NO_ERROR) goto error;
	*start=v;
	err=DftiGetValueDM(p->h,CDFT_LOCAL_OUT_NX,&v);
	if (err!=DFTI_NO_ERROR) goto error;
	*n_out=v;
	err=DftiGetValueDM(p->h,CDFT_LOCAL_OUT_X_START,&v);
	if (err!=DFTI_NO_ERROR) goto error;
	*start_out=v;
	err=DftiGetValueDM(p->h,CDFT_LOCAL_SIZE,&v);
	if (err!=DFTI_NO_ERROR) goto error;
	*size=v;
	return;
error:
	fprintf(stderr,"CDFT error %ld in fftw_mpi_local_sizes wrapper\n",err);
	return;
}
