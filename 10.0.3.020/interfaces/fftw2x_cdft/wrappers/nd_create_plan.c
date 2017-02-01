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
!       Wrapper library building. Functions fftw2d_mpi_create_plan,
!	fftw3d_mpi_create_plan, fftwnd_mpi_create_plan.
!       This library allows to use MKL CDFT routines through MPI FFTW interface.
!******************************************************************************/
#include "common.h"

#ifdef MKL_SINGLE
	#define PREC DFTI_SINGLE
#else
	#define PREC DFTI_DOUBLE
#endif


fftwnd_mpi_plan fftw2d_mpi_create_plan(MPI_Comm comm,int nx, int ny,fftw_direction dir, int flags){
	MKL_LONG err;
	fftwnd_mpi_plan p;
	MKL_LONG n[2];
	if ((flags!=FFTW_ESTIMATE)&&(flags!=FFTW_MEASURE))
		if ((!(flags & FFTW_SCRAMBLED_OUTPUT))&&(!(flags & FFTW_SCRAMBLED_INPUT)))  goto error2;

	p=(fftwnd_mpi_plan)malloc(sizeof(fftwnd_mpi_plan_struct));
	p->dir=dir;

	n[0]=nx; n[1]=ny;
	err=DftiCreateDescriptorDM(comm,&p->h,PREC,DFTI_COMPLEX,2,n);
	if (err!=DFTI_NO_ERROR) goto error;

	if ((flags & FFTW_SCRAMBLED_OUTPUT)||(flags & FFTW_SCRAMBLED_INPUT) )
	{
	    err = DftiSetValueDM(p->h,DFTI_ORDERING, DFTI_ORDERED);
	    if (err!=DFTI_NO_ERROR) goto error;
	}
	err=DftiCommitDescriptorDM(p->h);
	if (err!=DFTI_NO_ERROR) goto error;
	return p;

error:
	fprintf(stderr,"CDFT error %ld in fftw2d_mpi_create_plan wrapper\n",err);
	free(p);
	return NULL;
error2:
	fprintf(stderr,"CDFT error  in fftw2d_mpi_create_plan wrapper: unknown flags\n");
	return NULL;
}

fftwnd_mpi_plan fftw3d_mpi_create_plan(MPI_Comm comm,int nx, int ny,int nz,fftw_direction dir, int flags){
	long err;
	fftwnd_mpi_plan p;
	long n[3];
	if ((flags!=FFTW_ESTIMATE)&&(flags!=FFTW_MEASURE))
		if ((!(flags & FFTW_SCRAMBLED_OUTPUT))&&(!(flags & FFTW_SCRAMBLED_INPUT)))  goto error2;

	p=(fftwnd_mpi_plan)malloc(sizeof(fftwnd_mpi_plan_struct));
	p->dir=dir;

	n[0]=nx; n[1]=ny; n[2]=nz;
	err=DftiCreateDescriptorDM(comm,&p->h,PREC,DFTI_COMPLEX,3,n);
	if (err!=DFTI_NO_ERROR) goto error;

	if ((flags & FFTW_SCRAMBLED_OUTPUT)||(flags & FFTW_SCRAMBLED_INPUT) )
	{
	    err = DftiSetValueDM(p->h,DFTI_ORDERING, DFTI_ORDERED);
	    if (err!=DFTI_NO_ERROR) goto error;
	}

	err=DftiCommitDescriptorDM(p->h);
	if (err!=DFTI_NO_ERROR) goto error;

	return p;

error:
	fprintf(stderr,"CDFT error %ld in fftw3d_mpi_create_plan wrapper\n",err);
	free(p);
	return NULL;
error2:
	fprintf(stderr,"CDFT error  in fftw3d_mpi_create_plan wrapper: unknown flags\n");
	return NULL;
}

fftwnd_mpi_plan fftwnd_mpi_create_plan(MPI_Comm comm,int dim, int* nn,fftw_direction dir,int flags){
	long err;
	fftwnd_mpi_plan p;
	long* n;
	int i;
	if ((flags!=FFTW_ESTIMATE)&&(flags!=FFTW_MEASURE))
		if ((!(flags & FFTW_SCRAMBLED_OUTPUT))&&(!(flags & FFTW_SCRAMBLED_INPUT)))  goto error2;

	p=(fftwnd_mpi_plan)malloc(sizeof(fftwnd_mpi_plan_struct));
	p->dir=dir;

	n=(long*)malloc(dim*sizeof(long));
	for( i=0; i<dim; i++) n[i]=nn[i];

	err=DftiCreateDescriptorDM(comm,&p->h,PREC,DFTI_COMPLEX,dim,n);
    free(n);
	if (err!=DFTI_NO_ERROR) goto error;

	if ((flags & FFTW_SCRAMBLED_OUTPUT)||(flags & FFTW_SCRAMBLED_INPUT) )
	{
	    err = DftiSetValueDM(p->h,DFTI_ORDERING, DFTI_ORDERED);
	    if (err!=DFTI_NO_ERROR) goto error;
	}

	err=DftiCommitDescriptorDM(p->h);
	if (err!=DFTI_NO_ERROR) goto error;

	return p;

error:
	fprintf(stderr,"CDFT error %ld in fftwnd_mpi_create_plan wrapper\n",err);
	free(p);
	return NULL;

error2:
	fprintf(stderr,"CDFT error in fftwnd_mpi_create_plan wrapper: unknown flags\n");
	return NULL;
}
