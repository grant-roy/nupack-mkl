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
!       Wrapper library building. Functions rfftw2d_mpi_create_plan,
!	rfftw3d_mpi_create_plan, rfftwnd_mpi_create_plan.
!	Its empty wrappers yet.
!       This library allows to use MKL CDFT routines through MPI FFTW interface.
!******************************************************************************/

#include "common.h"

#ifdef MKL_SINGLE
	#define PREC DFTI_SINGLE
#else
	#define PREC DFTI_DOUBLE
#endif


rfftwnd_mpi_plan rfftw2d_mpi_create_plan(MPI_Comm comm,int nx, int ny,fftw_direction dir, int flags)
{
	fprintf(stderr,"CDFT error in rfftw2d_mpi_create_plan wrapper\n");
	return NULL;
}

rfftwnd_mpi_plan rfftw3d_mpi_create_plan(MPI_Comm comm,int nx, int ny,int nz,fftw_direction dir, int flags)\
{
	fprintf(stderr,"CDFT error in fftw3d_mpi_create_plan wrapper\n");
	return NULL;
}

rfftwnd_mpi_plan rfftwnd_mpi_create_plan(MPI_Comm comm,int dim, int* nn,fftw_direction dir,int flags)
{
	fprintf(stderr,"CDFT error in fftwnd_mpi_create_plan wrapper\n");
	return NULL;
}
