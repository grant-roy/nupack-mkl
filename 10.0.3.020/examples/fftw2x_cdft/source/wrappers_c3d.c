/*******************************************************************************
!                             INTEL CONFIDENTIAL
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
!   Content:
!       MKL Cluster DFT using FFTW interface (via wrappers)
!	example program (C-interface)
!
!       Forward-Backward 3D complex transform for double/single precision data.
!
!*******************************************************************************
!  Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX          (obligatory)
!           DFTI_PRECISION      = DFTI_DOUBLE           (default)
!           DFTI_DIMENSION      = 3                     (obligatory)
!           DFTI_LENGTHS        = {lenx,leny,lenz}      (obligatory)
!           DFTI_FORWARD_SCALE  = 1.0                   (default)
!           DFTI_BACKWARD_SCALE = 1.0/(lenx*leny*lenz)  (default=1.0)
!
!******************************************************************************/
#include <stdlib.h>
#include "fftw_mpi.h"

#ifdef FFTW_ENABLE_FLOAT
    #define EPS 1.0E-6
#else
    #define EPS 1.0E-12
#endif
int main(int argc,char *argv[])
{

    fftw_complex *local,*work;
    fftw_real *preal,t,err,scale;
    int i,PrS,PrR;
    fftwnd_mpi_plan plan;

    int len,nx,nx_out,start_x,start_x_out,size;
    int lenx, leny, lenz;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&PrS);
    MPI_Comm_rank(MPI_COMM_WORLD,&PrR);
    lenx=128; leny=64; lenz=32;

/*------------    Usage of MPI FFTW for Complex Multi-dimensional Transforms  (work==NULL) fftw3d_create_plan---*/
    if (PrR==0) printf("Usage of MPI FFTW for Complex Multi-dimensional Transforms (work==NULL).\nPlan = fftw3d_mpi_create_plan(...)\n");

    if(PrR==0) printf("PrS=%d,lengths=(%d,%d,%d)\n",PrS,lenx, leny,lenz);
    plan=fftw3d_mpi_create_plan(MPI_COMM_WORLD,lenx,leny,lenz,FFTW_FORWARD,FFTW_ESTIMATE);
    if (PrR==0) printf("     CreatePlan for Forward....DONE\n");
    fftwnd_mpi_local_sizes(plan,&nx,&start_x,&nx_out,&start_x_out,&size);
    if (PrR==0) printf("     LocalSizes................DONE\n");
    local=(fftw_complex*)malloc(size*sizeof(fftw_complex));
    work=(fftw_complex*)malloc(size*sizeof(fftw_complex));
    preal=(fftw_real*)local;
    srand(PrR*100);
    for(i=0;i<2*size;i++)
    	preal[i]=(fftw_real)rand()/RAND_MAX;

    fftwnd_mpi(plan,1,local,NULL,FFTW_NORMAL_ORDER);
    if (PrR==0) printf("     Forward FFTW..............DONE\n");

    fftwnd_mpi_destroy_plan(plan);
    if (PrR==0) printf("     Destroy plan for Forward..DONE\n");

    plan=fftw3d_mpi_create_plan(MPI_COMM_WORLD,lenx,leny,lenz,FFTW_BACKWARD,FFTW_ESTIMATE);
    if (PrR==0) printf("     CreatePlan for Backward...DONE\n");

    fftwnd_mpi(plan,1,local,NULL,FFTW_NORMAL_ORDER);
    scale=1.0/lenx/leny/lenz;
    if (PrR==0) printf("     Backward FFTW.............DONE\n");
    for(i=0;i<2*size;i++)
    	preal[i]*=scale;
    fftwnd_mpi_destroy_plan(plan);
    if (PrR==0) printf("     Destroy plan for Backward.DONE\n");
    srand(PrR*100);
    err=0.0;
    for(i=0;i<2*size;i++) {
    	t=fabs(preal[i]-(fftw_real)rand()/RAND_MAX);
	if(t>err) err=t;
    };
    free(local);
    free(work);
    if (PrR==0) printf("     Error=%e\n",err);
    if (PrR==0)
    {
	if (err<EPS) printf(" TEST PASSED\n\n"); else printf(" TEST FAILED\n");
    }

/*------------    Usage of MPI FFTW for Complex Multi-dimensional Transforms   fftw3d_create_plan---*/
    if (PrR==0) printf("Usage of MPI FFTW for Complex Multi-dimensional Transforms .\nPlan = fftw3d_mpi_create_plan(...)\n");

    if(PrR==0) printf("PrS=%d,lengths=(%d,%d,%d)\n",PrS,lenx,leny,lenz);
    plan=fftw3d_mpi_create_plan(MPI_COMM_WORLD,lenx,leny,lenz,FFTW_FORWARD,FFTW_ESTIMATE);
    if (PrR==0) printf("     CreatePlan for Forward....DONE\n");
    fftwnd_mpi_local_sizes(plan,&nx,&start_x,&nx_out,&start_x_out,&size);
    if (PrR==0) printf("     LocalSizes................DONE\n");
    local=(fftw_complex*)malloc(size*sizeof(fftw_complex));
    work=(fftw_complex*)malloc(size*sizeof(fftw_complex));
    preal=(fftw_real*)local;
    srand(PrR*100);
    for(i=0;i<2*size;i++)
    	preal[i]=(fftw_real)rand()/RAND_MAX;

    fftwnd_mpi(plan,1,local,work,FFTW_NORMAL_ORDER);
    if (PrR==0) printf("     Forward FFTW..............DONE\n");

    fftwnd_mpi_destroy_plan(plan);
    if (PrR==0) printf("     Destroy plan for Forward..DONE\n");

    plan=fftw3d_mpi_create_plan(MPI_COMM_WORLD,lenx,leny,lenz,FFTW_BACKWARD,FFTW_ESTIMATE);
    if (PrR==0) printf("     CreatePlan for Backward...DONE\n");

    fftwnd_mpi(plan,1,local,work,FFTW_NORMAL_ORDER);
    scale=1.0/lenx/leny/lenz;
    if (PrR==0) printf("     Backward FFTW.............DONE\n");
    for(i=0;i<2*size;i++)
    	preal[i]*=scale;
    fftwnd_mpi_destroy_plan(plan);
    if (PrR==0) printf("     Destroy plan for Backward.DONE\n");
    srand(PrR*100);
    err=0.0;
    for(i=0;i<2*size;i++) {
    	t=fabs(preal[i]-(fftw_real)rand()/RAND_MAX);
	if(t>err) err=t;
    };
    free(local);
    free(work);
    if (PrR==0) printf("     Error=%e\n",err);
    if (PrR==0)
    {
	if (err<EPS) printf(" TEST PASSED\n\n"); else printf(" TEST FAILED\n");
    }

    MPI_Finalize();
    return 0;
}
