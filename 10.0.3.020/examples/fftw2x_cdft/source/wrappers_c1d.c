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
!       Forward-Backward 1D complex transform for double/single precision data.
!
!*******************************************************************************
!  Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX      (obligatory)
!           DFTI_PRECISION      = DFTI_DOUBLE       (default)
!           DFTI_DIMENSION      = 1                 (obligatory)
!           DFTI_LENGTHS        = {len}             (obligatory)
!           DFTI_FORWARD_SCALE  = 1.0               (default)
!           DFTI_BACKWARD_SCALE = 1.0/(len)         (default=1.0)
!
!******************************************************************************/
#include <stdlib.h>
#include "fftw_mpi.h"
#include "mpi.h"

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
    fftw_mpi_plan plan;

    int len,nx,nx_out,start_x,start_x_out,size;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&PrS);
    MPI_Comm_rank(MPI_COMM_WORLD,&PrR);
    len = 256;
    if(PrR==0) printf("PrS=%d,lengths=%d\n",PrS,len);


/*--------------    Usage of MPI FFTW for Complex One-dimensional Transforms  (work==NULL)--------------*/

    if (PrR==0) printf("Usage of MPI FFTW for Complex One-dimensional Transforms (work==NULL)\n");
    plan=fftw_mpi_create_plan(MPI_COMM_WORLD,len,FFTW_FORWARD,FFTW_ESTIMATE);
    if (PrR==0) printf("     CreatePlan for Forward....DONE\n");
    fftw_mpi_local_sizes(plan,&nx,&start_x,&nx_out,&start_x_out,&size);
    if (PrR==0) printf("     LocalSizes................DONE\n");
    local=(fftw_complex*)malloc(size*sizeof(fftw_complex));
    work=(fftw_complex*)malloc(size*sizeof(fftw_complex));
    preal=(fftw_real*)local;
    srand(PrR*100);
    for(i=0;i<2*nx;i++)
    	preal[i]=(fftw_real)rand()/RAND_MAX;
    fftw_mpi(plan,1,local,NULL);
    if (PrR==0) printf("     Forward FFTW..............DONE\n");

    fftw_mpi_destroy_plan(plan);
    if (PrR==0) printf("     Destroy plan for Forward..DONE\n");
    plan=fftw_mpi_create_plan(MPI_COMM_WORLD,len,FFTW_BACKWARD,FFTW_ESTIMATE);
    if (PrR==0) printf("     CreatePlan for Backward...DONE\n");
    fftw_mpi(plan,1,local,NULL);
    scale=1.0/len;
    if (PrR==0) printf("     Backward FFTW.............DONE\n");
    for(i=0;i<2*nx;i++)
    	preal[i]*=scale;
    fftw_mpi_destroy_plan(plan);
    if (PrR==0) printf("     Destroy plan for Backward.DONE\n");
    srand(PrR*100);
    err=0.0;
    for(i=0;i<2*nx;i++) {
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

/*--------------    Usage of MPI FFTW for Complex One-dimensional Transforms  --------------*/
    if (PrR==0) printf("Usage of MPI FFTW for Complex One-dimensional Transforms \n");
    plan=fftw_mpi_create_plan(MPI_COMM_WORLD,len,FFTW_FORWARD,FFTW_ESTIMATE);
    if (PrR==0) printf("     CreatePlan for Forward....DONE\n");
    fftw_mpi_local_sizes(plan,&nx,&start_x,&nx_out,&start_x_out,&size);
    if (PrR==0) printf("     LocalSizes................DONE\n");
    local=(fftw_complex*)malloc(size*sizeof(fftw_complex));
    work=(fftw_complex*)malloc(size*sizeof(fftw_complex));
    preal=(fftw_real*)local;
    srand(PrR*100);
    for(i=0;i<2*nx;i++)
    	preal[i]=(fftw_real)rand()/RAND_MAX;

    fftw_mpi(plan,1,local,work);
    if (PrR==0) printf("     Forward FFTW..............DONE\n");

    fftw_mpi_destroy_plan(plan);
    if (PrR==0) printf("     Destroy plan for Forward..DONE\n");
    plan=fftw_mpi_create_plan(MPI_COMM_WORLD,len,FFTW_BACKWARD,FFTW_ESTIMATE);
    if (PrR==0) printf("     CreatePlan for Backward...DONE\n");
    fftw_mpi(plan,1,local,work);
    scale=1.0/len;
    if (PrR==0) printf("     Backward FFTW.............DONE\n");
    for(i=0;i<2*nx;i++)
    	preal[i]*=scale;
    fftw_mpi_destroy_plan(plan);
    if (PrR==0) printf("     Destroy plan for Backward.DONE\n");
    srand(PrR*100);
    err=0.0;
    for(i=0;i<2*nx;i++) {
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

