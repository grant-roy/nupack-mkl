/*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 2003-2008 Intel Corporation. All Rights Reserved.
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
!       MKL Cluster DFT interface example program (C-interface)
!
!       Forward-Backward 2D complex transform for double precision data.
!
!*******************************************************************************
!  Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX      (obligatory)
!           DFTI_PRECISION      = DFTI_DOUBLE       (obligatory)
!           DFTI_DIMENSION      = 2                 (obligatory)
!           DFTI_LENGTHS        = { m, n}           (obligatory)
!           DFTI_FORWARD_SCALE  = 1.0               (default)
!           DFTI_BACKWARD_SCALE = 1.0/(m*n)         (default=1.0)
!
!*******************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"
//#include "mkl_dfti_cluster.h"
#include "mkl_cdft.h"

#include "mkl_cdft_examples.h"
#define PREC DFTI_DOUBLE

int main(int argc, char *argv[])  /* DM_COMPLEX_2D_DOUBLE_EX1 */
{
    mkl_double_complex  *x_in;
    mkl_double_complex  *x_exp;
	mkl_double_complex  *local,*work;

    DFTI_DESCRIPTOR_DM_HANDLE desc;
    long RootRank,ElementSize,i,j;
    MKL_LONG nx,nx_out,start_x,start_x_out,size;

    long    m;
    long    n;

    long    Status;
    double  Scale;
    MKL_LONG lengths[2];

    double  maxerr;
    double  eps = DOUBLE_EPS;

    int MPI_err;
    int MPI_nProc;
    int MPI_Rank;

    /*
	/	1. Initiate MPI by calling MPI_Init (Perform MPI initialization)
    */
    MPI_err = MPI_Init(&argc, &argv);

    if(MPI_err != MPI_SUCCESS) {
	printf(" MPI initialization error\n");
	printf(" TEST FAILED\n");
	return 1;
    }

    MPI_Comm_size(MPI_COMM_WORLD, &MPI_nProc);
    MPI_Comm_rank(MPI_COMM_WORLD, &MPI_Rank);
    if (MPI_Rank == 0) printf( " Program is running on %d processes\n", MPI_nProc);

    /*
    /   Read input data from input file
    /   m - size of transform  along first dimension
    /   n - size of transform  along second dimension
    */
    if (MPI_Rank == 0) if(read_data_file_2d(argc, argv, &m, &n)) goto CLOSE_MPI;
    MPI_Bcast(&m,1,MPI_LONG_INT,0,MPI_COMM_WORLD);
    MPI_Bcast(&n,1,MPI_LONG_INT,0,MPI_COMM_WORLD);

    if(LEGEND_PRINT && (MPI_Rank == 0)) {
        printf("\n DM_COMPLEX_2D_DOUBLE_EX2  \n");
        printf(" Forward-Backward 2D complex transform for double precision data\n\n");
        printf(" Configuration parameters:                  \n\n");
        printf(" DFTI_FORWARD_DOMAIN = DFTI_COMPLEX         \n");
        printf(" DFTI_PRECISION      = DFTI_DOUBLE          \n");
        printf(" DFTI_DIMENSION      = 2                    \n");
        printf(" DFTI_LENGTHS (MxN)  = {%ld,%ld)              \n", m, n);
        printf(" DFTI_FORWARD_SCALE  = 1.0                  \n");
        printf(" DFTI_BACKWARD_SCALE = 1.0/(m*n)\n\n");
    }


    lengths[0] = m;
    lengths[1] = n;
	/*
    /	Allocate memory for dynamic arrays
    */
    x_in = (mkl_double_complex *)malloc(sizeof(mkl_double_complex)*m*n);
    x_exp = (mkl_double_complex *)malloc(sizeof(mkl_double_complex)*m*n);

	/*
    /	Put input data and expected result
    */
	init_data_2d_z(x_in, m, n);
	memcpy(x_exp,x_in,sizeof(mkl_double_complex)*m*n);

	 /* print input data assembled together */
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) {
    printf("\n INPUT Global vector X, n columns \n");
	print_data_2d_z(x_in, m, n, n);
	}

	/*
	/ 2. Allocate memory for the descriptor by calling DftiCreateDescriptorDM.
    / (Create DftiDM descriptor for 2D double precision transform)
	*/
	Status=DftiCreateDescriptorDM(MPI_COMM_WORLD,&desc,PREC,DFTI_COMPLEX,2,lengths);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Create=%ld\n",Status);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_MEM;
    }

	/*
	/  3. Obtain some values of configuration parameters by calls to DftiGetValueDM
	*/
	Status=DftiGetValueDM(desc,CDFT_LOCAL_SIZE,&size);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Get=%ld,size=%ld\n",Status,size);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	Status=DftiGetValueDM(desc,CDFT_LOCAL_NX,&nx);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Get=%ld,nx=%ld\n",Status,nx);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	Status=DftiGetValueDM(desc,CDFT_LOCAL_X_START,&start_x);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Get=%ld,start_x=%ld\n",Status,start_x);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	Status=DftiGetValueDM(desc,CDFT_LOCAL_OUT_NX,&nx_out);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Get=%ld,nx_out=%ld\n",Status,nx_out);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	Status=DftiGetValueDM(desc,CDFT_LOCAL_OUT_X_START,&start_x_out);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Get=%ld,start_x_out=%ld\n",Status,start_x_out);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	local=(mkl_double_complex*)malloc(size*sizeof(mkl_double_complex));
	work=(mkl_double_complex*)malloc(size*sizeof(mkl_double_complex));

	/*
	 4. Specify a value(s) of configuration parameters by a call(s) to DftiSetValueDM.
	*/
	Status=DftiSetValueDM(desc,CDFT_WORKSPACE,work);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Set=%ld,pointer=%p\n",Status,work);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	Status=DftiSetValueDM(desc,DFTI_PLACEMENT,DFTI_NOT_INPLACE);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("placement=out-of-place,%ld\n",Status);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }


	/*
	 5. Perform initialization that facilitates DFT computation by a call to
		DftiCommitDescriptorDM.
	*/
	Status=DftiCommitDescriptorDM(desc);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Commit=%ld\n",Status);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	/*
 	/ 6. Create arrays for local parts of input and output data (if it is needed) and fill the local part of input data with
	/	values (for more information, see Distributing Data among Processes).
	/	(Spread data among processors)
	*/
	RootRank=0;
	ElementSize=sizeof(mkl_double_complex);
	Status=MKL_CDFT_ScatterData(MPI_COMM_WORLD,RootRank,ElementSize,2,lengths,x_in,nx,start_x,local);
    if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Scatter=%ld\n",Status);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	/*
	/ 7. Compute the transform by calling DftiComputeForwardDM or DftiComputeBackward.
    / (Compute Forward transform)
	*/

	Status=DftiComputeForwardDM(desc,local,work);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("ComputeForward=%ld\n",Status);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }


	/*
    /   Gather data among processors
    */
	Status=MKL_CDFT_GatherData(MPI_COMM_WORLD,RootRank,ElementSize,2,lengths,x_in,nx,start_x,work);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Gather=%ld\n",Status);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	/*
    /   Set Scale number for Backward transform
    */
	Scale = 1.0/(double)(m*n);
    if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf(" \n\n DFTI_BACKWARD_SCALE  = 1/(m*n) \n");
	Status=DftiSetValueDM(desc,DFTI_BACKWARD_SCALE,Scale);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Set=%ld\n",Status);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	/*
    /   Commit DftiDM descriptor
    */
 	Status=DftiCommitDescriptorDM(desc);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Commit=%ld\n",Status);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	/*
    /   Spread data among processors
    */
	Status=MKL_CDFT_ScatterData(MPI_COMM_WORLD,RootRank,ElementSize,2,lengths,x_in,nx,start_x,work);
    if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Scatter=%ld\n",Status);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	/*
    /   Compute Backward transform
    */
    Status=DftiComputeBackwardDM(desc,work,local);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("ComputeBackward=%ld\n",Status);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	/*
	 8. Gather local output data into the global array using MPI functions or use them otherwise.
	*/
	Status=MKL_CDFT_GatherData(MPI_COMM_WORLD,RootRank,ElementSize,2,lengths,x_in,nx,start_x,local);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("Gather=%ld\n",Status);
	if(Status!=DFTI_NO_ERROR){
	if (MPI_Rank == 0) {
	    dfti_example_status_print(Status);
	    printf(" TEST FAILED\n");
	}
	goto FREE_DESCRIPTOR;
    }

	/* print data after DftiComputeBackwardDM; data assembled together */
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)){
    printf("\n Backward result X, n columns \n");
	print_data_2d_z(x_in, m, n, n);
	}

	/*
    /   Check result
    */
	    if (MPI_Rank == 0) {
	maxerr = check_result_z(x_in, x_exp, m*n);
	if(ACCURACY_PRINT) printf("\n ACCURACY = %g\n\n", maxerr);
	    if(maxerr < eps){
		printf(" TEST PASSED\n");
	    } else {
		printf(" TEST FAILED\n");
	    }
    }

FREE_DESCRIPTOR:

    /*
    /   9. Release memory allocated for a descriptor by a call to DftiFreeDescriptorDM.
	/	(Free DftiDM descriptor)
    */
    Status = DftiFreeDescriptorDM(&desc);
	if(ADVANCED_DATA_PRINT && (MPI_Rank == 0)) printf("FreeDescriptor=%ld\n",Status);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR) && (MPI_Rank == 0)){
	dfti_example_status_print(Status);
	printf(" TEST FAILED\n");
    }

FREE_MEM:

    /*
	/	Deallocate memory for dynamic arrays
    */
    free(x_in);
    free(x_exp);
	free(local);
	free(work);

CLOSE_MPI:

    /*
	/   10. Finalize communication through MPI by calling MPI_Finalize
    /   (Finalize MPI)
    */
    MPI_Finalize();

    return 0;
}
