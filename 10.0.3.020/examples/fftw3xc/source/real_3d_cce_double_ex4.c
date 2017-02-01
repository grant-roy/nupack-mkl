/*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 2005-2008 Intel Corporation. All Rights Reserved.
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
!          MKL DFTI implementation through FFTW interface (via wrappers) example
!          program (C-interface)
!
!   Forward-Backward 3D real transform for double precision data inplace.
!
!   Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_REAL             (obligatory)
!           DFTI_PRECISION      = DFTI_DOUBLE           (obligatory)
!           DFTI_DIMENSION      = 3                     (obligatory)
!           DFTI_LENGTHS        = { m, n, k}            (obligatory)
!           DFTI_PLACEMENT      = DFTI_NOT_INPLACE      (default)
!           DFTI_INPUT_STRIDES  = {first_index, step_in_m, step_in_n, step_in_k}
!                                                       (default = {0,n*k,k,1})
!           DFTI_FORWARD_SCALE  = 1.0                   (default)
!           DFTI_BACKWARD_SCALE = 1.0/(double)(m*n*k)   (default=1.0)
!           DFTI_NUMBER_OF_TRANSFORMS = 1               (default)
!
!  Other default configuration parameters are in the mkl_dfti.h interface file
!******************************************************************************/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "fftw3.h"

#include "mkl_dfti_examples.h"

int main()  /* REAL_3D_CCE_DOUBLE_EX4 */
{
    /*
    /   DFT input parameters
    */
    fftw_plan Desc_Handle;
    double x_in[M_MAX][N_MAX][K_MAX];
    fftw_complex x_out[M_MAX][N_MAX*2][K_MAX/2+1];
    long     m=5;
    long     n=4;
    long     k=3;
    long    step_in_k=2;
	/* */
    double x_exp[M_MAX][N_MAX][K_MAX];
    int rank, istride, idist, ostride, odist, multiple;
    int inembed[3];
    int onembed[3];
    double  Scale;
    long    strides_in[4];
    long    strides_out[4];
    double  maxerr;
    double  eps= DOUBLE_EPS;
    long i;
	int     nn[3];

    /*
    /  Put transform parameters
    */
    strides_in[0]   = 0;
    strides_in[1]   = step_in_k * N_MAX*K_MAX;
    strides_in[2]   = step_in_k * K_MAX;
    strides_in[3]   = step_in_k * 1;

    nn[0] = m;
    nn[1] = n;
    nn[2] = k;

    multiple = 1;
	rank = 3;
    idist = 1;
	inembed[0] = M_MAX;
	inembed[1] = N_MAX;
	inembed[2] = K_MAX;
	istride = strides_in[3];
    odist = 1;
	onembed[0] = M_MAX;
	onembed[1] = N_MAX*2;
	onembed[2] = K_MAX/2+1;
	ostride = strides_in[3];

    if(LEGEND_PRINT) {
        printf(" \n\n REAL_3D_CCE_DOUBLE_EX4  \n");
        printf(" Forward-Backward 3D real transform for double precision data\n\n");
        printf(" Configuration parameters:                  \n\n");
        printf(" DFTI_FORWARD_DOMAIN = DFTI_REAL         \n");
        printf(" DFTI_PRECISION      = DFTI_DOUBLE          \n");
        printf(" DFTI_DIMENSION      = 3                    \n");
        printf(" DFTI_LENGTHS        = {%d,%d,%d)           \n", m, n, k);
        printf(" DFTI_PLACEMENT      = DFTI_NOT_INPLACE     \n");
        printf(" DFTI_INPUT_STRIDES  = {%d, %d, %d, %d}     \n",
                strides_in[0], strides_in[1], strides_in[2], strides_in[3]);
        printf(" DFTI_FORWARD_SCALE  = 1.0                  \n");
        printf(" DFTI_BACKWARD_SCALE = 1.0/(double)(m*n*k)   \n\n");
    }

    /*
    /   Check test input parameters
    */
    if( (step_in_k*m > M_MAX) ||
        (step_in_k*n > N_MAX) ||
        (step_in_k*k > K_MAX) )
    {
        printf(" Error input parameters\n");
        printf(" TEST FAIL\n");  goto END_OF_TEST;
    }

    /*
    /   put input data and expected result
    */
    zero_init_d(x_in, M_MAX*N_MAX*K_MAX);

    init_3d_columns_d(x_in, m, n, k, strides_in);

    cblas_dcopy(M_MAX*N_MAX*K_MAX, x_in, 1, x_exp, 1);

    /*
    /   Create Dfti descriptor for 3D double precision forward transform
    */
    Desc_Handle = fftw_plan_many_dft_r2c( rank, nn, multiple, &x_in[0][0][0], inembed, istride, idist,
	                      &x_out[0][0][0], onembed, ostride, odist, FFTW_ESTIMATE);
    /*
    /   Compute DFT
    */
    fftw_execute(Desc_Handle);

	/*
    /   Destroy Dfti descriptor
    */
    fftw_destroy_plan(Desc_Handle);

    /*
    /   Set Scale number for Backward transform
    */
    Scale = 1.0/(double)(m*n*k);
    /*
    /   Create Dfti descriptor for 3D double precision backward transform
    */
    Desc_Handle = fftw_plan_many_dft_c2r( rank, nn, multiple, &x_out[0][0][0], onembed, ostride, odist,
	               &x_in[0][0][0], inembed, istride, idist, FFTW_ESTIMATE);
    /*
    /   Compute DFT
    */
    fftw_execute(Desc_Handle);
    /*
    /   Destroy Dfti descriptor
    */
    fftw_destroy_plan(Desc_Handle);
    /*
    /   Result scaling
    */
    scaling_dr(x_in, Scale, M_MAX*N_MAX*K_MAX);

    /*
    /   Check result
    */
    maxerr = check_result_d(x_in, x_exp, M_MAX*N_MAX*K_MAX);
    if(ACCURACY_PRINT)
    printf("\n ACCURACY = %g\n\n", maxerr);

    if(maxerr < eps){
        printf(" TEST PASSED\n");
    } else {
        printf(" TEST FAIL\n");
    }

END_OF_TEST:
    return 0;
}
