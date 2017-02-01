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
!          MKL DFTI implementation through FFTW interface (via wrappers) example
!          program (C-interface)
!
!   Forward-Backward 3D complex transform for double precision data inplace.
!
!   Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX          (obligatory)
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
#include "fftw.h"

#include "mkl_dfti_examples.h"

int main()  /* COMPLEX_3D_EX2 */
{
    /*
    /   DFT input parameters
    */
    fftwnd_plan Desc_Handle;
    fftw_complex x_in[M_MAX][N_MAX][K_MAX];
    int     m=M_MAX;
    int     n=N_MAX;
    int     k=K_MAX;
    int    first_index_in=0;
    int    step_in_m=1;
    int    step_in_n=1;
    int    step_in_k=1;
	/* */
    fftw_complex x_exp[M_MAX][N_MAX][K_MAX];
    fftw_complex x_out[M_MAX][N_MAX][K_MAX];
	fftw_direction dir;
    /* */
    TYPE_PRECISION  Scale;
    TYPE_PRECISION  maxerr;
    TYPE_PRECISION  eps= EPS;
    long    strides_in[4];
    int    i;

    /*
    /  Put transform parameters
    */
    strides_in[0]   = (long)first_index_in;
    strides_in[1]   = (long)step_in_m * N_MAX*K_MAX;
    strides_in[2]   = (long)step_in_n * K_MAX;
    strides_in[3]   = (long)step_in_k * 1;

    if(LEGEND_PRINT) {
        printf(" \n\n COMPLEX_3D_EX2  \n");
        printf(" Forward-Backward 3D complex transform for double precision data\n\n");
        printf(" Configuration parameters:                  \n\n");
        printf(" DFTI_FORWARD_DOMAIN = DFTI_COMPLEX         \n");
        printf(" DFTI_PRECISION      = DFTI_DOUBLE          \n");
        printf(" DFTI_DIMENSION      = 3                    \n");
        printf(" DFTI_LENGTHS        = {%d,%d,%d)           \n", m, n, k);
        printf(" DFTI_PLACEMENT      = DFTI_NOT_INPLACE         \n");
        printf(" DFTI_INPUT_STRIDES  = {%ld, %ld, %ld, %ld}     \n",
                strides_in[0], strides_in[1], strides_in[2], strides_in[3]);
        printf(" DFTI_FORWARD_SCALE  = 1.0                  \n");
        printf(" DFTI_BACKWARD_SCALE = 1.0/(double)(m*n*k)   \n\n");
    }

    /*
    /   put input data and expected result
    */
#ifdef MKL_DOUBLE
    zero_init_z(x_in, M_MAX*N_MAX*K_MAX);

    init_3d_columns_z(x_in, m, n, k, strides_in);

    cblas_zcopy(M_MAX*N_MAX*K_MAX, x_in, 1, x_exp, 1);
#else
    zero_init_c(x_in, M_MAX*N_MAX*K_MAX);

	init_3d_columns_c(x_in, m, n, k, strides_in);

	cblas_ccopy(M_MAX*N_MAX*K_MAX, x_in, 1, x_exp, 1);
#endif
    /*
    /   Create Dfti descriptor for 3D double precision forward transform
    */
    Desc_Handle = fftw3d_create_plan( m, n, k, FFTW_FORWARD, FFTW_ESTIMATE);
    /*
    /   Compute DFT
    */
    fftwnd_one(Desc_Handle, &x_in[0][0][0], &x_out[0][0][0]);

    /*
    /   Destroy Dfti descriptor
    */
    fftwnd_destroy_plan(Desc_Handle);

    /*
    /   Set Scale number for Backward transform
    */
    Scale = 1.0/(double)(m*n*k);
    /*
    /   Create Dfti descriptor for 3D double precision backward transform
    */
    Desc_Handle = fftw3d_create_plan( m, n, k, FFTW_BACKWARD, FFTW_ESTIMATE);
    /*
    /   Compute DFT
    */
    fftwnd_one(Desc_Handle, &x_out[0][0][0], &x_in[0][0][0]);
    /*
    /   Destroy Dfti descriptor
    */
    fftwnd_destroy_plan(Desc_Handle);
    /*
    /   Result scaling
    */
    scaling(x_in, Scale, M_MAX*N_MAX*K_MAX);

    /*
    /   Check result
    */
#ifdef MKL_DOUBLE
    maxerr = check_result_z(x_in, x_exp, M_MAX*N_MAX*K_MAX);
#else
    maxerr = check_result_c(x_in, x_exp, M_MAX*N_MAX*K_MAX);
#endif
    if(ACCURACY_PRINT)
    printf("\n ACCURACY = %g\n\n", maxerr);

    if(maxerr < eps){
        printf(" TEST PASSED\n");
    } else {
        printf(" TEST FAIL\n");
    }

    return 0;
}
