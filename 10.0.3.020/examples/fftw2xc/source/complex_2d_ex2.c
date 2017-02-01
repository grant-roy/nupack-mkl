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
!   Forward-Backward 2D complex transform for double precision data not inplace.
!
!   Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX          (obligatory)
!           DFTI_PRECISION      = DFTI_DOUBLE           (obligatory)
!           DFTI_DIMENSION      = 2                     (obligatory)
!           DFTI_LENGTHS        = { m, n}               (obligatory)
!           DFTI_PLACEMENT      = DFTI_NOT_INPLACE      (default= DFTI_INPLACE)
!           DFTI_INPUT_STRIDES  = {0, N_MAX, 1}         (default = {0,n,1})
!           DFTI_OUTPUT_STRIDES = {0, N_MAX, 1}         (default = {0,n,1})
!           DFTI_FORWARD_SCALE  = 1.0                   (default)
!           DFTI_BACKWARD_SCALE = 1.0/(double)(m*n)     (default=1.0)
!
!   Other default configuration parameters are in the mkl_dfti.h interface file
!******************************************************************************/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "fftw.h"

#include "mkl_dfti_examples.h"

int main()  /* COMPLEX_2D_EX2 */
{
    /*
    /   DFT input parameters
    */
    int  m, n;
    int  i, j;
	fftw_direction dir;
    /* */
    TYPE_PRECISION  Scale;
    TYPE_PRECISION  maxerr;
    TYPE_PRECISION  eps= EPS;

    fftwnd_plan Desc_Handle;
    fftw_complex x_in[M_MAX][N_MAX];
    fftw_complex x_out[M_MAX][N_MAX];
	/* */
    fftw_complex x_exp[M_MAX][N_MAX];

    /*
    /  Put transform parameters
    */
    m = M_MAX;
    n = N_MAX;

    if(LEGEND_PRINT) {
        printf(" \n\n COMPLEX_2D_EX2  \n");
        printf(" Forward-Backward 2D complex transform for double/single precision data\n\n");
        printf(" Configuration parameters:                  \n\n");
        printf(" DFTI_FORWARD_DOMAIN = DFTI_COMPLEX         \n");
        printf(" DFTI_PRECISION      = DFTI_DOUBLE/SINGLE   \n");
        printf(" DFTI_DIMENSION      = 2                    \n");
        printf(" DFTI_LENGTHS        = {%d,%d)              \n", m, n);
        printf(" DFTI_PLACEMENT      = DFTI_NOT_INPLACE     \n");
        printf(" DFTI_INPUT_STRIDES  = {0, %d, 1}           \n", N_MAX);
        printf(" DFTI_OUTPUT_STRIDES = {0, %d, 1}           \n", N_MAX);
        printf(" DFTI_FORWARD_SCALE  = 1.0                  \n");
        printf(" DFTI_BACKWARD_SCALE = 1.0/(double)(m*n)    \n\n");
    }

    /*
    /   Check test input parameters
    */
    if((m*n) > (M_MAX* N_MAX)) {
        printf(" Error input parameters: (m*n)>(M_MAX*N_MAX)\n");
        printf(" Please see mkl_dfti_examples.h file\n");
        printf(" TEST FAIL\n");  goto END_OF_TEST;
    }

    /*
    /   put input data and expected result
    */
#ifdef MKL_DOUBLE
    zero_init_z(x_in,  M_MAX*N_MAX);
    zero_init_z(x_out, M_MAX*N_MAX);
    init_multiple_columns_z(x_in, m, n, 0, N_MAX);

    cblas_zcopy(M_MAX*N_MAX, x_in, 1, x_exp, 1);
#else
    zero_init_c(x_in,  M_MAX*N_MAX);
    zero_init_c(x_out, M_MAX*N_MAX);
    init_multiple_columns_c(x_in, m, n, 0, N_MAX);

    cblas_ccopy(M_MAX*N_MAX, x_in, 1, x_exp, 1);
#endif

    /*
    /   Create Dfti descriptor for 2D double/single precision forward transform
    */
    Desc_Handle = fftw2d_create_plan( m, n, FFTW_FORWARD, FFTW_ESTIMATE);
    /*
    /   Compute DFT
    */
    fftwnd_one(Desc_Handle, &x_in[0][0], &x_out[0][0]);

/*    {
		int i;
		for (i=0; i<M_MAX; i++) {
			for (j=0; j<N_MAX; j++) {
            printf(" backward: x_out[%d][%d].re = %f, x_out[%d][%d].im = %f\n", i, j, x_out[i][j].re, i, j, x_out[i][j].im);
			}
		}
	} */
    /*
    /   Destroy Dfti descriptor
    */
    fftwnd_destroy_plan(Desc_Handle);

    /*
    /   Set Scale number for Backward transform
    */
    Scale = 1.0/(TYPE_PRECISION)(m*n);
    /*
    /   Create Dfti descriptor for 2D double precision backward transform
    */
    Desc_Handle = fftw2d_create_plan( m, n, FFTW_BACKWARD, FFTW_ESTIMATE);
    /*
    /   Compute DFT
    */
    fftwnd_one(Desc_Handle, &x_out[0][0], &x_in[0][0]);

/*    {
		int i;
		for (i=0; i<M_MAX; i++) {
			for (j=0; j<N_MAX; j++) {
            printf(" backward: x_in[%d][%d].re = %f, x_in[%d][%d].im = %f\n", i, j, x_in[i][j].re, i, j, x_in[i][j].im);
			}
		}
	} */

    /*
    /   Destroy Dfti descriptor
    */
    fftwnd_destroy_plan(Desc_Handle);
    /*
    /   Result scaling
    */
    scaling(x_in, Scale, m*n);

    /*
    /   Check result
    */
#ifdef MKL_DOUBLE
    maxerr = check_result_z(x_in, x_exp, M_MAX*N_MAX);
#else
    maxerr = check_result_c(x_in, x_exp, M_MAX*N_MAX);
#endif

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
