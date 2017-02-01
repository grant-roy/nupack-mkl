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
!   Forward-Backward 3D complex transform for double precision data not inplace.
!
!   Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX          (obligatory)
!           DFTI_PRECISION      = DFTI_DOUBLE           (obligatory)
!           DFTI_DIMENSION      = 3                     (obligatory)
!           DFTI_LENGTHS        = { m, n}               (obligatory)
!           DFTI_PLACEMENT      = DFTI_NOT_INPLACE          (default= DFTI_INPLACE)
!           DFTI_INPUT_STRIDES  =                       (default = {0,n*k,k,1})
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

int main()  /* COMPLEX_3D_EX12*/
{
    /*
    /   DFT input parameters
    */
    int m = 4;
	int n = 5;
	int k = 6;
	int rank = 3;
	int multiple = 3;
	int istride = 2;
	int idist = 800;
	int ostride = 2;
	int odist = 770;

	fftw_direction dir;
    /* */
    TYPE_PRECISION  Scale;
    TYPE_PRECISION  maxerr;
    TYPE_PRECISION  eps= EPS;

    fftwnd_plan Desc_Handle;
    fftw_complex* x_in;
    fftw_complex* x_out;

	/* */
    fftw_complex* x_exp;
	int lda, lda_out;
    int i, j;
	int nn[3];

	/*
    /  Put transform parameters
    */

    if(LEGEND_PRINT) {
        printf(" \n\n COMPLEX_3D_EX12 \n");
        printf(" Forward-Backward 3D complex transform for double/single precision data\n\n");
        printf(" Configuration parameters:                  \n\n");
        printf(" DFTI_FORWARD_DOMAIN = DFTI_COMPLEX         \n");
        printf(" DFTI_PRECISION      = DFTI_DOUBLE/SINGLE   \n");
        printf(" DFTI_DIMENSION      = 2                    \n");
        printf(" DFTI_LENGTHS        = {%d,%d)              \n", m, n);
        printf(" DFTI_PLACEMENT      = DFTI_INPLACE         \n");
        printf(" DFTI_INPUT_STRIDES  = {0, %d, %d, %d}           \n", n*k*istride, k*istride, istride);
        printf(" DFTI_OUTPUT_STRIDES = {0, %d, %d, %d}           \n", n*k*istride, k*istride, istride);
        printf(" DFTI_FORWARD_SCALE  = 1.0                  \n");
        printf(" DFTI_BACKWARD_SCALE = 1.0/(double)(m*n)    \n\n");
    }

    /*
    /   put input data and expected result
    */

	lda = m*n*k*istride*multiple;
	if (idist > m*n*k*istride) lda = idist*multiple;
	lda_out = m*n*k*ostride*multiple;
	if (odist > m*n*k*ostride) lda_out = odist*multiple;

    x_in = (fftw_complex*)malloc(2*lda*sizeof(TYPE_PRECISION));
    x_exp = (fftw_complex*)malloc(2*lda*sizeof(TYPE_PRECISION));
    x_out = (fftw_complex*)malloc(2*lda_out*sizeof(TYPE_PRECISION));

#ifdef MKL_DOUBLE
    zero_init_z(x_in,  lda);
    init_multiple_columns_step_z(x_in, m*n*k, multiple, idist, istride);

    cblas_zcopy(lda, x_in, 1, x_exp, 1);
#else
    zero_init_c(x_in,  lda);
    init_multiple_columns_step_c(x_in, m*n*k, multiple, idist, istride);

    cblas_ccopy(lda, x_in, 1, x_exp, 1);
#endif

    nn[0] = m;
    nn[1] = n;
    nn[2] = k;
	/*
    /   Create Dfti descriptor for 3D double/single precision forward transform
    */
    Desc_Handle = fftwnd_create_plan( rank, nn, FFTW_FORWARD, FFTW_ESTIMATE);

    /*
    /   Compute DFT
    */
	fftwnd(Desc_Handle, multiple, x_in, istride, idist, x_out, ostride, odist);


    /*
    /   Destroy Dfti descriptor
    */
    fftwnd_destroy_plan(Desc_Handle);

    /*
    /   Set Scale number for Backward transform
    */
    Scale = 1.0/(TYPE_PRECISION)(m*n*k);
    /*
    /   Create Dfti descriptor for 3D double precision backward transform
    */
    Desc_Handle = fftwnd_create_plan( rank, nn, FFTW_BACKWARD, FFTW_ESTIMATE);
    /*
    /   Compute DFT
    */
    fftwnd(Desc_Handle, multiple, x_out, ostride, odist, x_in, istride, idist);

    /*
    /   Destroy Dfti descriptor
    */
    fftwnd_destroy_plan(Desc_Handle);
    /*
    /   Result scaling
    */
    scaling_multiple(x_in, Scale, m*n*k, multiple, istride, idist);

    /*
    /   Check result
    */
#ifdef MKL_DOUBLE
    maxerr = check_result_z(x_in, x_exp, lda);
#else
    maxerr = check_result_c(x_in, x_exp, lda);
#endif

    if(ACCURACY_PRINT)
    printf("\n ACCURACY = %g\n\n", maxerr);

    if(maxerr < eps){
        printf(" TEST PASSED\n");
    } else {
        printf(" TEST FAIL\n");
    }
    free(x_exp);
    free(x_in);
    free(x_out);

    return 0;
}
