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
!   Complex-to-complex 1D transform for double/float precision data inplace.
!
!   Configuration parameters for MKL DFTI:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX   (obligatory)
!           DFTI_PRECISION      = DFTI_DOUBLE/DFTI_SINGLE    (obligatory)
!           DFTI_DIMENSION      = 1              (obligatory)
!           DFTI_LENGTHS        = n              (obligatory)
!           DFTI_PLACEMENT      = DFTI_INPLACE   (default)
!           DFTI_FORWARD_SCALE  = 1.0            (default)
!           DFTI_BACKWARD_SCALE = 1.0/(double/float)n  (default=1.0)
!
!  Other default configuration parameters are in the mkl_dfti.h interface file
!******************************************************************************/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "fftw.h"

#include "mkl_dfti_examples.h"

int main()  /* COMPLEX_1D_EX5 */
{
    /*
    /   DFT input parameters
    */
    int  n=4;
	int  multiple = 2;
	int istride = 2;
	int idist = 8;
	int lda;
    fftw_plan Desc_Handle;
    fftw_complex* x_in;
	fftw_direction dir;
    /* */
    fftw_complex* x_exp;
    TYPE_PRECISION  Scale;
    TYPE_PRECISION  maxerr;
    TYPE_PRECISION  eps= EPS;

    if(LEGEND_PRINT) {
        printf(" \n\n COMPLEX_1D_EX5 \n");
        printf(" Forward-Backward 1D complex transform for double/float precision data\n\n");
        printf(" Configuration parameters:            \n\n");
        printf(" DFTI_FORWARD_DOMAIN = DFTI_COMPLEX   \n");
        printf(" DFTI_PRECISION      = DFTI_DOUBLE/DFTI_SINGLE    \n");
        printf(" DFTI_DIMENSION      = 1              \n");
        printf(" DFTI_LENGTHS        = %d             \n", n);
        printf(" DFTI_PLACEMENT      = DFTI_INPLACE   \n");
        printf(" DFTI_FORWARD_SCALE  = 1.0            \n");
        printf(" DFTI_BACKWARD_SCALE = 1.0/(double/float)n  \n\n");
    }

    /*
    /   Allocate array for input data
    */

	lda = n*istride*multiple;
	if (idist > n*istride) lda = idist*multiple;

    x_in = (fftw_complex*)malloc(2*lda*sizeof(TYPE_PRECISION));
    x_exp = (fftw_complex*)malloc(2*lda*sizeof(TYPE_PRECISION));
    /*
    /   initialize x_in and copy to expected x_exp
    */
    /*
    /   Create Dfti descriptor for 1D double precision forward transform
    */
    Desc_Handle = fftw_create_plan_specific( n, FFTW_FORWARD, FFTW_ESTIMATE | FFTW_IN_PLACE,
		                                     x_in, istride, x_in, istride);

#ifdef MKL_DOUBLE
    init_input_and_expected_vectors_z(x_in, x_exp, lda);
#else
    init_input_and_expected_vectors_c(x_in, x_exp, lda);
#endif
    /*
    /   Compute DFT
    */
    fftw(Desc_Handle, multiple, x_in, istride, idist, x_in, istride, idist);

/*	{
		int i;
		for (i=0; i<lda; i++) {
            printf(" forward: x_in[%d].re = %f, x_in[%d].im = %f\n", i, x_in[i].re, i, x_in[i].im);
		}
	} */

	/*
    /   Destroy Dfti descriptor
    */
    fftw_destroy_plan(Desc_Handle);

    /*
    /   Set Scale number for Backward transform
    */
    Scale = 1.0/(TYPE_PRECISION)n;
    /*
    /   Create Dfti descriptor for 1D double precision backward transform
    */
    Desc_Handle = fftw_create_plan_specific( n, FFTW_BACKWARD, FFTW_ESTIMATE | FFTW_IN_PLACE,
		                                     x_in, istride, x_in, istride);
    /*
    /   Compute DFT
    */
    fftw(Desc_Handle, multiple, x_in, istride, idist, NULL, 0, 0);

/*	{
		int i;
		for (i=0; i<lda; i++) {
            printf(" backward: x_in[%d].re = %f, x_in[%d].im = %f\n", i, x_in[i].re, i, x_in[i].im);
		}
	} */
    /*
    /   Destroy Dfti descriptor
    */
    fftw_destroy_plan(Desc_Handle);
    /*
    /   Result scaling
    */
    scaling_multiple(x_in, Scale, n, multiple, istride, idist);

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

    return 0;
}
