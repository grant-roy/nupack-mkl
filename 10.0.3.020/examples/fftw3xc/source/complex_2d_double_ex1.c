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
!   Forward-Backward 2D complex transform for double precision data inplace.
!
!   Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX          (obligatory)
!           DFTI_PRECISION      = DFTI_DOUBLE           (obligatory)
!           DFTI_DIMENSION      = 2                     (obligatory)
!           DFTI_LENGTHS        = { m, n}               (obligatory)
!           DFTI_PLACEMENT      = DFTI_INPLACE          (default)
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
#include "fftw3.h"

#include "mkl_dfti_examples.h"

int main()  /* COMPLEX_2D_DOUBLE_EX1 */
{
    /*
    /   DFT input parameters
    */
    int rank = 2;
    long     m;
    fftw_plan Desc_Handle;
    fftw_complex x_in[M_MAX][N_MAX];
    	/* */
    fftw_complex x_exp[M_MAX][N_MAX];
    double   Scale;
    double   maxerr;
    double   eps = DOUBLE_EPS;
    int     n[2]={M_MAX, N_MAX};
    /*
    /  Put transform parameters
    */
    m = M_MAX;

    if(LEGEND_PRINT) {
        printf(" \n\n COMPLEX_2D_DOUBLE_EX1  \n");
        printf(" Forward-Backward 2D complex transform for double precision data\n\n");
        printf(" Configuration parameters:                  \n\n");
        printf(" DFTI_FORWARD_DOMAIN = DFTI_COMPLEX         \n");
        printf(" DFTI_PRECISION      = DFTI_DOUBLE          \n");
        printf(" DFTI_DIMENSION      = 2                    \n");
        printf(" DFTI_LENGTHS        = {%d,%d)              \n", m, n);
        printf(" DFTI_PLACEMENT      = DFTI_INPLACE         \n");
        printf(" DFTI_INPUT_STRIDES  = {0, %d, 1}           \n", N_MAX);
        printf(" DFTI_OUTPUT_STRIDES = {0, %d, 1}           \n", N_MAX);
        printf(" DFTI_FORWARD_SCALE  = 1.0                  \n");
        printf(" DFTI_BACKWARD_SCALE = 1.0/(double)(m*n)    \n\n");
    }

    /*
    /   Check test input parameters
    */
    if((n[0]*n[1]) > (M_MAX*N_MAX)) {
        printf(" Error input parameters: (m*n)>(M_MAX*N_MAX)\n");
        printf(" Please see mkl_dfti_examples.h file\n");
        printf(" TEST FAIL\n");  goto END_OF_TEST;
    }

    /*
    /   put input data and expected result
    */
    zero_init_z(x_in,  M_MAX*N_MAX);

    init_multiple_columns_z(x_in, n[0], n[1], 0, N_MAX);

    cblas_zcopy(M_MAX*N_MAX, x_in, 1, x_exp, 1);

    /*
    /   Create Dfti descriptor for 2D double precision forward transform
    */
    Desc_Handle = fftw_plan_dft(rank, n, &x_in, &x_in, FFTW_FORWARD, FFTW_ESTIMATE);
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
    Scale = 1.0/((double)(n[0]*n[1]));
    /*
    /   Create Dfti descriptor for 2D double precision backward transform
    */
    Desc_Handle = fftw_plan_dft(rank, n, &x_in, &x_in, FFTW_BACKWARD, FFTW_ESTIMATE);
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
    scaling_d(x_in, Scale, M_MAX*N_MAX);

    /*
    /   Check result
    */
    maxerr = check_result_z(x_in, x_exp, M_MAX*N_MAX);
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
