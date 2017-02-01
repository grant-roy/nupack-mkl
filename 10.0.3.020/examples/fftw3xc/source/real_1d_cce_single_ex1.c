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
!   Real-to-complex and complex-to-real 1D transform for single precision data
!   inplace CCE packed format for complex conjugate-symmetric data
!
!   Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_REAL                 (obligatory)
!           DFTI_PRECISION      = DFTI_SINGLE               (obligatory)
!           DFTI_DIMENSION      = 1                         (obligatory)
!           DFTI_LENGTHS        = n                         (obligatory)
!           DFTI_PACKED_FORMAT  = DFTI_CCE_FORMAT           (default)
!           DFTI_PLACEMENT      = DFTI_INPLACE              (default)
!           DFTI_FORWARD_SCALE  = 1.0                       (default)
!           DFTI_BACKWARD_SCALE = 1.0/(float)n              (default=1.0)
!
!          DFTI_REAL_STORAGE   = DFTI_REAL_REAL            (default)
!          DFTI_CONJUGATE_EVEN_STORAGE = DFTI_COMPLEX_REAL (default)
!
!  Other default configuration parameters are in the mkl_dfti.h interface file
!******************************************************************************/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "fftw3.h"

#include "mkl_dfti_examples.h"

int main()  /* REAL_1D_CCE_SINGLE_EX1 */
{
    /*
    /   DFT input parameters
    */
    fftwf_plan Desc_Handle;
    float*  x_in;
    long    n=7;
    /* */
    float*  x_exp;
    long    nn;
    float   Scale;
    float   maxerr;
    float   eps= SINGLE_EPS;

    if(LEGEND_PRINT) {
        printf(" \n\n REAL_1D_CCE_SINGLE_EX1           \n");
        printf(" Forward-Backward 1D real transform for single precision data\n\n");
        printf(" Configuration parameters:              \n\n");
        printf(" DFTI_FORWARD_DOMAIN = DFTI_REAL        \n");
        printf(" DFTI_PRECISION      = DFTI_SINGLE      \n");
        printf(" DFTI_DIMENSION      = 1                \n");
        printf(" DFTI_LENGTHS        = %d               \n", n);
        printf(" DFTI_PACKED_FORMAT  = DFTI_CCE_FORMAT  \n");
        printf(" DFTI_PLACEMENT      = DFTI_INPLACE     \n");
        printf(" DFTI_FORWARD_SCALE  = 1.0              \n");
        printf(" DFTI_BACKWARD_SCALE = 1.0/(float)n     \n\n");
    }

    /*
    /   Allocate arrays for input and expected data
    */
    nn = 2*(n/2+1);
    x_in  = (float*)malloc(nn*sizeof(float));
    x_exp = (float*)malloc(n*sizeof(float));
    /*
    /   initialize x_in and copy to expected x_exp
    */
    zero_init_s(x_in, nn);
    init_real_vectors_s(x_in, n);
    cblas_scopy(n, x_in, 1, x_exp, 1);

    /*
    /   Create Dfti descriptor for 1D single precision forward transform
    */
    Desc_Handle = fftwf_plan_dft_r2c_1d( n, x_in, (fftwf_complex*)x_in, FFTW_ESTIMATE);
    /*
    /   Compute DFT
    */
    fftwf_execute(Desc_Handle);
    /*
    /   Destroy Dfti descriptor
    */
    fftwf_destroy_plan(Desc_Handle);

    /*
    /   Set Scale number for Backward transform
    */
    Scale = 1.0/(float)n;
    /*
    /   Create Dfti descriptor for 1D single precision backward transform
    */
    Desc_Handle = fftwf_plan_dft_c2r_1d( n, (fftwf_complex*)x_in, x_in, FFTW_ESTIMATE);
    /*
    /   Compute DFT
    */
    fftwf_execute(Desc_Handle);
    /*
    /   Destroy Dfti descriptor
    */
    fftwf_destroy_plan(Desc_Handle);
    /*
    /   Result scaling
    */
    scaling_fr(x_in, Scale, n);

    /*
    /   Check result
    */
    maxerr = check_result_s(x_in, x_exp, n);
    if(ACCURACY_PRINT)
        printf("\n ACCURACY = %g\n\n", maxerr);

    if(maxerr < eps){
        printf(" TEST PASSED\n");
    } else {
        printf(" TEST FAIL\n");
    }

    /*
    /   Free arrays for input and expected data
    */

    free(x_exp);
    free(x_in);

    return 0;
}
