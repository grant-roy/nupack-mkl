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
!       MKL DFTI interface example program (C-interface)
!
!       Forward-Backward 2D complex transform for single precision data not inplace.
!
!*******************************************************************************
!  Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX          (obligatory)
!           DFTI_PRECISION      = DFTI_SINGLE           (obligatory)
!           DFTI_DIMENSION      = 2                     (obligatory)
!           DFTI_LENGTHS        = { m, n}               (obligatory)
!           DFTI_PLACEMENT      = DFTI_NOT_INPLACE      (default= DFTI_INPLACE)
!           DFTI_INPUT_STRIDES  = {0, N_MAX, 1}         (default = {0,n,1})
!           DFTI_OUTPUT_STRIDES = {0, N_MAX, 1}         (default = {0,n,1})
!           DFTI_FORWARD_SCALE  = 1.0                   (default)
!           DFTI_BACKWARD_SCALE = 1.0/(float)(m*n)      (default=1.0)
!
! Other default configuration parameters are in the mkl_dfti.h interface file
!*******************************************************************************
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "mkl_dfti.h"

#include "mkl_dfti_examples.h"

int main(int argc, char *argv[])  /* COMPLEX_2D_SINGLE_EX2 */
{
    mkl_float_complex  x_in [M_MAX][N_MAX];
    mkl_float_complex  x_out[M_MAX][N_MAX];
    mkl_float_complex  x_exp[M_MAX][N_MAX];

    DFTI_DESCRIPTOR_HANDLE Desc_Handle = 0;

    MKL_LONG m;
    MKL_LONG n;

    MKL_LONG Status;
    float   Scale;
    MKL_LONG lengths[2];
    MKL_LONG strides_in[3];
    MKL_LONG strides_out[3];

    float   maxerr;
    float   eps= SINGLE_EPS;

    /*
    /   Read input data from input file
    /   m - size of transform  along first dimension
    /   n - size of transform  along second dimension
    */
    if(read_data_file_2d(argc, argv, &m, &n)) return 1;

    /*
    /  Put transform parameters
    */
    lengths[0] = m;
    lengths[1] = n;

    strides_in[0]= 0;
    strides_in[1]= N_MAX;
    strides_in[2]= 1;

    strides_out[0]= 0;
    strides_out[1]= N_MAX;
    strides_out[2]= 1;

    if(LEGEND_PRINT) {
        printf(" \n\n COMPLEX_2D_SINGLE_EX2 \n");
        printf(" Forward-Backward 2D complex transform for single precision data\n\n");
        printf(" Configuration parameters:                  \n\n");
        printf(" DFTI_FORWARD_DOMAIN = DFTI_COMPLEX         \n");
        printf(" DFTI_PRECISION      = DFTI_SINGLE          \n");
        printf(" DFTI_DIMENSION      = 2                    \n");
        printf(" DFTI_LENGTHS        = {%d,%d)              \n", m, n);
        printf(" DFTI_PLACEMENT      = DFTI_NOT_INPLACE     \n");
        printf(" DFTI_INPUT_STRIDES  = {0, %d, 1}           \n", N_MAX);
        printf(" DFTI_OUTPUT_STRIDES = {0, %d, 1}           \n", N_MAX);
        printf(" DFTI_FORWARD_SCALE  = 1.0                  \n");
        printf(" DFTI_BACKWARD_SCALE = 1.0/(float)(m*n)     \n\n");
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
    zero_init_c(x_in,  M_MAX*N_MAX);
    zero_init_c(x_out, M_MAX*N_MAX);

    init_multiple_columns_c(x_in, m, n, 0, N_MAX);

    cblas_ccopy(M_MAX*N_MAX, x_in, 1, x_exp, 1);

    if(ADVANCED_DATA_PRINT){
        printf("\n INPUT X, 3 columns \n\n");
        print_three_columns_c(x_in, m, 0, N_MAX);
    }

    /*
    /   Create Dfti descriptor for 1D single precision  transform
    */
    Status = DftiCreateDescriptor( &Desc_Handle, DFTI_SINGLE,
                                    DFTI_COMPLEX, 2, lengths);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n");
        return 0;
    }

    /*
    /   Set placement of result DFTI_NOT_INPLACE
    */
    Status = DftiSetValue( Desc_Handle, DFTI_PLACEMENT, DFTI_NOT_INPLACE);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n");  goto FREE_DESCRIPTOR;
    }

    /*
    /    In case of data allocation in the 2D array[M_MAX][N_MAX](C interface):
    /    srides[1] = N_MAX is not default parameter if n is not equal to N_MAX
    */

    if(n != N_MAX){
        Status = DftiSetValue(Desc_Handle, DFTI_INPUT_STRIDES, strides_in);
        if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
            dfti_example_status_print(Status);
            printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
        }

        Status = DftiSetValue(Desc_Handle, DFTI_OUTPUT_STRIDES, strides_out);
        if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
            dfti_example_status_print(Status);
            printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
        }
    }/* (n != N_MAX) */

    /*
    /   Commit Dfti descriptor
    */
    Status = DftiCommitDescriptor( Desc_Handle );
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    /*
    /   Compute Forward transform
    */
    printf("\n Compute DftiComputeForward\n");
    Status = DftiComputeForward( Desc_Handle, x_in, x_out);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    if(ADVANCED_DATA_PRINT){
        printf("\n Forward result X_OUT, 3 columns \n\n");
        print_three_columns_c(x_out, m, 0, N_MAX);

    }
    /*
    /   Set Scale number for Backward transform
    */
    Scale = 1.0/(float)(m*n);
    printf(" \n\n DFTI_BACKWARD_SCALE  = 1/(m*n) \n");

    Status = DftiSetValue(Desc_Handle, DFTI_BACKWARD_SCALE, Scale);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    /*
    /   Commit Dfti descriptor
    */
    Status = DftiCommitDescriptor( Desc_Handle );
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    /*
    /   Compute Backward transform
    */
    printf("\n Compute DftiComputeBackward\n");
    Status = DftiComputeBackward( Desc_Handle, x_out, x_in);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    if(ADVANCED_DATA_PRINT){
        printf("\n Backward result X_IN, 3 columns \n\n");
        print_three_columns_c(x_in, m, 0, N_MAX);
    }

    /*
    /   Check result
    */
    maxerr = check_result_c(x_in, x_exp, M_MAX*N_MAX);
    if(ACCURACY_PRINT)
    printf("\n ACCURACY = %g\n\n", maxerr);

    if(maxerr < eps){
        printf(" TEST PASSED\n");
    } else {
        printf(" TEST FAIL\n");
    }

    /*
    /   Free Dfti descriptor
    */
FREE_DESCRIPTOR:
    Status = DftiFreeDescriptor(&Desc_Handle);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n");
    }
END_OF_TEST:
    return 0;
}


