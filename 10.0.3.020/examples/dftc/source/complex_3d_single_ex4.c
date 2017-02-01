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
!       Forward-Backward 3D complex transform for single precision data not inplace.
!
!*******************************************************************************
!  Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX          (obligatory)
!           DFTI_PRECISION      = DFTI_SINGLE           (obligatory)
!           DFTI_DIMENSION      = 3                     (obligatory)
!           DFTI_LENGTHS        = { m, n, k}            (obligatory)
!           DFTI_PLACEMENT      = DFTI_NOT_INPLACE      (default= DFTI_INPLACE)
!           DFTI_INPUT_STRIDES  = {first_index_in, step_in_m, step_in_n, step_in_k}
!           DFTI_OUTPUT_STRIDES = {first_index_out, step_out_m, step_out_n, step_out_k}
!                                                       (default = {0,n*k,k,1})
!           DFTI_FORWARD_SCALE  = 1.0                   (default)
!           DFTI_BACKWARD_SCALE = 1.0/(float)(m*n*k)    (default=1.0)
!           DFTI_NUMBER_OF_TRANSFORMS = 1               (default)
!
! Other default configuration parameters are in the mkl_dfti.h interface file
!*******************************************************************************
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "mkl_dfti.h"

#include "mkl_dfti_examples.h"

int main(int argc, char *argv[])  /* COMPLEX_3D_SINGLE_EX4 */
{
    mkl_float_complex  x_in [M_MAX][N_MAX][K_MAX];
    mkl_float_complex  x_out[M_MAX][N_MAX][K_MAX];
    mkl_float_complex  x_exp[M_MAX][N_MAX][K_MAX];

    DFTI_DESCRIPTOR_HANDLE Desc_Handle = 0;

    MKL_LONG m;
    MKL_LONG n;
    MKL_LONG k;
    MKL_LONG index_strides_in[4];
    MKL_LONG index_strides_out[4];

    MKL_LONG Status;
    float   Scale;
    MKL_LONG lengths[3];
    MKL_LONG strides_in[4];
    MKL_LONG strides_out[4];

    float   maxerr;
    float   eps= SINGLE_EPS;
    MKL_LONG i;

    MKL_LONG first_index_in;
    MKL_LONG step_in_m;
    MKL_LONG step_in_n;
    MKL_LONG step_in_k;

    MKL_LONG first_index_out;
    MKL_LONG step_out_m;
    MKL_LONG step_out_n;
    MKL_LONG step_out_k;

    /*
    /   Read input data from input file
    /   m - size of transform  along first dimension
    /   n - size of transform  along second dimension
    /   k - size of transform  along third dimension
    /   index_strides_in[0]  - displacement from the first element of input array
    /   index_strides_in[1]  - index step along first dimension of input array
    /   index_strides_in[2]  - index step along second dimension of input array
    /   index_strides_in[3]  - index step along third dimension of input array
    /   index_strides_out[0] - displacement from the first element of output array
    /   index_strides_out[1] - index step along first dimension of output array
    /   index_strides_out[2] - index step along second dimension of output array
    /   index_strides_out[3] - index step along third dimension of output array
    */
    if(read_data_file_3d_strides_out(argc, argv, &m, &n, &k,
                      index_strides_in, index_strides_out)) return 1;

    /*
    /  Put transform parameters
    */
    lengths[0] = m;
    lengths[1] = n;
    lengths[2] = k;

    first_index_in  = index_strides_in[0];
    step_in_m       = index_strides_in[1];
    step_in_n       = index_strides_in[2];
    step_in_k       = index_strides_in[3];

    strides_in[0]   = first_index_in;
    strides_in[1]   = step_in_m * N_MAX*K_MAX;
    strides_in[2]   = step_in_n * K_MAX;
    strides_in[3]   = step_in_k * 1;

    first_index_out = index_strides_out[0];
    step_out_m      = index_strides_out[1];
    step_out_n      = index_strides_out[2];
    step_out_k      = index_strides_out[3];

    strides_out[0]  = first_index_out;
    strides_out[1]  = step_out_m * N_MAX*K_MAX;
    strides_out[2]  = step_out_n * K_MAX;
    strides_out[3]  = step_out_k * 1;

    if(LEGEND_PRINT) {
        printf(" \n\n COMPLEX_3D_SINGLE_EX4  \n");
        printf(" Forward-Backward 3D complex transform for single precision data\n\n");
        printf(" Configuration parameters:                  \n\n");
        printf(" DFTI_FORWARD_DOMAIN = DFTI_COMPLEX         \n");
        printf(" DFTI_PRECISION      = DFTI_SINGLE          \n");
        printf(" DFTI_DIMENSION      = 3                    \n");
        printf(" DFTI_LENGTHS        = {%d,%d,%d)           \n", m, n, k);
        printf(" DFTI_PLACEMENT      = DFTI_NOT_INPLACE     \n");
        printf(" DFTI_INPUT_STRIDES  = {%d, %d, %d, %d}     \n",
                strides_in[0], strides_in[1], strides_in[2], strides_in[3]);
        printf(" DFTI_OUTPUT_STRIDES  = {%d, %d, %d, %d}    \n",
                strides_out[0], strides_out[1], strides_out[2], strides_out[3]);
        printf(" DFTI_FORWARD_SCALE  = 1.0                  \n");
        printf(" DFTI_BACKWARD_SCALE = 1.0/(float)(m*n*k)   \n\n");
    }

    /*
    /   Check test input parameters
    */
    if( (first_index_in  + step_in_m * m  > M_MAX) ||
        (first_index_in  + step_in_n * n  > N_MAX) ||
        (first_index_in  + step_in_k * k  > K_MAX) ||
        (first_index_out + step_out_m * m > M_MAX) ||
        (first_index_out + step_out_n * n > N_MAX) ||
        (first_index_out + step_out_k * k > K_MAX) )
    {
        printf(" Error input parameters\n");
        printf(" TEST FAIL\n");  goto END_OF_TEST;
    }

    /*
    /   put input data and expected result
    */
    zero_init_c(x_in,  M_MAX*N_MAX*K_MAX);
    zero_init_c(x_out, M_MAX*N_MAX*K_MAX);

    init_3d_columns_c(x_in, m, n, k, strides_in);

    cblas_ccopy(M_MAX*N_MAX*K_MAX, x_in, 1, x_exp, 1);

    if(ADVANCED_DATA_PRINT){
        for(i=0; i < k; i+=step_in_k){
            printf("\n INPUT X[0:%d,0:2, %d], 3 columns \n\n", m-1, i);
            print_three_2d_columns_c(&x_in[0][0][i], m, strides_in);
        }/* for i */
    }

    /*
    /   Create Dfti descriptor for 1D single precision  transform
    */
    Status = DftiCreateDescriptor( &Desc_Handle, DFTI_SINGLE,
                                    DFTI_COMPLEX, 3, lengths);
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
    /   Set strides parameters
    */
    Status = DftiSetValue(Desc_Handle, DFTI_INPUT_STRIDES, strides_in);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" DFTI_INPUT_STRIDES\n");
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    Status = DftiSetValue(Desc_Handle, DFTI_OUTPUT_STRIDES, strides_out);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" DFTI_INPUT_STRIDES\n");
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
    /   Compute Forward transform
    */
    printf("\n Compute DftiComputeForward\n");
    Status = DftiComputeForward( Desc_Handle, x_in, x_out);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    if(ADVANCED_DATA_PRINT){
        for(i=0; i < k; i+=step_in_k){
            printf("\n Forward result X_OUT[0:%d,0:2, %d], 3 columns \n\n", m-1, i);
            print_three_2d_columns_c(&x_out[0][0][i], m, strides_out);
        }/* for i */
    }

    /*
    /   Set Scale number for Backward transform
    */
    Scale = 1.0/(float)(m*n*k);
    printf(" \n\n DFTI_BACKWARD_SCALE  = 1/(m*n*k) \n");

    Status = DftiSetValue(Desc_Handle, DFTI_BACKWARD_SCALE, Scale);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    /*
    /    You should change DFTI_INPUT_STRIDES and DFTI_OUTPUT_STRIDES values
    /    if strides_in is not equal to strides_out
    */
    i=0;
    do{
        if(strides_in[i] != strides_out[i]){
            Status = DftiSetValue(Desc_Handle, DFTI_OUTPUT_STRIDES, strides_in);
            if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
                dfti_example_status_print(Status);
                printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
            }

            Status = DftiSetValue(Desc_Handle, DFTI_INPUT_STRIDES, strides_out);
            if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
                dfti_example_status_print(Status);
                printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
            }

            break;
        }/* if */
        i++;
    } while(i<4);

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
    printf("\n Compute DftiComputeBackward\n\n");
    Status = DftiComputeBackward( Desc_Handle, x_out, x_in);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    if(ADVANCED_DATA_PRINT){
        for(i=0; i < k; i+=step_in_k){
            printf("\n Backward result X[0:%d,0:2, %d], 3 columns \n\n", m-1, i);
            print_three_2d_columns_c(&x_in[0][0][i], m, strides_in);
        }/* for i */
    }

    /*
    /   Check result
    */
    maxerr = check_result_c(x_in, x_exp, M_MAX*N_MAX*K_MAX);
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


