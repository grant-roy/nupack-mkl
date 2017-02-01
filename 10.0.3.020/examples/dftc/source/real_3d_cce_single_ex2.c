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
!       Forward-Backward 3D real transform for single precision data not inplace.
!
!*******************************************************************************
!  Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_REAL                     (obligatory)
!           DFTI_PRECISION      = DFTI_SINGLE                   (obligatory)
!           DFTI_DIMENSION      = 3                             (obligatory)
!           DFTI_LENGTHS        = { m, n, k}                    (obligatory)
!           DFTI_PLACEMENT      = DFTI_NOT_INPLACE              (default= DFTI_INPLACE)
!           DFTI_INPUT_STRIDES  = {0, K_MAX*N_MAX, K_MAX, 1}    (default = {0,n*k,k,1})
!           DFTI_OUTPUT_STRIDES = {0, (K_MAX/2+1)*N_MAX, K_MAX/2+1, 1}
!                                                               (default = {0,n*k,k,1})
!           DFTI_FORWARD_SCALE  = 1.0                           (default)
!           DFTI_BACKWARD_SCALE = 1.0/(float)(m*n*k)            (default=1.0)
!           DFTI_CONJUGATE_EVEN_STORAGE = DFTI_COMPLEX_COMPLEX (default=DFTI_COMPLEX_REAL)
!
! Other default configuration parameters are in the mkl_dfti.h interface file
!*******************************************************************************
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "mkl_dfti.h"

#include "mkl_dfti_examples.h"

int main(int argc, char *argv[])  /* REAL_3D_CCE_SINGLE_EX2 */
{
    float  x_in [M_MAX][N_MAX][K_MAX];
    mkl_float_complex  x_out[M_MAX][N_MAX][K_MAX/2+1];
    float  x_exp[M_MAX][N_MAX][K_MAX];

    DFTI_DESCRIPTOR_HANDLE Desc_Handle = 0;

    MKL_LONG  m;
    MKL_LONG  n;
    MKL_LONG  k;

    MKL_LONG  Status;
    float    Scale;
    MKL_LONG  lengths[3];
    MKL_LONG  strides_in[4];
    MKL_LONG  strides_out[4];

    float   maxerr;
    float   eps= SINGLE_EPS;
    MKL_LONG i;

    /*
    /   Read input data from input file
    /   m - size of transform  along first dimension
    /   n - size of transform  along second dimension
    /   k - size of transform  along third dimension
    */
    if(read_data_file_3d(argc, argv, &m, &n, &k)) return 1;

    /*
    /  Put transform parameters
    */
    m = M_MAX;
    n = N_MAX;
    k = K_MAX;


    lengths[0] = m;
    lengths[1] = n;
    lengths[2] = k;

    /*
    /    In case of data allocation in the 3D array[M_MAX][N_MAX][K_MAX](C-interface)
    /    parameter STRIDES may be not default
    */

    strides_in[0] = 0;
    strides_in[1] = K_MAX*N_MAX;
    strides_in[2] = K_MAX;
    strides_in[3] = 1;

    strides_out[0] = 0;
    strides_out[1] = (K_MAX/2+1)*N_MAX;
    strides_out[2] = K_MAX/2+1;
    strides_out[3] = 1;

    /*
    /   Check test input parameters
    */
    if((m*n*k) > (M_MAX* N_MAX*K_MAX)) {
        printf(" Error input parameters: (m*n*k)>(M_MAX*N_MAX*K_MAX)\n");
        printf(" Please see mkl_dfti_examples.h file\n");
        printf(" TEST FAIL\n");  goto END_OF_TEST;
    }

    if(LEGEND_PRINT) {
        printf(" \n\n REAL_3D_CCE_SINGLE_EX2  \n");
        printf(" Forward-Backward 3D real transform for single precision data\n\n");
        printf(" Configuration parameters:                  \n\n");
        printf(" DFTI_FORWARD_DOMAIN = DFTI_REAL            \n");
        printf(" DFTI_PRECISION      = DFTI_SINGLE          \n");
        printf(" DFTI_DIMENSION      = 3                    \n");
        printf(" DFTI_LENGTHS        = {%d,%d,%d)           \n", m, n, k);
        printf(" DFTI_PLACEMENT      = DFTI_NOT_INPLACE     \n");
        printf(" DFTI_INPUT_STRIDES  = {0, %d, %d, 1}       \n", N_MAX*K_MAX, K_MAX);
        printf(" DFTI_OUTPUT_STRIDES = {0, %d, %d, 1}       \n", N_MAX*(K_MAX/2+1), K_MAX/2+1);
        printf(" DFTI_FORWARD_SCALE  = 1.0                  \n");
        printf(" DFTI_BACKWARD_SCALE = 1.0/(float)(m*n*k)   \n\n");
    }

    /*
    /   put input data and expected result
    */
    zero_init_s(x_in,  M_MAX*N_MAX*K_MAX);
    zero_init_c(x_out, M_MAX*N_MAX*(K_MAX/2+1));

    init_3d_columns_s(x_in, m, n, k, strides_in);

    cblas_scopy(M_MAX*N_MAX*K_MAX, x_in, 1, x_exp, 1);

    if(ADVANCED_DATA_PRINT){
        for(i=0; i < k; i++){
            printf("\n INPUT X[0:%d,0:2, %d], 3 columns \n\n", m-1, i);
            print_three_2d_columns_s(&x_in[0][0][i], m, n, strides_in);
        }/* for i */
    }

    /*
    /   Create Dfti descriptor for 1D single precision  transform
    */
    Status = DftiCreateDescriptor( &Desc_Handle, DFTI_SINGLE,
                                    DFTI_REAL, 3, lengths);
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
    /  Set storage for output complex conjugate-symmetric data
    */
      Status = DftiSetValue(Desc_Handle, DFTI_CONJUGATE_EVEN_STORAGE, DFTI_COMPLEX_COMPLEX);
      if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
          dfti_example_status_print(Status);
          printf(" DftiSetValue TEST FAIL\n");  goto FREE_DESCRIPTOR;
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
        for(i=0; i < k; i++){
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
        for(i=0; i < k; i++){
            printf("\n Backward result X_IN[0:%d,0:2, %d], 3 columns \n\n", m-1, i);
            print_three_2d_columns_s(&x_in[0][0][i], m, n, strides_in);
        }/* for i */
    }

    /*
    /   Check result
    */
    maxerr = check_result_s(x_in, x_exp, M_MAX*N_MAX*K_MAX);
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
