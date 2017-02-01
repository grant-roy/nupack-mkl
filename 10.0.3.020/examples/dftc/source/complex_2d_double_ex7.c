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
!       Complex-to-complex multiple 2D transform for double precision data
!       which are allocated in three-dimension array
!
!       Perform multiple two-dimensional transform along the 3-d dimension of 3D array
!       Note that data are not unit-stride
!
!*******************************************************************************
!  Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX      (obligatory)
!           DFTI_PRECISION      = DFTI_DOUBLE       (obligatory)
!           DFTI_DIMENSION      = 2                 (obligatory)
!           DFTI_LENGTHS        = { m, n}           (obligatory)
!           DFTI_PLACEMENT      = DFTI_INPLACE      (default)
!
!           DFTI_FORWARD_SCALE  = 1.0               (default)
!           DFTI_BACKWARD_SCALE = 1.0/(double)(m*n) (default=1.0)
!
!           DFTI_INPUT_STRIDES  = {first_index, stride_in_m, stride_in_n}   (default={0, n, 1})
!
!           DFTI_NUMBER_OF_TRANSFORMS   = multiple                      (default = 1)
!           DFTI_INPUT_DISTANCE         = dist_in    (obligatory, if NUMBER_OF_TRANSFORMS >1)
!
! Other default configuration parameters are in the mkl_dfti.h interface file
!*******************************************************************************
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "mkl_dfti.h"

#include "mkl_dfti_examples.h"

int main(int argc, char *argv[])  /* COMPLEX_2D_DOUBLE_EX7 */
{
    mkl_double_complex  x_in [M_MAX][N_MAX][K_MAX];
    mkl_double_complex  x_exp[M_MAX][N_MAX][K_MAX];

    DFTI_DESCRIPTOR_HANDLE Desc_Handle = 0;

    MKL_LONG m;
    MKL_LONG n;
    MKL_LONG first_index;
    MKL_LONG multiple;

    MKL_LONG Status;
    double  Scale;
    MKL_LONG lengths[2];
    MKL_LONG strides_in[3];
    MKL_LONG dist_in;

    double   maxerr;
    double   eps= SINGLE_EPS;

    MKL_LONG  i;

    /*
    /   Read input data from input file
    /   m - size of transform  along first dimension
    /   n - size of transform  along second dimension
    /   first_index - displacement from the first element of data array
    /   multiple - number of multiple transform
    */
    if(read_data_file_2d_mult(argc, argv, &m, &n, &first_index, &multiple)) return 1;

    /*
    /  Put transform parameters
    */
    lengths[0] = m;
    lengths[1] = n;

    dist_in = 1;

    strides_in[0] = first_index;
    strides_in[1] = N_MAX * K_MAX;
    strides_in[2] = K_MAX;

    if(LEGEND_PRINT) {
        printf(" \n\n  COMPLEX_2D_DOUBLE_EX7 \n");
        printf(" Forward-Backward 2D complex transform for double precision data        \n\n");
        printf(" Multiple 2D transform along 3-d dimension of 3D array                  \n");
        printf(" Configuration parameters:                      \n\n");
        printf(" DFTI_FORWARD_DOMAIN        = DFTI_COMPLEX      \n");
        printf(" DFTI_PRECISION             = DFTI_DOUBLE       \n");
        printf(" DFTI_DIMENSION             = 2                 \n");
        printf(" DFTI_LENGTHS               = {%d,%d)           \n", m, n);
        printf(" DFTI_PLACEMENT             = DFTI_INPLACE      \n");
        printf(" DFTI_FORWARD_SCALE         = 1.0               \n");
        printf(" DFTI_BACKWARD_SCALE        = 1.0/(double)(m*n)  \n");
        printf(" DFTI_INPUT_STRIDES         = {%d, %d, %d}      \n", first_index,
                                                strides_in[1], strides_in[2]);
        printf(" DFTI_NUMBER_OF_TRANSFORMS  = %d                \n", multiple);
        if(multiple > 1)
        printf(" DFTI_INPUT_DISTANCE        = %d                \n", dist_in);

    }

    /*
    /   Check test input parameters
    /   for multiple 2D transform along the 3-d dimension of 3D array
    */
    if(first_index + multiple*(m*n) > M_MAX*N_MAX*K_MAX){
        printf(" Error input parameters\n");
        printf(" TEST FAIL\n");  goto END_OF_TEST;
    }

    /*
    /   put input data and expected result
    */
    zero_init_z(x_in, M_MAX*N_MAX*K_MAX);

    for(i=0; i< multiple; i++){
        init_multiple_2d_columns_z(&x_in[0][0][i], m, n, strides_in);
    }

    print_vector_z(x_in, M_MAX*N_MAX*K_MAX);

    cblas_zcopy(M_MAX*N_MAX*K_MAX, x_in, 1, x_exp, 1);

    if(ADVANCED_DATA_PRINT){
        printf("\n INPUT  X three 2D columns\n");
        for(i=0; i< multiple; i++) {
            printf("\n Transform Serial=%d \n", i);
            print_three_2d_columns_z(&x_in[0][0][i], m, strides_in);
        }
    }

    /*
    /   Create Dfti descriptor for 1D double precision  transform
    */
    Status = DftiCreateDescriptor( &Desc_Handle, DFTI_DOUBLE,
                                    DFTI_COMPLEX, 2, lengths);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n");
        return 0;
    }

    /*
    /   Set parameters for multiple transform mode
    */
    if(multiple > 1){
        Status = DftiSetValue( Desc_Handle, DFTI_NUMBER_OF_TRANSFORMS, multiple );
        if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
            dfti_example_status_print(Status);
            printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
        }

        Status = DftiSetValue( Desc_Handle, DFTI_INPUT_DISTANCE, dist_in);
        if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
            dfti_example_status_print(Status);
            printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
        }
    }

    /*
    /   Set strides parameters
    */

    Status = DftiSetValue(Desc_Handle, DFTI_INPUT_STRIDES, strides_in);
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
    /   Compute Forward transform
    */
    printf("\n Compute DftiComputeForward\n");
    Status = DftiComputeForward( Desc_Handle, x_in);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    if(ADVANCED_DATA_PRINT){
        printf("\n Forward OUTPUT  X three 2D columns\n");
        for(i=0; i< multiple; i++) {
            printf("\n Transform Serial=%d \n", i);
            print_three_2d_columns_z(&x_in[0][0][i], m, strides_in);
        }
    }

    /*
    /   Set Scale number for Backward transform
    */
    Scale = 1.0/(double)(m*n);
    printf(" \n DFTI_BACKWARD_SCALE  = 1/(m*n) \n");

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
    Status = DftiComputeBackward( Desc_Handle, x_in);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    if(ADVANCED_DATA_PRINT){
        printf("\n Backward OUTPUT  X three 2D columns\n");
        for(i=0; i< multiple; i++) {
            printf("\n Transform Serial=%d \n", i);
            print_three_2d_columns_z(&x_in[0][0][i], m, strides_in);
        }
    }

    /*
    /   Check result
    */
    maxerr = check_result_z(x_in, x_exp, M_MAX*N_MAX*K_MAX);
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


