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
!       Complex-to-complex multiple 1D transform for single precision data
!       which are allocated in two-dimension array, not inplace.
!
!       Perform multiple orow ne-dimensional transform
!       Note that row is along leading dimension and data are unit-stride
!
!*******************************************************************************
!  Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX          (obligatory)
!           DFTI_PRECISION      = DFTI_SINGLE           (obligatory)
!           DFTI_DIMENSION      = 1                     (obligatory)
!           DFTI_LENGTHS        = n                     (obligatory)
!           DFTI_PLACEMENT      = DFTI_NOT_INPLACE      (default= DFTI_INPLACE)
!           DFTI_FORWARD_SCALE  = 1.0                   (default)
!           DFTI_BACKWARD_SCALE = 1.0/(float)n          (default=1.0)
!
!           DFTI_INPUT_STRIDES  = {first_index, step_in}    (default={0,1})
!           DFTI_OUTPUT_STRIDES = {first_index, step_out}   (default={0,1})
!           DFTI_NUMBER_OF_TRANSFORMS = multiple            (default = 1)
!           DFTI_INPUT_DISTANCE       = dist_in   (obligatory, if multiple >1)
!           DFTI_OUTPUT_DISTANCE      = dist_out  (obligatory, if multiple >1)
! Other default configuration parameters are in the mkl_dfti.h interface file
!*******************************************************************************
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "mkl_dfti.h"
#include "mkl_dfti_examples.h"

int main(int argc, char *argv[])  /* COMPLEX_1D_SINGLE_EX4 */
{
    mkl_float_complex  x_in [M_MAX][N_MAX];
    mkl_float_complex  x_out[M_MAX][N_MAX];
    mkl_float_complex  x_exp[M_MAX][N_MAX];

    DFTI_DESCRIPTOR_HANDLE Desc_Handle = 0;

    MKL_LONG n;
    MKL_LONG first_index;
    MKL_LONG multiple;

    MKL_LONG Status;
    float   Scale;

    MKL_LONG dist_in;
    MKL_LONG dist_out;

    MKL_LONG strides_in[2];
    MKL_LONG strides_out[2];

    float   maxerr;
    float   eps= SINGLE_EPS;
    int     i;

    /*
    /   Read input data from input file
    /   n - size of transform
    /   first_index - displacement from the first element of data array
    /   multiple - number of multiple transform
    */
    if(read_data_file_1d_mult(argc, argv, &n, &first_index, &multiple)) return 1;

    /*
    /  Put transform parameters
    */
    dist_in  = N_MAX;
    dist_out = N_MAX;

    strides_in[0] = first_index;
    strides_in[1] = 1;

    strides_out[0] = first_index;
    strides_out[1] = 1;

    if(LEGEND_PRINT) {
        printf(" \n\n COMPLEX_1D_SINGLE_EX4  \n");
        printf(" Forward-Backward multiple row 1D complex transform                     \n");
        printf(" Note that row is along leading dimension and data are unit-stride  \n\n");
        printf(" Configuration parameters:                                          \n\n");
        printf(" DFTI_FORWARD_DOMAIN = DFTI_COMPLEX     \n");
        printf(" DFTI_PRECISION      = DFTI_SINGLE      \n");
        printf(" DFTI_DIMENSION      = 1                \n");
        printf(" DFTI_LENGTHS        = %d               \n", n);
        printf(" DFTI_PLACEMENT      = DFTI_NOT_INPLACE \n");
        printf(" DFTI_FORWARD_SCALE  = 1.0              \n");
        printf(" DFTI_BACKWARD_SCALE = 1.0/(float)n     \n\n");
        printf(" DFTI_NUMBER_OF_TRANSFORMS = %d         \n", multiple);
        printf(" DFTI_INPUT_DISTANCE       = %d         \n", dist_in);
        printf(" DFTI_INPUT_STRIDES        ={%d, %d}    \n", strides_in[0], strides_in[1]);
        printf(" DFTI_OUTPUT_DISTANCE      = %d         \n", dist_out);
        printf(" DFTI_OUTPUT_STRIDES       ={%d, %d}    \n", strides_out[0], strides_out[1]);
    }

    /*
    /   Check input parameters
    */
    if(first_index + multiple*n > M_MAX*N_MAX){
        printf(" Error input parameters: first_index + multiple*n > M_MAX*N_MAX \n");
        printf(" Please see mkl_dfti_examples.h file\n");
        printf(" TEST FAIL\n");
        return 0;
    }

    /*
    /   put input data and expected result
    */
    zero_init_c(x_in, M_MAX*N_MAX);
    zero_init_c(x_out, M_MAX*N_MAX);

    init_multiple_rows_c(x_in, n, multiple, first_index, dist_in);

    cblas_ccopy(M_MAX*N_MAX, x_in, 1, x_exp, 1);

    if(ADVANCED_DATA_PRINT){
        printf("\n INPUT X by columns (for multiple=3)\n\n");
        print_three_rows_c(x_in, n, first_index, dist_in);
    }

    /*
    /   Create Dfti descriptor for 1D single precision  transform
    */
    Status = DftiCreateDescriptor( &Desc_Handle, DFTI_SINGLE,
                                    DFTI_COMPLEX, 1, n );
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

        Status = DftiSetValue( Desc_Handle, DFTI_OUTPUT_DISTANCE, dist_out);
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

    Status = DftiSetValue(Desc_Handle, DFTI_OUTPUT_STRIDES, strides_out);
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
    Status = DftiComputeForward( Desc_Handle, x_in, x_out);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    if(ADVANCED_DATA_PRINT){
        printf("\n Fortward result X_OUT by columns (for multiple=3)\n\n");
        print_three_rows_c(x_in, n, first_index, dist_in);
    }
    /*
    /   Set Scale number for Backward transform
    */
    Scale = 1.0/(float)n;
    printf(" \n\n DFTI_BACKWARD_SCALE  = 1/n \n");

    Status = DftiSetValue(Desc_Handle, DFTI_BACKWARD_SCALE, Scale);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    /*
    /    You should change DFTI_INPUT_DISTANCE and DFTI_OUTPUT_DISTANCE values
    /       if dist_in is not equal to dist_out
    */

    if(dist_in != dist_out) {
        Status = DftiSetValue( Desc_Handle, DFTI_INPUT_DISTANCE, dist_out);
        if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
            dfti_example_status_print(Status);
            printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
        }
        Status = DftiSetValue( Desc_Handle, DFTI_OUTPUT_DISTANCE, dist_in);
        if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
            dfti_example_status_print(Status);
            printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
        }
    }/* if(dist_in != dist_out) */

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
    } while(i<2);

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
        printf(" Backward result X by columns (for multiple=3)\n\n");
        print_three_rows_c(x_in, n, first_index, dist_in);
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
    return 0;
}

