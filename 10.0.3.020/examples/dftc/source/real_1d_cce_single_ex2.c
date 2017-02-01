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
!       Real-to-complex and complex-to-real 1D transform
!       for single precision data not inplace
!       CCE packed format for complex conjugate-symmetric data
!
!*******************************************************************************
!  Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_REAL                 (obligatory)
!           DFTI_PRECISION      = DFTI_SINGLE               (obligatory)
!           DFTI_DIMENSION      = 1                         (obligatory)
!           DFTI_LENGTHS        = n                         (obligatory)
!           DFTI_PACKED_FORMAT  = DFTI_CCE_FORMAT           (default)
!           DFTI_PLACEMENT      = DFTI_NOT_INPLACE          (default = DFTI_INPLACE)
!           DFTI_FORWARD_SCALE  = 1.0                       (default)
!           DFTI_BACKWARD_SCALE = 1.0/(float)n              (default=1.0)
!
!           DFTI_REAL_STORAGE   =  DFTI_REAL_REAL           (default)
!           DFTI_CONJUGATE_EVEN_STORAGE = DFTI_COMPLEX_REAL (default)
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

int main(int argc, char *argv[])  /* REAL_1D_CCE_SINGLE_EX2 */
{
    float*  x_in;
    mkl_float_complex*  x_out;
    float*  x_exp;
    float*  x_alloc;

    DFTI_DESCRIPTOR_HANDLE Desc_Handle = 0;
    MKL_LONG n;

    MKL_LONG Status;
    float   Scale;

    float   maxerr = SINGLE_EPS;
    float   eps = SINGLE_EPS;

    /*
    /   Read input data from input file
    /   n - size of transform
    */
    if(read_data_file_1d(argc, argv, &n)) return 1;

    if(LEGEND_PRINT) {
        printf(" \n\n REAL_1D_CCE_SINGLE_EX2                   \n");
        printf(" Forward-Backward 1D real transform for single precision data\n\n");
        printf(" Configuration parameters:                      \n\n");
        printf(" DFTI_FORWARD_DOMAIN = DFTI_REAL                \n");
        printf(" DFTI_PRECISION      = DFTI_SINGLE              \n");
        printf(" DFTI_DIMENSION      = 1                        \n");
        printf(" DFTI_LENGTHS        = %d                       \n", n);
        printf(" DFTI_PACKED_FORMAT  = DFTI_CCE_FORMAT          \n");
        printf(" DFTI_PLACEMENT      = DFTI_NOT_INPLACE         \n");
        printf(" DFTI_FORWARD_SCALE  = 1.0                      \n");
        printf(" DFTI_BACKWARD_SCALE = 1.0/(float)n             \n\n");
    }

    /*
    /   Allocate arrays for input and expected data
    */
    x_in  = (float*)malloc(n*sizeof(float));
    x_out = (float*)malloc((n/2+1)*2*sizeof(float));
    x_exp = (float*)malloc(n*sizeof(float));

    /*
    /   initialize x_in and copy to expected x_exp
    */

    zero_init_c(x_out, n/2+1);
    init_real_vectors_s(x_in, n);
    cblas_scopy(n, x_in, 1, x_exp, 1);

    if(ADVANCED_DATA_PRINT){
        printf(" INPUT vector X_IN \n");
        print_real_vector_s(x_in, n);
    }

    /*
    /   Create Dfti descriptor for 1D single precision  transform
    */
    Status = DftiCreateDescriptor( &Desc_Handle, DFTI_SINGLE,
                                    DFTI_REAL, 1, n );
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" DftiCreateDescriptor TEST FAIL\n");  goto FREE_ARRAYS;
    }

    /*
    /   Set placement of result DFTI_NOT_INPLACE
    */
    Status = DftiSetValue( Desc_Handle, DFTI_PLACEMENT, DFTI_NOT_INPLACE);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" DftiSetValue TEST FAIL\n");  goto FREE_DESCRIPTOR;
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
    /   Commit Dfti descriptor
    */
    Status = DftiCommitDescriptor( Desc_Handle );
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" DftiCommitDescriptor TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    /*
    /   Compute Forward transform
    */
    printf("\n Compute DftiComputeForward\n\n");
    Status = DftiComputeForward( Desc_Handle, x_in, x_out);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" DftiComputeForward TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    if(ADVANCED_DATA_PRINT){
        printf(" Forward result vector X_OUT \n");
        print_vector_c(x_out, n/2+1);
    }

    /*
    /   Set Scale number for Backward transform
    */
    Scale = 1.0/(float)n;
    printf("\n DFTI_BACKWARD_SCALE  = 1/n \n");

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
    printf("\n Compute DftiComputeBackward\n\n");

    Status = DftiComputeBackward( Desc_Handle, x_out, x_in);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n"); goto FREE_DESCRIPTOR;
    }

    if(ADVANCED_DATA_PRINT){
        printf(" Backward result vector X_IN \n");
        print_real_vector_s(x_in, n);
    }

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
    /   Free DFTI descriptor
    */
FREE_DESCRIPTOR:
    Status = DftiFreeDescriptor(&Desc_Handle);
    if(! DftiErrorClass(Status, DFTI_NO_ERROR)){
        dfti_example_status_print(Status);
        printf(" TEST FAIL\n");
    }

    /*
    /   Free arrays for input and expected data
    */
FREE_ARRAYS:
    free(x_exp);
    free(x_out);
    free(x_in);
    return 0;
}

