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
!       MKL DFTI example's definitions file (C-interface)
!
!*******************************************************************************
*/

#include "mkl_cblas.h"
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

/*
/  Staic array's size definition
*/
#define M_MAX 15
#define N_MAX 13
#define K_MAX 12

/*
/  Print level definition
*/
#define  ADVANCED_DATA_PRINT 1
#define  ACCURACY_PRINT 1
#define  LEGEND_PRINT 1

/*
/  Accuracy definitions
*/

#define SINGLE_EPS 1.0E-6
#define DOUBLE_EPS 1.0E-12

/*
/  Constant's definition
*/
#define  MATH_PI     3.14159265358979323846  /* pi */


/* MKL test _Complex type definition */
typedef struct {
    float re;
    float im;
} mkl_float_complex;

typedef struct {
    double re;
    double im;
}mkl_double_complex;


/*
/  Example support function's interfaces
*/

void dfti_example_status_print(MKL_LONG status);

int read_data_file_1d(int argc, char *argv[], MKL_LONG* n);
int read_data_file_1d_mult(int argc, char *argv[], MKL_LONG* n,
                             MKL_LONG* first_index, MKL_LONG* multiple);
int read_data_file_2d(int argc, char *argv[], MKL_LONG* m, MKL_LONG* n);
int read_data_file_2d_mult(int argc, char *argv[], MKL_LONG* m, MKL_LONG* n,
                             MKL_LONG* first_index, MKL_LONG* multiple);
int read_data_file_3d(int argc, char *argv[], MKL_LONG* m, MKL_LONG* n, MKL_LONG* k);
int read_data_file_3d_strides(int argc, char *argv[], MKL_LONG* m, MKL_LONG* n, MKL_LONG* k,
                                MKL_LONG* index_strides_in);
int read_data_file_3d_strides_out(int argc, char *argv[], MKL_LONG* m, MKL_LONG* n, MKL_LONG* k,
                                MKL_LONG* index_strides_in, MKL_LONG* index_strides_out);

int init_forward_tone_and_expected_result_c(void* x_in, void* x_exp, MKL_LONG n);
int init_forward_tone_and_expected_result_z(void* x_in, void* x_exp, MKL_LONG n);
int init_backward_tone_and_expected_result_c(void* x_in, void* x_exp, MKL_LONG n);
int init_backward_tone_and_expected_result_z(void* x_in, void* x_exp, MKL_LONG n);

int init_input_and_expected_vectors_c(void* x_in, void* x_exp, MKL_LONG n);
int init_input_and_expected_vectors_z(void* x_in, void* x_exp, MKL_LONG n);

int zero_init_c(void* x_in, MKL_LONG n);
int zero_init_z(void* x_in, MKL_LONG n);

int zero_init_s(void* x_in, MKL_LONG n);
int zero_init_d(void* x_in, MKL_LONG n);

int one_init_s(void* x_in, MKL_LONG n);
int one_init_d(void* x_in, MKL_LONG n);

int init_real_vectors_s(void*  x_in, MKL_LONG n);
int init_real_vectors_d(void* x_in, MKL_LONG n);

int init_multiple_columns_c(void* x, MKL_LONG n, MKL_LONG multiple,
                             MKL_LONG first_index, MKL_LONG step_x);
int init_multiple_2d_columns_c(void* x, MKL_LONG n, MKL_LONG multiple, MKL_LONG* strides);

int init_multiple_columns_z(void* x, MKL_LONG n, MKL_LONG multiple,
                             MKL_LONG first_index, MKL_LONG step_x);
int init_multiple_2d_columns_z(void* x, MKL_LONG n, MKL_LONG multiple, MKL_LONG* strides);

int init_multiple_rows_c(void* x, MKL_LONG n, MKL_LONG multiple,
                          MKL_LONG first_index, MKL_LONG dist_x);
int init_multiple_rows_z(void* x, MKL_LONG n, MKL_LONG multiple,
                          MKL_LONG first_index, MKL_LONG dist_x);

int init_3d_columns_c(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG k,
                       MKL_LONG* strides);
int init_3d_columns_z(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG k,
                       MKL_LONG* strides);
int init_multiple_columns_s(void* x, MKL_LONG n, MKL_LONG multiple,
                             MKL_LONG dist, MKL_LONG* strides);
int init_multiple_columns_d(void* x, MKL_LONG n, MKL_LONG multiple,
                             MKL_LONG dist, MKL_LONG* strides);

int init_real_2d_s(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG* strides);
int init_real_2d_d(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG* strides);

int init_real_mult_2d_s(void*x, MKL_LONG m, MKL_LONG n, MKL_LONG mult, MKL_LONG dist, MKL_LONG* strides);
int init_real_mult_2d_d(void*x, MKL_LONG m, MKL_LONG n, MKL_LONG mult, MKL_LONG dist, MKL_LONG* strides);

int print_vector_c(void* x_in, MKL_LONG n);
int print_vector_z(void* x_in, MKL_LONG n);

int print_real_vector_s(void*  x_in, MKL_LONG n);
int print_real_vector_d(void* x_in, MKL_LONG n);

int print_three_rows_c(void* x, MKL_LONG n, MKL_LONG first_index, MKL_LONG dist_x);
int print_three_rows_z(void* x, MKL_LONG n, MKL_LONG first_index, MKL_LONG dist_x);

int print_three_columns_c(void* x, MKL_LONG n, MKL_LONG first_index, MKL_LONG step_x);
int print_three_columns_z(void* x, MKL_LONG n, MKL_LONG first_index, MKL_LONG step_x);

int print_three_2d_columns_c(void* x, MKL_LONG n, MKL_LONG* strides);
int print_three_2d_columns_z(void* x, MKL_LONG n, MKL_LONG* strides);


int print_three_3d_columns_c(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG k,
                        MKL_LONG* strides);
int print_three_3d_columns_z(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG k,
                       MKL_LONG* strides);

int print_three_columns_s(void* x, MKL_LONG n, MKL_LONG dist, MKL_LONG* strides);
int print_three_columns_d(void* x, MKL_LONG n, MKL_LONG dist, MKL_LONG* strides);

int print_three_first_and_three_last_columns_s(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG* strides);
int print_three_first_and_three_last_columns_d(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG* strides);

int print_three_2d_columns_s(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG* strides);
int print_three_2d_columns_d(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG* strides);

float  check_result_c(void* x_in, void* x_exp, MKL_LONG n);
double check_result_z(void* x_in, void* x_exp, MKL_LONG n);

float  check_result_s(void* x_in, void* x_exp, MKL_LONG n);
double check_result_d(void* x_in, void* x_exp, MKL_LONG n);

float  check_result_2d_s(void* x_in, void* x_exp, MKL_LONG m, MKL_LONG n, MKL_LONG* strides);
double check_result_2d_d(void* x_in, void* x_exp, MKL_LONG m, MKL_LONG n, MKL_LONG* strides);

double check_result_mult_2d_d(void* x, void* y, MKL_LONG m, MKL_LONG n,
                               MKL_LONG mult, MKL_LONG dist, MKL_LONG* strides);
float  check_result_mult_2d_s(void* x, void* y, MKL_LONG m, MKL_LONG n,
                               MKL_LONG mult, MKL_LONG dist, MKL_LONG* strides);
