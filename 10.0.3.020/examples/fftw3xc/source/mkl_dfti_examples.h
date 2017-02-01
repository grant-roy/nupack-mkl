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
!      MKL DFTI examples definitions file (C-interface)
!
!*******************************************************************************

#include <stdio.h>
#include <math.h>
#include <stdlib.h>


/*
/  Print level definition
*/
#define  ACCURACY_PRINT 1
#define  LEGEND_PRINT 1

/*
/  Staic array's size definition
*/
#define M_MAX 13
#define N_MAX 15
#define K_MAX 12
#define L_MAX 10
#define M_MAX4 5
#define N_MAX4 3
#define K_MAX4 2
#define L_MAX4 4

/*
/  Accuracy definitions
*/

#define SINGLE_EPS 2.0E-5
#define DOUBLE_EPS 1.0E-11

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

int init_input_and_expected_vectors_c(void* x_in, void* x_exp, long n);
int init_input_and_expected_vectors_z(void* x_in, void* x_exp, long n);

int zero_init_c(void* x_in, long n);
int zero_init_z(void* x_in, long n);

int zero_init_s(void* x_in, long n);
int zero_init_d(void* x_in, long n);

int init_real_vectors_s(void*  x_in, long n);
int init_real_vectors_d(void* x_in, long n);

int init_multiple_columns_c(void* x, long n, long multiple,
                             long first_index, long step_x);
int init_multiple_columns_z(void* x, long n, long multiple,
                             long first_index, long step_x);
int init_3d_columns_c(void* x, long m, long n, long k,
                       long* strides);
int init_3d_columns_z(void* x, long m, long n, long k,
                       long* strides);
int init_3d_columns_mult_z(void* x, long m, long n, long k,
                       long* strides, long multiple, long dist);

int init_2d_columns_mult_z(void* x, long m, long n, long* strides, long multiple, long dist);
int init_2d_columns_z(void* x, long m, long n, long* strides);

float  check_result_c(void* x_in, void* x_exp, long n);
double check_result_z(void* x_in, void* x_exp, long n);

float  check_result_s(void* x_in, void* x_exp, long n);
double check_result_d(void* x_in, void* x_exp, long n);

void cblas_ccopy(int, void*, int, void*, int);
void cblas_zcopy(int, void*, int, void*, int);
void cblas_scopy(int, const float*, int, float*, int);
void cblas_dcopy(int, const double*, int, double*, int);
void scaling_d(void*, double, int);
void scaling_f(void*, float, int);
void scaling_dr(void*, double, int);
void scaling_fr(void*, float, int);

double check_result_2d_d(double*, double*, int, int);
double check_result_3d_d(double*, double*, int, int, int);
double check_result_2d_d_multiple(double*, double*, int, int, int, long*, int);
double check_result_3d_d_multiple(double*, double*, int, int, int, int, long*, int);

float check_result_2d_f(float*, float*, int, int);
float check_result_3d_f(float*, float*, int, int, int);
float check_result_2d_f_multiple(float*, float*, int, int, int, long*, int);
float check_result_3d_f_multiple(float*, float*, int, int, int, int, long*, int);

void scaling_2d_d(double*, double, int, int);
void scaling_3d_d(double*, double, int, int, int);
void scaling_2d_d_multiple(double*, double, int, int, int, long*, int);
void scaling_3d_d_multiple(double*, double, int, int, int, int, long*, int);

void scaling_2d_f(float*, float, int, int);
void scaling_3d_f(float*, float, int, int, int);
void scaling_2d_f_multiple(float*, float, int, int, int, long*, int);
void scaling_3d_f_multiple(float*, float, int, int, int, int, long*, int);
