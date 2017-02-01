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
!
!   Content:
!       MKL DFTI interface example program (C-interface)
!
!       Examples support function set
!
!------------------------------------------------------------------------
*/
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "mkl_dfti.h"
#include "mkl_dfti_examples.h"

#if defined( MKL_ILP64 ) && defined ( _WIN )
#define SCANF_MKL_LONG_FORMAT "%I64d"
#else
#define SCANF_MKL_LONG_FORMAT "%ld"
#endif

/*
/   Read data from input file routines
*/

int read_data_file_1d(int argc, char *argv[], MKL_LONG* n)
{
    FILE *in_file;
    char *in_file_name;
    char scan_line[100];
    int  maxline=100;

    if (argc == 1) {
        printf("\n You must specify in_file data file as 1-st parameter");
        return 1;
    } /* if */
    in_file_name = argv[1];

    if( (in_file = fopen( in_file_name, "r" )) == NULL ) {
        printf("\nERROR on OPEN '%s' with mode=%s\n", in_file_name, "r");
        return 1;
    } /* if */

    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, n);

    fclose(in_file);
    return 0;
}

int  read_data_file_1d_mult(int argc, char *argv[], MKL_LONG* n,
                             MKL_LONG* first_index, MKL_LONG* multiple)
{
    FILE *in_file;
    char *in_file_name;
    char scan_line[100];
    int  maxline=100;

    if (argc == 1) {
        printf("\n You must specify in_file data file as 1-st parameter");
        return 1;
    } /* if */
    in_file_name = argv[1];

    if( (in_file = fopen( in_file_name, "r" )) == NULL ) {
        printf("\nERROR on OPEN '%s' with mode=%s\n", in_file_name, "r");
        return 1;
    } /* if */

    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, n);

    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, first_index);

    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, multiple);

    fclose(in_file);
    return 0;
}

int  read_data_file_2d(int argc, char *argv[], MKL_LONG* m, MKL_LONG* n)
{
    FILE *in_file;
    char *in_file_name;
    char scan_line[100];
    int  maxline=100;

    if (argc == 1) {
        printf("\n You must specify in_file data file as 1-st parameter");
        return 1;
    } /* if */
    in_file_name = argv[1];

    if( (in_file = fopen( in_file_name, "r" )) == NULL ) {
        printf("\nERROR on OPEN '%s' with mode=%s\n", in_file_name, "r");
        return 1;
    } /* if */

    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, m);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, n);

    fclose(in_file);
    return 0;
}

int  read_data_file_2d_mult(int argc, char *argv[], MKL_LONG* m, MKL_LONG* n,
                             MKL_LONG* first_index, MKL_LONG* multiple)
{
    FILE *in_file;
    char *in_file_name;
    char scan_line[100];
    int  maxline=100;

    if (argc == 1) {
        printf("\n You must specify in_file data file as 1-st parameter");
        return 1;
    } /* if */
    in_file_name = argv[1];

    if( (in_file = fopen( in_file_name, "r" )) == NULL ) {
        printf("\nERROR on OPEN '%s' with mode=%s\n", in_file_name, "r");
        return 1;
    } /* if */

    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, m);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, n);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, first_index);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, multiple);

    fclose(in_file);
    return 0;
}


int  read_data_file_3d(int argc, char *argv[], MKL_LONG* m, MKL_LONG* n, MKL_LONG* k)
{
    FILE *in_file;
    char *in_file_name;
    char scan_line[100];
    int  maxline=100;

    if (argc == 1) {
        printf("\n You must specify in_file data file as 1-st parameter");
        return 1;
    } /* if */
    in_file_name = argv[1];

    if( (in_file = fopen( in_file_name, "r" )) == NULL ) {
        printf("\nERROR on OPEN '%s' with mode=%s\n", in_file_name, "r");
        return 1;
    } /* if */

    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, m);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, n);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, k);

    fclose(in_file);
    return 0;
}

int read_data_file_3d_strides(int argc, char *argv[], MKL_LONG* m, MKL_LONG* n, MKL_LONG* k,
                                MKL_LONG* index_strides_in)
{
    FILE *in_file;
    char *in_file_name;
    char scan_line[100];
    int  maxline=100;

    if (argc == 1) {
        printf("\n You must specify in_file data file as 1-st parameter");
        return 1;
    } /* if */
    in_file_name = argv[1];

    if( (in_file = fopen( in_file_name, "r" )) == NULL ) {
        printf("\nERROR on OPEN '%s' with mode=%s\n", in_file_name, "r");
        return 1;
    } /* if */

    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, m);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, n);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, k);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, &index_strides_in[0]);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, &index_strides_in[1]);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, &index_strides_in[2]);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, &index_strides_in[3]);

    fclose(in_file);
    return 0;
}

int read_data_file_3d_strides_out(int argc, char *argv[], MKL_LONG* m, MKL_LONG* n, MKL_LONG* k,
                                MKL_LONG* index_strides_in, MKL_LONG* index_strides_out)
{
    FILE *in_file;
    char *in_file_name;
    char scan_line[100];
    int  maxline=100;

    if (argc == 1) {
        printf("\n You must specify in_file data file as 1-st parameter");
        return 1;
    } /* if */
    in_file_name = argv[1];

    if( (in_file = fopen( in_file_name, "r" )) == NULL ) {
        printf("\nERROR on OPEN '%s' with mode=%s\n", in_file_name, "r");
        return 1;
    } /* if */

    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, m);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, n);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, k);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, &index_strides_in[0]);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, &index_strides_in[1]);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, &index_strides_in[2]);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, &index_strides_in[3]);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, &index_strides_out[0]);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, &index_strides_out[1]);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, &index_strides_out[2]);
    fgets(scan_line, maxline, in_file);
    fscanf(in_file,SCANF_MKL_LONG_FORMAT, &index_strides_out[3]);

    fclose(in_file);
    return 0;
}

/*
/   init Sinusoid tone and expected uotput result
*/

int init_forward_tone_and_expected_result_c(void* x_in, void* x_exp, MKL_LONG n)
{
    float* tone_in  = x_in;
    float* res_exp  = x_exp;

    MKL_LONG i;

    if(n == 1) return 0;

    /*
    /   put input data into X(1),...,X(n)
    */
    for (i = 0; i < n; i++) {
        tone_in[2*i]   = cos(2 * MATH_PI * i/ n);
        tone_in[2*i+1] = sin(2 * MATH_PI * i/ n );
    }

    /*
    /   put expected data into X_EXP(1),...,X_EXP(n)
    */
    for (i = 0; i < 2*n; i++) res_exp[i] = 0.0;
    res_exp[2] = 1.0;

    return 0;
}/* init_forward_tone_and_expected_result_c */

int init_forward_tone_and_expected_result_z(void* x_in, void* x_exp, MKL_LONG n)
{
    double* tone_in  = x_in;
    double* res_exp  = x_exp;

    MKL_LONG i;

    if(n == 1) return 0;

    /*
    /   put input data into X(1),...,X(n)
    */
    for (i = 0; i < n; i++) {
        tone_in[2*i]   = cos(2 * MATH_PI * i/ n);
        tone_in[2*i+1] = sin(2 * MATH_PI * i/ n );
    }

    /*
    /   put expected data into X_EXP(1),...,X_EXP(n)
    */
    for (i = 0; i < 2*n; i++) res_exp[i] = 0.0;
    res_exp[2] = 1.0;

    return 0;
}/* init_forward_tone_and_expected_result_z */

int init_backward_tone_and_expected_result_c(void* x_in, void* x_exp, MKL_LONG n)
{
    float* tone_in  = x_in;
    float* res_exp  = x_exp;

    MKL_LONG i;

    if(n == 1) return 0;

    /*
    /   put input data into X(1),...,X(n)
    */
    for (i = 0; i < n; i++) {
        tone_in[2*i]   =  cos(2 * MATH_PI * i/ n);
        tone_in[2*i+1] = -sin(2 * MATH_PI * i/ n );
    }

    /*
    /   put expected data into X_EXP(1),...,X_EXP(n)
    */
    for (i = 0; i < 2*n; i++) res_exp[i] = 0.0;
    res_exp[2] = 1.0;

    return 0;
}/* init_backward_tone_and_expected_result_c */

int init_backward_tone_and_expected_result_z(void* x_in, void* x_exp, MKL_LONG n)
{
    double* tone_in  = x_in;
    double* res_exp  = x_exp;

    MKL_LONG i;

    if(n == 1) return 0;

    /*
    /   put input data into X(1),...,X(n)
    */
    for (i = 0; i < n; i++) {
        tone_in[2*i]   =  cos(2 * MATH_PI * i/ n);
        tone_in[2*i+1] = -sin(2 * MATH_PI * i/ n );
    }

    /*
    /   put expected data into X_EXP(1),...,X_EXP(n)
    */
    for (i = 0; i < 2*n; i++) res_exp[i] = 0.0;
    res_exp[2] = 1.0;

    return 0;
}/* init_backward_tone_and_expected_result_z */


/*
/   initialize x_in and copy to expected x_exp
*/

int init_input_and_expected_vectors_c(void* x_init, void* res_exp, MKL_LONG n)
{
    float* x_in  = x_init;
    float* x_exp = res_exp;

    MKL_LONG    i;
    float   step, step0;
    /*
    /   put input data into X(1),...,X(n)
    */
    step = -MATH_PI;
    step0 = (2.0*MATH_PI)/(float)n;
    step = step + step0;
    for (i = 0; i < n; i++) {
        x_in[2*i]   = sin(step)*sqrt(3.0)/(float)2;
        x_in[2*i+1] = sin(step)/sqrt(3.0);
        step = step + step0;
    }

    /*
    /   put expected data into X_EXP(1),...,X_EXP(n)
    */
    for (i = 0; i < n; i++){
        x_exp[2*i]   = x_in[2*i];
        x_exp[2*i+1] = x_in[2*i+1];
    }
    return 0;
}/* init_input_and_expected_vectors_c */

int init_input_and_expected_vectors_z(void* x_init, void* res_exp, MKL_LONG n)
{
    double* x_in  = x_init;
    double* x_exp = res_exp;

    MKL_LONG    i;
    double   step, step0;
    /*
    /   put input data into X(1),...,X(n)
    */
    step = -MATH_PI;
    step0 = (2.0*MATH_PI)/(double)n;
    step = step + step0;
    for (i = 0; i < n; i++) {
        x_in[2*i]   = sin(step)*sqrt(3.0)/(double)2;
        x_in[2*i+1] = sin(step)/sqrt(3.0);
        step = step + step0;
    }

    /*
    /   put expected data into X_EXP(1),...,X_EXP(n)
    */
    for (i = 0; i < n; i++){
        x_exp[2*i]   = x_in[2*i];
        x_exp[2*i+1] = x_in[2*i+1];
    }
    return 0;
}/* init_input_and_expected_vectors_z */

int init_real_vectors_s(void* x, MKL_LONG n)
{
    float*  x_in = x;
    MKL_LONG    i;
    float   step;

    /*
    /   put input data into X(1),...,X(n)
    */
    for (i = 0; i < n; i++) {
          step  = (float)(i+1);
          x_in[i] = sin(step)*sqrt(3.0)/(float)2;
    }
    return 0;
}/* init_real_vectors_s */

int init_real_vectors_d(void* x, MKL_LONG n)
{
    double* x_in = x;
    MKL_LONG    i;
    double  step;

    /*
    /   put input data into X(1),...,X(n)
    */
    for (i = 0; i < n; i++) {
          step  = (double)(i+1);
          x_in[i] = sin(step)*sqrt(3.0)/(double)2;
    }
    return 0;
}/* init_real_vectors_d */

int init_real_mult_2d_d(void*x, MKL_LONG m, MKL_LONG n, MKL_LONG mult, MKL_LONG dist, MKL_LONG* strides)
{
    double* x_in;
    double  step;
    MKL_LONG    i, j, k;
    MKL_LONG    first_index, step_1d, step_2d;

    first_index = strides[0];
    step_1d     = strides[1];
    step_2d     = strides[2];

    for (k = 0; k < mult; k++) {

        x_in  = (double*)x + k*dist + first_index;

        for (i = 0; i < n; i++) {
            for (j = 0; j < m; j++) {
                step  = (double)(i+1)*(i+j+1);
                x_in[j*step_1d] = sin(step)*sqrt(3.0)/(double)2;
            }
            x_in  += step_2d;
        }/* for i */
    }/* for k */
    return 0;
}

int init_real_mult_2d_s(void*x, MKL_LONG m, MKL_LONG n, MKL_LONG mult, MKL_LONG dist, MKL_LONG* strides)
{
    float* x_in;
    float  step;
    MKL_LONG    i, j, k;
    MKL_LONG    first_index, step_1d, step_2d;

    first_index = strides[0];
    step_1d     = strides[1];
    step_2d     = strides[2];

    for (k = 0; k < mult; k++) {

        x_in  = (float*)x + k*dist + first_index;

        for (i = 0; i < n; i++) {
            for (j = 0; j < m; j++) {
                step  = (float)(i+1)*(j+1);
                x_in[j*step_1d] = sin(step)*sqrt(3.0)/(float)2;
            }
            x_in  += step_2d;
        }/* for i */
    }/* for k */
    return 0;
}

int zero_init_c(void* x, MKL_LONG n)
{
    float*  x_in = x;
    MKL_LONG    i;

    for (i = 0; i < n; i++) {
            x_in[2*i]   = 0.0;
            x_in[2*i+1] = 0.0;
    }/* for i */

    return 0;
}/* zero_init_c */

int zero_init_z(void* x, MKL_LONG n)
{
    double* x_in = x;
    MKL_LONG    i;

    for (i = 0; i < n; i++) {
            x_in[2*i]   = 0.0;
            x_in[2*i+1] = 0.0;
    }/* for i */

    return 0;
}/* zero_init_z */


int zero_init_s(void* x, MKL_LONG n)
{
    float*  x_in = x;
    MKL_LONG    i;
    for (i = 0; i < n; i++)
        x_in[i] = 0.0;
    return 0;
}/* zero_init_s */

int zero_init_d(void* x, MKL_LONG n)
{
    double*  x_in = x;
    MKL_LONG    i;
    for (i = 0; i < n; i++)
        x_in[i] = 0.0;
    return 0;
}/* zero_init_d */

int one_init_d(void* x, MKL_LONG n)
{
    double* x_in = x;
    MKL_LONG    i;
    for (i = 0; i < n; i++)
        x_in[i] = 1.0;
    return 0;
}/* one_init_d */

int one_init_s(void* x, MKL_LONG n)
{
    float*  x_in = x;
    MKL_LONG    i;
    for (i = 0; i < n; i++)
        x_in[i] = 1.0;
    return 0;
}/* one_init_s */

int init_multiple_columns_c(void* x, MKL_LONG n, MKL_LONG multiple,
                             MKL_LONG first_index, MKL_LONG step_x)
{
    float*  x_in = x;
    float   step;
    MKL_LONG    i, j;

    x_in += 2*first_index;

    for (i = 0; i < multiple; i++) {
        for (j = 0; j < n; j++) {
            step = (float)(i+1)*(float)(i+j+1);
            x_in[2*j*step_x]   = sin(step)*sqrt(3.0)/(float)2;
            x_in[2*j*step_x+1] = sin(step)/sqrt(3.0);
        }
        x_in += 2;
    }/* for i */
    return 0;
}/* init_multiple_columns_c */

int init_multiple_columns_s(void* x, MKL_LONG n, MKL_LONG multiple,
                             MKL_LONG dist, MKL_LONG* strides)
{
    float*  x_in = x;
    float   step;
    MKL_LONG    i, j;
    MKL_LONG    first_index, stride_x;

    first_index = strides[0];
    stride_x   = strides[1];

    x_in += first_index;

    for (i = 0; i < multiple; i++) {
        for (j = 0; j < n; j++) {
          step  = (float)(i+1)*(j+1);
          x_in[j*stride_x] = sin(step)*sqrt(3.0)/(float)2;
        }
        x_in +=dist;
    }/* for i */
    return 0;
}/* init_multiple_columns_s*/

int init_multiple_columns_d(void* x, MKL_LONG n, MKL_LONG multiple,
                             MKL_LONG dist, MKL_LONG* strides)
{
    double*     x_in = x;
    double      step;
    MKL_LONG        i, j;
    MKL_LONG        first_index, stride_x;

    first_index = strides[0];
    stride_x   = strides[1];

    x_in += first_index;

    for (i = 0; i < multiple; i++) {
        for (j = 0; j < n; j++) {
          step  = (double)(i+1)*(j+1);
          x_in[j*stride_x] = sin(step)*sqrt(3.0)/(double)2;
        }
        x_in +=dist;
    }/* for i */
    return 0;
}/* init_multiple_columns_d */

int init_real_2d_s(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG* strides)
{
    float*  x_in = x;
    float   step;
    MKL_LONG    first_index;
    MKL_LONG    step_1d, step_2d;
    MKL_LONG    i, j;

    first_index = strides[0];
    step_1d     = strides[1];
    step_2d     = strides[2];

    x_in += first_index;

    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++) {
            step = (float)(i+1)*(float)(i+j+1);
            x_in[j*step_1d] = sin(step)*sqrt(3.0)/(float)2;
        }
        x_in += step_2d;
    }/* for i */
    return 0;
}

int init_real_2d_d(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG* strides)
{
    double*  x_in = x;
    double   step;
    MKL_LONG    first_index;
    MKL_LONG    step_1d, step_2d;
    MKL_LONG    i, j;

    first_index = strides[0];
    step_1d     = strides[1];
    step_2d     = strides[2];

    x_in += first_index;

    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++) {
            step = (double)(i+1)*(double)(i+j+1);
            x_in[j*step_1d] = sin(step)*sqrt(3.0)/(double)2;
        }
        x_in += step_2d;
    }/* for i */
    return 0;
}

int init_multiple_2d_columns_c(void* x, MKL_LONG n, MKL_LONG multiple,
                             MKL_LONG* strides)
{
    float*  x_in = x;
    float   step;
    MKL_LONG    i, j;
    MKL_LONG    first_index, step_1d, step_2d;

    first_index = 2*strides[0];
    step_1d     = 2*strides[1];
    step_2d     = 2*strides[2];

    x_in += first_index;

    for (i = 0; i < multiple; i++) {
        for (j = 0; j < n; j++) {
            step = (float)(i+1)*(float)(i+j+1);
            x_in[j*step_1d]   = sin(step)*sqrt(3.0)/(float)2;
            x_in[j*step_1d+1] = sin(step)/sqrt(3.0);
        }
        x_in += step_2d;
    }/* for i */
    return 0;
}/* init_multiple_columns_c */


int init_multiple_2d_columns_z(void* x, MKL_LONG n, MKL_LONG multiple,
                             MKL_LONG* strides)
{
    double*  x_in = x;
    double   step;
    MKL_LONG    i, j;
    MKL_LONG    first_index, step_1d, step_2d;

    first_index = 2*strides[0];
    step_1d     = 2*strides[1];
    step_2d     = 2*strides[2];

    x_in += first_index;
    for (i = 0; i < multiple; i++) {
        for (j = 0; j < n; j++) {
            step = (double)(i+1)*(double)(i+j+1);
            x_in[j*step_1d]   = sin(step)*sqrt(3.0)/(double)2;
            x_in[j*step_1d+1] = sin(step)/sqrt(3.0);
        }
        x_in += step_2d;
    }/* for i */
    return 0;
}/* init_multiple_2d_columns_z */

int init_multiple_2d_columns_s(void* x, MKL_LONG n, MKL_LONG multiple,
                               MKL_LONG* strides)
{
    float*  x_in = x;
    float   step;
    MKL_LONG    i, j;
    MKL_LONG    first_index, step_1d, step_2d;

    first_index = strides[0];
    step_1d     = strides[1];
    step_2d     = strides[2];

    x_in += first_index;

    for (i = 0; i < multiple; i++) {
        for (j = 0; j < n; j++) {
            step = (float)(i+1)*(float)(i+j+1);
            x_in[j*step_1d]   = sin(step)*sqrt(3.0)/(float)2;
        }
        x_in += step_2d;
    }/* for i */
    return 0;
}/* init_multiple_2d_columns_s */

int init_multiple_2d_columns_d(void* x, MKL_LONG n, MKL_LONG multiple,
                               MKL_LONG* strides)
{
    double*  x_in = x;
    double  step;
    MKL_LONG    i, j;
    MKL_LONG    first_index, step_1d, step_2d;

    first_index = strides[0];
    step_1d     = strides[1];
    step_2d     = strides[2];

    x_in += first_index;

    for (i = 0; i < multiple; i++) {
        for (j = 0; j < n; j++) {
            step = (double)(i+1)*(double)(i+j+1);
            x_in[j*step_1d]   = sin(step)*sqrt(3.0)/(double)2;
        }
        x_in += step_2d;
    }/* for i */
    return 0;
}/* init_multiple_2d_columns_d */

int init_3d_columns_c(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG k,
                       MKL_LONG* strides)
{
    float*  x_2d = x;
    float*  x_in;
    float   step;
    MKL_LONG    i, j, l;

    MKL_LONG    first_index;
    MKL_LONG    step_m, step_n, step_k;

    first_index = 2*strides[0];
    step_m      = 2*strides[1];
    step_n      = 2*strides[2];
    step_k      = 2*strides[3];

    x_2d += first_index;
    for (l = 0; l < k; l++) {
         x_in = x_2d;
         for (i = 0; i < n; i++) {
            for (j = 0; j < m; j++) {
                step = (float)(l+1)*(float)(i+1)*(float)(i+j+1);
                x_in[j*step_m]   = sin(step)*sqrt(3.0)/(float)2;
                x_in[j*step_m+1] = sin(step)/sqrt(3.0);
             }/* for j */
            x_in += step_n;
        }/* for i */
        x_2d += step_k;
    }/* for l */

    return 0;
}/* init_3d_columns_c */

int init_3d_columns_s(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG k,
                       MKL_LONG* strides)
{
    float*  x_2d = x;
    float*  x_in;
    float   step;
    MKL_LONG    i, j, l;

    MKL_LONG    first_index;
    MKL_LONG    step_m, step_n, step_k;

    first_index = strides[0];
    step_m      = strides[1];
    step_n      = strides[2];
    step_k      = strides[3];

    x_2d += first_index;
    for (l = 0; l < k; l++) {
         x_in = x_2d;
         for (i = 0; i < n; i++) {
            for (j = 0; j < m; j++) {
                step = (float)(l+1)*(float)(i+1)*(float)(i+j+1);
                x_in[j*step_m]   = sin(step)*sqrt(3.0)/(float)2;
             }/* for j */
            x_in += step_n;
        }/* for i */
        x_2d += step_k;
    }/* for l */

    return 0;
}/* init_3d_columns_s */

int init_3d_columns_z(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG k,
                       MKL_LONG* strides)
{
    double* x_2d = x;
    double* x_in;
    double  step;
    MKL_LONG    i, j, l;
    MKL_LONG    first_index;
    MKL_LONG    step_m, step_n, step_k;

    first_index = 2*strides[0];
    step_m      = 2*strides[1];
    step_n      = 2*strides[2];
    step_k      = 2*strides[3];

    x_2d += first_index;
    for (l = 0; l < k; l++) {
         x_in = x_2d;
         for (i = 0; i < n; i++) {
            for (j = 0; j < m; j++) {
                step = (float)(l+1)*(float)(i+1)*(float)(i+j+1);
                x_in[j*step_m]   = sin(step)*sqrt(3.0)/(float)2;
                x_in[j*step_m+1]   = sin(step)/sqrt(3.0);
            }
            x_in += step_n;
        }/* for i */
        x_2d += step_k;
    }/* for l */

    return 0;
}/* init_3d_columns_z */

int init_3d_columns_d(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG k,
                       MKL_LONG* strides)
{
    double* x_2d = x;
    double* x_in;
    double  step;
    MKL_LONG    i, j, l;
    MKL_LONG    first_index;
    MKL_LONG    step_m, step_n, step_k;

    first_index = strides[0];
    step_m      = strides[1];
    step_n      = strides[2];
    step_k      = strides[3];

    x_2d += first_index;
    for (l = 0; l < k; l++) {
         x_in = x_2d;
         for (i = 0; i < n; i++) {
            for (j = 0; j < m; j++) {
                step = (float)(l+1)*(float)(i+1)*(float)(i+j+1);
                x_in[j*step_m]   = sin(step)*sqrt(3.0)/(float)2;
                /*x_in[j*step_m+1]   = sin(step)/sqrt(3.0);*/
            }
            x_in += step_n;
        }/* for i */
        x_2d += step_k;
    }/* for l */

    return 0;
}/* init_3d_columns_d */

int init_multiple_rows_c(void* x, MKL_LONG n, MKL_LONG multiple,
                          MKL_LONG first_index, MKL_LONG dist_x)
{
    float*  x_in = x;
    float   step;
    MKL_LONG    i, j, index;

    for (i = 0; i < multiple; i++) {

        index = first_index + i * dist_x;
        for (j = index; j < (index+n); j++) {
            step = (float)(j+1);
            x_in[2*j]   = sin(step)*sqrt(3.0)/(float)2;
            x_in[2*j+1] = sin(step)/sqrt(3.0);
        }
    }/* for i */

    return 0;
}/* init_multiple_rows_c */

int init_multiple_columns_z(void* x, MKL_LONG n, MKL_LONG multiple,
                             MKL_LONG first_index, MKL_LONG step_x)
{
    double*  x_in = x;
    double   step;
    MKL_LONG    i, j, index;

    x_in += 2*first_index;
    for (i = 0; i < multiple; i++) {
        for (j = 0; j < n; j++) {
            step = (double)(i+j+1);
            x_in[2*j*step_x]   = sin(step)*sqrt(3.0)/(double)2;
            x_in[2*j*step_x+1] = sin(step)/sqrt(3.0);
        }
        x_in += 2;
    }/* for i */
    return 0;
}/* init_multiple_columns_z */


int init_multiple_rows_z(void* x, MKL_LONG n, MKL_LONG multiple,
                          MKL_LONG first_index, MKL_LONG dist_x)
{
    double*  x_in = x;
    double   step;
    MKL_LONG    i, j, index;

    for (i = 0; i < multiple; i++) {

        index = first_index + i * dist_x;
        for (j = index; j < (index+n); j++) {
            step = (double)(j+1);
            x_in[2*j]   = sin(step)*sqrt(3.0)/(double)2;
            x_in[2*j+1] = sin(step)/sqrt(3.0);
        }
    }/* for i */
    return 0;
}/* init_multiple_rows_z */


/*
/   Print support
*/

int print_vector_c(void* x, MKL_LONG n)
{
    float* x_in  = x;
    MKL_LONG    i;

    for (i = 0; i < n; i++)
    printf(" X(%3d)=(%8.3f, %8.3f)\n", i, x_in[2*i], x_in[2*i+1]);

    return 0;
}/* print_vector_z */

int print_vector_z(void* x, MKL_LONG n)
{
    double* x_in  = x;
    MKL_LONG    i;

    for (i = 0; i < n; i++)
    printf(" X(%3d)=(%8.3f, %8.3f)\n", i, x_in[2*i], x_in[2*i+1]);

    return 0;
}/* print_vector_z */

int print_real_vector_s(void* x, MKL_LONG n)
{
    float* x_in = x;
    MKL_LONG    i;

    for (i = 0; i < n; i++)
    printf(" X(%3d)=%8.3f \n", i, x_in[i]);

    return 0;
}/* print_real_vector_s */

int print_real_vector_d(void* x, MKL_LONG n)
{
    double* x_in = x;
    MKL_LONG    i;

    for (i = 0; i < n; i++)
    printf(" X(%3d)=%8.3f \n", i, x_in[i]);

    return 0;
}/* print_real_vector_d */

int print_three_rows_c(void* x, MKL_LONG n, MKL_LONG first_index, MKL_LONG dist_x)
{
    float*  x_first = x;
    float*  x_second;
    float*  x_third;
    MKL_LONG    j;

    x_first += 2*first_index;
    x_second = x_first + 2*dist_x;
    x_third  = x_first + 4*dist_x;

    for (j = 0; j < n; j++) {
        printf(" (%8.3f, %8.3f)  ", x_first[2*j],  x_first[2*j+1]);
        printf(" (%8.3f, %8.3f)  ", x_second[2*j], x_second[2*j+1]);
        printf(" (%8.3f, %8.3f)\n", x_third[2*j],  x_third[2*j+1]);
    }/* for */

    return 0;
}/* print_three_rows_c */


int print_three_columns_c(void* x, MKL_LONG n, MKL_LONG first_index, MKL_LONG step_x)
{
    float*  x_first = x;
    float*  x_second;
    float*  x_third;
    MKL_LONG    j;

    x_first  += 2*first_index;
    x_second = x_first + 2;
    x_third  = x_first + 4;

    for (j = 0; j < n; j++) {
        printf(" (%8.3f, %8.3f) ", x_first[2*j*step_x],  x_first[2*j*step_x+1]);
        printf(" (%8.3f, %8.3f) ", x_second[2*j*step_x], x_second[2*j*step_x+1]);
        printf(" (%8.3f, %8.3f)\n", x_third[2*j*step_x],  x_third[2*j*step_x+1]);
    }/* for */

    return 0;
}/* print_three_columns_c */

int print_three_rows_z(void* x, MKL_LONG n, MKL_LONG first_index, MKL_LONG dist_x)
{
    double*  x_first = x;
    double*  x_second;
    double*  x_third;
    MKL_LONG    j;

    x_first += 2*first_index;
    x_second = x_first + 2*dist_x;
    x_third  = x_first + 4*dist_x;

    for (j = 0; j < n; j++) {
        printf(" (%8.3f, %8.3f)  ", x_first[2*j],  x_first[2*j+1]);
        printf(" (%8.3f, %8.3f)  ", x_second[2*j], x_second[2*j+1]);
        printf(" (%8.3f, %8.3f)\n", x_third[2*j],  x_third[2*j+1]);
    }/* for */

    return 0;
}/* print_three_rows_z */


int print_three_columns_z(void* x, MKL_LONG n, MKL_LONG first_index, MKL_LONG step_x)
{
    double*  x_first = x;
    double*  x_second;
    double*  x_third;
    MKL_LONG    j;

    x_first  += 2*first_index;
    x_second = x_first + 2;
    x_third  = x_first + 4;

    for (j = 0; j < n; j++) {
        printf(" (%8.3f, %8.3f) ", x_first[2*j*step_x],  x_first[2*j*step_x+1]);
        printf(" (%8.3f, %8.3f) ", x_second[2*j*step_x], x_second[2*j*step_x+1]);
        printf(" (%8.3f, %8.3f)\n", x_third[2*j*step_x],  x_third[2*j*step_x+1]);
    }/* for */

    return 0;
}/* print_three_columns_z */

int print_three_columns_s(void* x, MKL_LONG n, MKL_LONG dist, MKL_LONG* strides)
{
    float*  x_first = x;
    float*  x_second;
    float*  x_third;

    MKL_LONG    first_index;
    MKL_LONG    step_x;
    MKL_LONG    j;

    first_index = strides[0];
    step_x      = strides[1];

    x_first += first_index;
    x_second = x_first + dist;
    x_third  = x_first + 2*dist;

    for (j = 0; j < n; j++) {
        printf(" %8.3f    ", x_first [j*step_x]);
        printf(" %8.3f    ", x_second[j*step_x]);
        printf(" %8.3f  \n", x_third [j*step_x]);
    }/* for */

    return 0;
}/* print_three_columns_s */

int print_three_columns_complex(void* x, MKL_LONG n, MKL_LONG dist, MKL_LONG* strides)
{
    float*  x_first = x;
    float*  x_second;
    float*  x_third;

    MKL_LONG    first_index;
    MKL_LONG    step_x;
    MKL_LONG    j;

    first_index = strides[0];
    step_x      = strides[1];

    x_first += first_index*2;
    x_second = x_first + dist*2;
    x_third  = x_first + 4*dist;

    for (j = 0; j < n; j++) {
        printf(" (%8.3f, %8.3f) ", x_first [2*j*step_x], x_first [(2*j+1)*step_x]);
        printf(" (%8.3f, %8.3f) ", x_second[2*j*step_x], x_second[(2*j+1)*step_x]);
        printf(" (%8.3f, %8.3f) \n", x_third [2*j*step_x], x_third [(2*j+1)*step_x]);
    }/* for */

    return 0;
}/* print_three_columns_complex */

int print_three_columns_complex_z(void* x, MKL_LONG n, MKL_LONG dist, MKL_LONG* strides)
{
    double*  x_first = x;
    double*  x_second;
    double*  x_third;

    MKL_LONG    first_index;
    MKL_LONG    step_x;
    MKL_LONG    j;

    first_index = strides[0];
    step_x      = strides[1];

    x_first += first_index*2;
    x_second = x_first + dist*2;
    x_third  = x_first + 4*dist;

    for (j = 0; j < n; j++) {
        printf(" (%8.3f, %8.3f) ", x_first [2*j*step_x], x_first [(2*j+1)*step_x]);
        printf(" (%8.3f, %8.3f) ", x_second[2*j*step_x], x_second[(2*j+1)*step_x]);
        printf(" (%8.3f, %8.3f) \n", x_third [2*j*step_x], x_third [(2*j+1)*step_x]);
    }/* for */

    return 0;
}/* print_three_columns_complex_z */

int print_three_columns_d(void* x, MKL_LONG n, MKL_LONG dist, MKL_LONG* strides)
{
    double*  x_first = x;
    double*  x_second;
    double*  x_third;

    MKL_LONG    first_index;
    MKL_LONG    step_x;
    MKL_LONG    j;

    first_index = strides[0];
    step_x      = strides[1];

    x_first += first_index;
    x_second = x_first + dist;
    x_third  = x_first + 2*dist;

    for (j = 0; j < n; j++) {
        printf(" %8.3f    ", x_first [j*step_x]);
        printf(" %8.3f    ", x_second[j*step_x]);
        printf(" %8.3f  \n", x_third [j*step_x]);
    }/* for */

    return 0;
}/* print_three_columns_d */

int print_three_first_and_three_last_columns_d(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG* strides)
{
    double* x_first = x;
    double* x_second;
    double* x_third;
    double* x_last2;
    double* x_last1;
    double* x_last0;

    MKL_LONG    first_index;
    MKL_LONG    step_x;
    MKL_LONG    step_y;
    MKL_LONG    j;

    first_index = strides[0];
    step_x      = strides[1];
    step_y      = strides[2];

    x_first += first_index;
    x_second = x_first + step_y;
    x_third  = x_first + 2*step_y;
    x_last2  = x_first + (n-3)*step_y;
    x_last1  = x_first + (n-2)*step_y;
    x_last0  = x_first + (n-1)*step_y;

    for (j = 0; j < m; j++) {
        printf(" %8.3f    ", x_first [j*step_x]);
        printf(" %8.3f    ", x_second[j*step_x]);
        printf(" %8.3f    ", x_third [j*step_x]);
        printf(" ... ");
        printf(" %8.3f    ", x_last2[j*step_x]);
        printf(" %8.3f    ", x_last1[j*step_x]);
        printf(" %8.3f  \n", x_last0[j*step_x]);
    }/* for */

    return 0;
}/* print_three_first_and_three_last_columns_d */

int print_three_first_and_three_last_columns_s(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG* strides)
{
    float*  x_first = x;
    float*  x_second;
    float*  x_third;
    float*  x_last2;
    float*  x_last1;
    float*  x_last0;

    MKL_LONG    first_index;
    MKL_LONG    step_x;
    MKL_LONG    step_y;
    MKL_LONG    j;

    first_index = strides[0];
    step_x      = strides[1];
    step_y      = strides[2];

    x_first += first_index;
    x_second = x_first + step_y;
    x_third  = x_first + 2*step_y;
    x_last2  = x_first + (n-3)*step_y;
    x_last1  = x_first + (n-2)*step_y;
    x_last0  = x_first + (n-1)*step_y;

    for (j = 0; j < m; j++) {
        printf(" %8.3f    ", x_first [j*step_x]);
        printf(" %8.3f    ", x_second[j*step_x]);
        printf(" %8.3f    ", x_third [j*step_x]);
        printf(" ... ");
        printf(" %8.3f    ", x_last2[j*step_x]);
        printf(" %8.3f    ", x_last1[j*step_x]);
        printf(" %8.3f  \n", x_last0[j*step_x]);
    }/* for */

    return 0;
}/* print_three_first_and_three_last_columns_s */

int print_three_2d_columns_d(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG* strides)
{
    double* x_first = x;
    double* x_second;
    double* x_third;

    MKL_LONG    first_index;
    MKL_LONG    step_x;
    MKL_LONG    step_y;
    MKL_LONG    j;

    first_index = strides[0];
    step_x      = strides[1];
    step_y      = strides[2];

    x_first += first_index;
    x_second = x_first + step_y;
    x_third  = x_first + 2*step_y;

    for (j = 0; j < m; j++) {
        printf(" %8.3f    ", x_first [j*step_x]);
        printf(" %8.3f    ", x_second[j*step_x]);
        printf(" %8.3f  \n", x_third [j*step_x]);
    }/* for */

    return 0;
}/* print_three_2d_columns_d */

int print_three_2d_columns_s(void* x, MKL_LONG m, MKL_LONG n, MKL_LONG* strides)
{
    float*  x_first = x;
    float*  x_second;
    float*  x_third;

    MKL_LONG    first_index;
    MKL_LONG    step_x;
    MKL_LONG    step_y;
    MKL_LONG    j;

    first_index = strides[0];
    step_x      = strides[1];
    step_y      = strides[2];

    x_first += first_index;
    x_second = x_first + step_y;
    x_third  = x_first + 2*step_y;

    for (j = 0; j < m; j++) {
        printf(" %8.3f    ", x_first [j*step_x]);
        printf(" %8.3f    ", x_second[j*step_x]);
        printf(" %8.3f  \n", x_third [j*step_x]);
    }/* for */

    return 0;
}/* print_three_2d_columns_s */

int print_three_2d_columns_c(void* x, MKL_LONG n, MKL_LONG* strides)
{
    float*  x_first = x;
    float*  x_second;
    float*  x_third;
    MKL_LONG    first_index, step_1d, step_2d;
    MKL_LONG    j;

    first_index = 2*strides[0];
    step_1d     = 2*strides[1];
    step_2d     = 2*strides[2];

    x_first += first_index;
    x_second = x_first + step_2d;
    x_third  = x_first + 2*step_2d;

    for (j = 0; j < n; j++) {
        printf(" (%8.3f, %8.3f) ",  x_first[j*step_1d],  x_first[j*step_1d+1]);
        printf(" (%8.3f, %8.3f) ",  x_second[j*step_1d], x_second[j*step_1d+1]);
        printf(" (%8.3f, %8.3f)\n", x_third[j*step_1d],  x_third[j*step_1d+1]);
    }/* for */

    return 0;
}/* print_three_columns_c */

int print_three_2d_columns_z(void* x, MKL_LONG n, MKL_LONG* strides)
{
    double* x_first = x;
    double* x_second;
    double* x_third;
    MKL_LONG    first_index, step_1d, step_2d;
    MKL_LONG    j;

    first_index = 2*strides[0];
    step_1d     = 2*strides[1];
    step_2d     = 2*strides[2];

    x_first += first_index;
    x_second = x_first + step_2d;
    x_third  = x_first + 2*step_2d;

    for (j = 0; j < n; j++) {
        printf(" (%8.3f, %8.3f) ",  x_first[j*step_1d],  x_first[j*step_1d+1]);
        printf(" (%8.3f, %8.3f) ",  x_second[j*step_1d], x_second[j*step_1d+1]);
        printf(" (%8.3f, %8.3f)\n", x_third[j*step_1d],  x_third[j*step_1d+1]);
    }/* for */

    return 0;
}/* print_three_columns_z */


 /*
/   Check result
*/

float check_result_c(void* x, void* res_exp, MKL_LONG n)
{
    float* x_in = x;
    float* x_exp  = res_exp;

    float  maxerr, d;
    MKL_LONG     i;

    maxerr = 0;
    for (i = 0; i < n; i++){
        d = x_exp[2*i]   - x_in[2*i];   if (d < 0) d = -d; if (d > maxerr) maxerr = d;
        d = x_exp[2*i+1] - x_in[2*i+1]; if (d < 0) d = -d; if (d > maxerr) maxerr = d;
    }/* for i */

    return maxerr;
}/* check_result_c */


double check_result_z(void* x, void* res_exp, MKL_LONG n)
{
    double* x_in = x;
    double* x_exp  = res_exp;

    double  maxerr, d;
    MKL_LONG     i;

    maxerr = 0;
    for (i = 0; i < n; i++){
        d = x_exp[2*i]   - x_in[2*i];   if (d < 0) d = -d; if (d > maxerr) maxerr = d;
        d = x_exp[2*i+1] - x_in[2*i+1]; if (d < 0) d = -d; if (d > maxerr) maxerr = d;
    }/* for i */

    return maxerr;
}/* check_result_z */

float check_result_s(void* x, void* y, MKL_LONG n)
{
    float*  x_in  = x;
    float*  x_exp = y;
    float   maxerr, d;
    MKL_LONG    i;

    maxerr = 0;
    for (i = 0; i < n; i++){
        d = x_exp[i] - x_in[i];
        if (d < 0) d = -d; if (d > maxerr) maxerr = d;
    }
    return maxerr;
}/* check_result_s */

double check_result_d(void* x, void* y, MKL_LONG n)
{
    double* x_in = x;
    double*  x_exp = y;
    double  maxerr, d;
    MKL_LONG    i;

    maxerr = 0;
    for (i = 0; i < n; i++){
        d = x_exp[i] - x_in[i];
        if (d < 0) d = -d; if (d > maxerr) maxerr = d;
    }
    return maxerr;
}/* check_result_d */

double check_result_2d_d(void* x, void* y, MKL_LONG m, MKL_LONG n, MKL_LONG* strides)
{
    double* x_in  = x;
    double* x_exp = y;
    double  step;
    MKL_LONG    i, j;
    MKL_LONG    first_index, step_1d, step_2d;

    double  maxerr, d;

    maxerr = 0;
    first_index = strides[0];
    step_1d     = strides[1];
    step_2d     = strides[2];

    x_in  += first_index;
    x_exp += first_index;

    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++) {
            d = x_exp[j*step_1d] - x_in[j*step_1d];
            if (d < 0) d = -d; if (d > maxerr) maxerr = d;
        }
        x_in  += step_2d;
        x_exp += step_2d;
    }/* for i */
    return maxerr;
}/* check_result_2d_d */

float check_result_2d_s(void* x, void* y, MKL_LONG m, MKL_LONG n, MKL_LONG* strides)
{
    float*  x_in  = x;
    float*  x_exp = y;
    float   step;
    MKL_LONG    i, j;
    MKL_LONG    first_index, step_1d, step_2d;

    float   maxerr, d;

    maxerr = 0;
    first_index = strides[0];
    step_1d     = strides[1];
    step_2d     = strides[2];

    x_in += first_index;
    x_exp += first_index;

    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++) {
            d = x_exp[j*step_1d] - x_in[j*step_1d];
            if (d < 0) d = -d; if (d > maxerr) maxerr = d;
        }
        x_in  += step_2d;
        x_exp += step_2d;
    }/* for i */
    return maxerr;
}/* check_result_2d_s */

double check_result_mult_2d_d(void* x, void* y, MKL_LONG m, MKL_LONG n,
                               MKL_LONG mult, MKL_LONG dist, MKL_LONG* strides)
{
    double* x_in;
    double* x_exp;
    double  step;
    MKL_LONG    i, j, k;
    MKL_LONG    first_index, step_1d, step_2d;

    double  maxerr, d;

    maxerr = 0;
    first_index = strides[0];
    step_1d     = strides[1];
    step_2d     = strides[2];

    for (k = 0; k < mult; k++) {

        x_in  = (double*)x + k*dist + first_index;
        x_exp = (double*)y + k*dist + first_index;
        for (i = 0; i < n; i++) {
            for (j = 0; j < m; j++) {
                d = x_exp[j*step_1d] - x_in[j*step_1d];
                if (d < 0) d = -d; if (d > maxerr) maxerr = d;
            }
            x_in  += step_2d;
            x_exp += step_2d;
        }/* for i */
    }/* for k */
    return maxerr;
}/* check_result_mult_2d_d */

float check_result_mult_2d_s(void* x, void* y, MKL_LONG m, MKL_LONG n,
                               MKL_LONG mult, MKL_LONG dist, MKL_LONG* strides)
{
    float* x_in;
    float* x_exp;
    float  step;
    MKL_LONG    i, j, k;
    MKL_LONG    first_index, step_1d, step_2d;

    float  maxerr, d;

    maxerr = 0;
    first_index = strides[0];
    step_1d     = strides[1];
    step_2d     = strides[2];

    for (k = 0; k < mult; k++) {

        x_in  = (float*)x + k*dist + first_index;
        x_exp = (float*)y + k*dist + first_index;
        for (i = 0; i < n; i++) {
            for (j = 0; j < m; j++) {
                d = x_exp[j*step_1d] - x_in[j*step_1d];
                if (d < 0) d = -d; if (d > maxerr) maxerr = d;
            }
            x_in  += step_2d;
            x_exp += step_2d;
        }/* for i */
    }/* for k */
    return maxerr;
}/* check_result_mult_2d_s */

