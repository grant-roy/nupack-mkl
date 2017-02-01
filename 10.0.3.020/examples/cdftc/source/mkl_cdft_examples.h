/*******************************************************************************
!                              INTEL CONFIDENTIAL
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
!       MKL Cluster DFT example's definitions file (C-interface)
!
!*******************************************************************************
*/

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
/  MKL test _Complex type definition
*/
typedef struct {
    float re;
    float im;
} mkl_float_complex;

typedef struct {
    double re;
    double im;
} mkl_double_complex;

/*
/  Example support function's interfaces
*/
int read_data_file_2d(int, char*[], long*, long*);

void dfti_example_status_print(long);

void print_data_2d_z(void*, long, long, long);

void init_data_2d_z(void*, long, long);

double check_result_z(void*, void*, long);

void print_data_2d_c(void*, long, long, long);

void init_data_2d_c(void*, long, long);

float check_result_c(void*, void*, long);

int MKL_CDFT_Data(MPI_Comm Comm,int RootRank,int ElementSize,long Dim,long Lengths[],void *global,long nx,long start_x,void *local,int Flag);

int MKL_CDFT_ScatterData(MPI_Comm Comm,int RootRank,int ElementSize,long Dim,long Lengths[],void *global_in,long nx,long start_x,void *local_in);

int MKL_CDFT_GatherData(MPI_Comm Comm,int RootRank,int ElementSize,long Dim,long Lengths[],void *global_out,long nx,long start_x,void *local_out);
