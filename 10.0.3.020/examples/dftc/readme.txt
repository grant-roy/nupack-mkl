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
!       DFTI examples description file
!*******************************************************************************
*/

!*******************************************************************************
!       DFTI examples  macro substitutions
!*******************************************************************************

Header file (mkl_dfti_examples.h for C and mkl_dfti_examples.fi for Fortran)
contains the follow macro substitutions which can be changed to modify
example's features:

/*
/  Static array's size definition
*/
M_MAX - define first  dimension size for static array
N_MAX - define second dimension size for static array
K_MAX - define third  dimension size for static array

/*
/  Print level definition
*/
If macro substitution ADVANCED_DATA_PRINT is egual to logical TRUE
then output file will contain input and uoutput data value.

If macro substitution ACCURACY_PRINT is egual to logical TRUE
then output file will contain calculate accuracy value.

If macro substitution LEGEND_PRINT is egual to logical TRUE
then output file will contain DFTI transform parameters for given example.

/*
/  Accuracy definitions
*/

SINGLE_EPS - expected accuracy value for single precision data
DOUBLE_EPS - expected accuracy value for double precision data

!*******************************************************************************
!       DFTI examples name abbreviation
!*******************************************************************************

{complex/real}_ - complex or real DFT transform
{1/2/3}d_       - 1, 2 or 3 rank of transform
{single/double} - data precision
{ccs/pack/perm} - packed format for real transform

ex{i}, i=0,1...  - example number which specified other features
   odd  number of i - inplace transform for certain feature
   even number of i - not inplace transform for certain feature

!*******************************************************************************
!       Example's set for Complex-to-Complex transform inplace and not inplace
!*******************************************************************************

complex_1d_{single/double}_ex{1/2}  - Forward-Backward 1D transform,
                                      tight data are allocated dynamically to 1D array with any size.

complex_1d_{single/double}_ex{3/4}  - multiple row 1D transform,
                                      data are allocated in static 2D array [M_MAX][N_MAX] untightly.

complex_1d_{single/double}_ex{5/6}  - multiple column 1D transform,
                                      data are allocated in static 2D array [M_MAX][N_MAX]  untightly.

complex_1d_{single/double}_ex{7/8}  - Forward 1D transform accuracy test,
                                      tight data are allocated dynamically to 1D array with any size.

complex_1d_{single/double}_ex{9/10} - Backward 1D transform accuracy test,
                                      tight data are allocated dynamically to 1D array with any size.

complex_2d_{single/double}_ex{1/2}  - Forward-Backward 2D transform,
                                      data are allocated in static 2D array [M_MAX][N_MAX].

complex_2d_{single/double}_ex{3/4}  - multiple 2D transform along first dimension of 3D array,
                                      data are allocated in static 3D array [M_MAX][N_MAX][K_MAX].

complex_2d_{single/double}_ex{5/6}  - multiple 2D transform along second dimension of 3D array,
                                      data are allocated in static 3D array [M_MAX][N_MAX][K_MAX].

complex_2d_{single/double}_ex{7/8}  - multiple 2D transform along third dimension of 3D array,
                                      data are allocated in static 3D array [M_MAX][N_MAX][K_MAX].

complex_3d_{single/double}_ex{1/2}  - Forward-Backward 3D transform, data are allocated
                                      in static 3D array [M_MAX][N_MAX][K_MAX] tightly.

complex_3d_{single/double}_ex{3/4}  - Forward-Backward 3D transform, data are allocated
                                      in static 3D array [M_MAX][N_MAX][K_MAX] untightly.

real_1d_{ccs/perm/pack}_{single/double}_ex{1/2} - Real-t0-Complex and Complex-to-Real 1D transform,
                                      tight data are allocated dynamically to 1D array with any size.

real_1d_{ccs/perm/pack}_{single/double}_ex{3/4} - Real-t0-Complex and Complex-to-Real 1D transform,
                                      data are allocated in static 2D array [M_MAX][N_MAX] tightly.

real_1d_{ccs/perm/pack}_{single/double}_ex{5/6} - Real-t0-Complex and Complex-to-Real 1D transform,
                                      data are allocated in static 2D array [M_MAX][N_MAX] untightly.

real_2d_{ccs/perm/pack}_{single/double}_ex{1/2} - Real-t0-Complex and Complex-to-Real 2D transform,
                                      data are allocated in static 2D array [M_MAX][N_MAX].

real_2d_{ccs/perm/pack}_{single/double}_ex{3/4}  - multiple 2D Real-t0-Complex and Complex-to-Real
				      transform along first dimension of 3D array,
                                      data are allocated in static 3D array [M_MAX][N_MAX][K_MAX].

real_2d_{ccs/perm/pack}_{single/double}_ex{5/6}  - multiple 2D Real-t0-Complex and Complex-to-Real
				      transform along second dimension of 3D array,
                                      data are allocated in static 3D array [M_MAX][N_MAX][K_MAX].

real_2d_{ccs/perm/pack}_{single/double}_ex{7/8}  - multiple 2D Real-t0-Complex and Complex-to-Real
				      transform along third dimension of 3D array,
                                      data are allocated in static 3D array [M_MAX][N_MAX][K_MAX].

