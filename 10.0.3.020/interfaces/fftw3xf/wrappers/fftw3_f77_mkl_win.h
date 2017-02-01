/*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 2006-2008 Intel Corporation. All Rights Reserved.
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
!   header      : file for fftw3_mkl.lib library with wrappers
!                 from this function to DFTI MKL computation functions
!   Content     : plan structures for float precision data,
!                 for double precision data and long double precision data
!*******************************************************************************
*/
#ifndef FFTW3_F77_MKL_H
#define FFTW3_F77_MKL_H

#define dfftw_cleanup                      DFFTW_CLEANUP
#define dfftw_cleanup_threads              DFFTW_CLEANUP_THREADS
#define dfftw_destroy_plan                 DFFTW_DESTROY_PLAN
#define dfftw_execute                      DFFTW_EXECUTE
#define dfftw_execute_dft                  DFFTW_EXECUTE_DFT
#define dfftw_execute_dft_c2r              DFFTW_EXECUTE_DFT_C2R
#define dfftw_execute_dft_r2c              DFFTW_EXECUTE_DFT_R2C
#define dfftw_execute_r2r                  DFFTW_EXECUTE_R2R
#define dfftw_execute_split_dft            DFFTW_EXECUTE_SPLIT_DFT
#define dfftw_execute_split_dft_c2r        DFFTW_EXECUTE_SPLIT_DFT_C2R
#define dfftw_execute_split_dft_r2c        DFFTW_EXECUTE_SPLIT_DFT_R2C
#define dfftw_export_wisdom                DFFTW_EXPORT_WISDOM
#define dfftw_flops                        DFFTW_FLOPS
#define dfftw_forget_wisdom                DFFTW_FORGET_WISDOM
#define dfftw_import_system_wisdom         DFFTW_IMPORT_SYSTEM_WISDOM
#define dfftw_import_wisdom                DFFTW_IMPORT_WISDOM
#define dfftw_init_threads                 DFFTW_INIT_THREADS
#define dfftw_plan_dft                     DFFTW_PLAN_DFT
#define dfftw_plan_dft_1d                  DFFTW_PLAN_DFT_1D
#define dfftw_plan_dft_2d                  DFFTW_PLAN_DFT_2D
#define dfftw_plan_dft_3d                  DFFTW_PLAN_DFT_3D
#define dfftw_plan_dft_c2r                 DFFTW_PLAN_DFT_C2R
#define dfftw_plan_dft_c2r_1d              DFFTW_PLAN_DFT_C2R_1D
#define dfftw_plan_dft_c2r_2d              DFFTW_PLAN_DFT_C2R_2D
#define dfftw_plan_dft_c2r_3d              DFFTW_PLAN_DFT_C2R_3D
#define dfftw_plan_dft_r2c                 DFFTW_PLAN_DFT_R2C
#define dfftw_plan_dft_r2c_1d              DFFTW_PLAN_DFT_R2C_1D
#define dfftw_plan_dft_r2c_2d              DFFTW_PLAN_DFT_R2C_2D
#define dfftw_plan_dft_r2c_3d              DFFTW_PLAN_DFT_R2C_3D
#define dfftw_plan_guru_dft                DFFTW_PLAN_GURU_DFT
#define dfftw_plan_guru_dft_c2r            DFFTW_PLAN_GURU_DFT_C2R
#define dfftw_plan_guru_dft_r2c            DFFTW_PLAN_GURU_DFT_R2C
#define dfftw_plan_guru_r2r                DFFTW_PLAN_GURU_R2R
#define dfftw_plan_guru_split_dft          DFFTW_PLAN_GURU_SPLIT_DFT
#define dfftw_plan_guru_split_dft_c2r      DFFTW_PLAN_GURU_SPLIT_DFT_C2R
#define dfftw_plan_guru_split_dft_r2c      DFFTW_PLAN_GURU_SPLIT_DFT_R2C
#define dfftw_plan_many_dft                DFFTW_PLAN_MANY_DFT
#define dfftw_plan_many_dft_c2r            DFFTW_PLAN_MANY_DFT_C2R
#define dfftw_plan_many_dft_r2c            DFFTW_PLAN_MANY_DFT_R2C
#define dfftw_plan_many_r2r                DFFTW_PLAN_MANY_R2R
#define dfftw_plan_r2r                     DFFTW_PLAN_R2R
#define dfftw_plan_r2r_1d                  DFFTW_PLAN_R2R_1D
#define dfftw_plan_r2r_2d                  DFFTW_PLAN_R2R_2D
#define dfftw_plan_r2r_3d                  DFFTW_PLAN_R2R_3D
#define dfftw_plan_with_nthreads           DFFTW_PLAN_WITH_NTHREADS
#define dfftw_print_plan                   DFFTW_PRINT_PLAN
#define sfftw_cleanup                      SFFTW_CLEANUP
#define sfftw_cleanup_threads              SFFTW_CLEANUP_THREADS
#define sfftw_destroy_plan                 SFFTW_DESTROY_PLAN
#define sfftw_execute                      SFFTW_EXECUTE
#define sfftw_execute_dft                  SFFTW_EXECUTE_DFT
#define sfftw_execute_dft_c2r              SFFTW_EXECUTE_DFT_C2R
#define sfftw_execute_dft_r2c              SFFTW_EXECUTE_DFT_R2C
#define sfftw_execute_r2r                  SFFTW_EXECUTE_R2R
#define sfftw_execute_split_dft            SFFTW_EXECUTE_SPLIT_DFT
#define sfftw_execute_split_dft_c2r        SFFTW_EXECUTE_SPLIT_DFT_C2R
#define sfftw_execute_split_dft_r2c        SFFTW_EXECUTE_SPLIT_DFT_R2C
#define sfftw_export_wisdom                SFFTW_EXPORT_WISDOM
#define sfftw_flops                        SFFTW_FLOPS
#define sfftw_forget_wisdom                SFFTW_FORGET_WISDOM
#define sfftw_import_system_wisdom         SFFTW_IMPORT_SYSTEM_WISDOM
#define sfftw_import_wisdom                SFFTW_IMPORT_WISDOM
#define sfftw_init_threads                 SFFTW_INIT_THREADS
#define sfftw_plan_dft                     SFFTW_PLAN_DFT
#define sfftw_plan_dft_1d                  SFFTW_PLAN_DFT_1D
#define sfftw_plan_dft_2d                  SFFTW_PLAN_DFT_2D
#define sfftw_plan_dft_3d                  SFFTW_PLAN_DFT_3D
#define sfftw_plan_dft_c2r                 SFFTW_PLAN_DFT_C2R
#define sfftw_plan_dft_c2r_1d              SFFTW_PLAN_DFT_C2R_1D
#define sfftw_plan_dft_c2r_2d              SFFTW_PLAN_DFT_C2R_2D
#define sfftw_plan_dft_c2r_3d              SFFTW_PLAN_DFT_C2R_3D
#define sfftw_plan_dft_r2c                 SFFTW_PLAN_DFT_R2C
#define sfftw_plan_dft_r2c_1d              SFFTW_PLAN_DFT_R2C_1D
#define sfftw_plan_dft_r2c_2d              SFFTW_PLAN_DFT_R2C_2D
#define sfftw_plan_dft_r2c_3d              SFFTW_PLAN_DFT_R2C_3D
#define sfftw_plan_guru_dft                SFFTW_PLAN_GURU_DFT
#define sfftw_plan_guru_dft_c2r            SFFTW_PLAN_GURU_DFT_C2R
#define sfftw_plan_guru_dft_r2c            SFFTW_PLAN_GURU_DFT_R2C
#define sfftw_plan_guru_r2r                SFFTW_PLAN_GURU_R2R
#define sfftw_plan_guru_split_dft          SFFTW_PLAN_GURU_SPLIT_DFT
#define sfftw_plan_guru_split_dft_c2r      SFFTW_PLAN_GURU_SPLIT_DFT_C2R
#define sfftw_plan_guru_split_dft_r2c      SFFTW_PLAN_GURU_SPLIT_DFT_R2C
#define sfftw_plan_many_dft                SFFTW_PLAN_MANY_DFT
#define sfftw_plan_many_dft_c2r            SFFTW_PLAN_MANY_DFT_C2R
#define sfftw_plan_many_dft_r2c            SFFTW_PLAN_MANY_DFT_R2C
#define sfftw_plan_many_r2r                SFFTW_PLAN_MANY_R2R
#define sfftw_plan_r2r                     SFFTW_PLAN_R2R
#define sfftw_plan_r2r_1d                  SFFTW_PLAN_R2R_1D
#define sfftw_plan_r2r_2d                  SFFTW_PLAN_R2R_2D
#define sfftw_plan_r2r_3d                  SFFTW_PLAN_R2R_3D
#define sfftw_plan_with_nthreads           SFFTW_PLAN_WITH_NTHREADS
#define sfftw_print_plan                   SFFTW_PRINT_PLAN
#define lfftw_cleanup                      LFFTW_CLEANUP
#define lfftw_cleanup_threads              LFFTW_CLEANUP_THREADS
#define lfftw_destroy_plan                 LFFTW_DESTROY_PLAN
#define lfftw_execute                      LFFTW_EXECUTE
#define lfftw_execute_dft                  LFFTW_EXECUTE_DFT
#define lfftw_execute_dft_c2r              LFFTW_EXECUTE_DFT_C2R
#define lfftw_execute_dft_r2c              LFFTW_EXECUTE_DFT_R2C
#define lfftw_execute_r2r                  LFFTW_EXECUTE_R2R
#define lfftw_execute_split_dft            LFFTW_EXECUTE_SPLIT_DFT
#define lfftw_execute_split_dft_c2r        LFFTW_EXECUTE_SPLIT_DFT_C2R
#define lfftw_execute_split_dft_r2c        LFFTW_EXECUTE_SPLIT_DFT_R2C
#define lfftw_export_wisdom                LFFTW_EXPORT_WISDOM
#define lfftw_flops                        LFFTW_FLOPS
#define lfftw_forget_wisdom                LFFTW_FORGET_WISDOM
#define lfftw_import_system_wisdom         LFFTW_IMPORT_SYSTEM_WISDOM
#define lfftw_import_wisdom                LFFTW_IMPORT_WISDOM
#define lfftw_init_threads                 LFFTW_INIT_THREADS
#define lfftw_plan_dft                     LFFTW_PLAN_DFT
#define lfftw_plan_dft_1d                  LFFTW_PLAN_DFT_1D
#define lfftw_plan_dft_2d                  LFFTW_PLAN_DFT_2D
#define lfftw_plan_dft_3d                  LFFTW_PLAN_DFT_3D
#define lfftw_plan_dft_c2r                 LFFTW_PLAN_DFT_C2R
#define lfftw_plan_dft_c2r_1d              LFFTW_PLAN_DFT_C2R_1D
#define lfftw_plan_dft_c2r_2d              LFFTW_PLAN_DFT_C2R_2D
#define lfftw_plan_dft_c2r_3d              LFFTW_PLAN_DFT_C2R_3D
#define lfftw_plan_dft_r2c                 LFFTW_PLAN_DFT_R2C
#define lfftw_plan_dft_r2c_1d              LFFTW_PLAN_DFT_R2C_1D
#define lfftw_plan_dft_r2c_2d              LFFTW_PLAN_DFT_R2C_2D
#define lfftw_plan_dft_r2c_3d              LFFTW_PLAN_DFT_R2C_3D
#define lfftw_plan_guru_dft                LFFTW_PLAN_GURU_DFT
#define lfftw_plan_guru_dft_c2r            LFFTW_PLAN_GURU_DFT_C2R
#define lfftw_plan_guru_dft_r2c            LFFTW_PLAN_GURU_DFT_R2C
#define lfftw_plan_guru_r2r                LFFTW_PLAN_GURU_R2R
#define lfftw_plan_guru_split_dft          LFFTW_PLAN_GURU_SPLIT_DFT
#define lfftw_plan_guru_split_dft_c2r      LFFTW_PLAN_GURU_SPLIT_DFT_C2R
#define lfftw_plan_guru_split_dft_r2c      LFFTW_PLAN_GURU_SPLIT_DFT_R2C
#define lfftw_plan_many_dft                LFFTW_PLAN_MANY_DFT
#define lfftw_plan_many_dft_c2r            LFFTW_PLAN_MANY_DFT_C2R
#define lfftw_plan_many_dft_r2c            LFFTW_PLAN_MANY_DFT_R2C
#define lfftw_plan_many_r2r                LFFTW_PLAN_MANY_R2R
#define lfftw_plan_r2r                     LFFTW_PLAN_R2R
#define lfftw_plan_r2r_1d                  LFFTW_PLAN_R2R_1D
#define lfftw_plan_r2r_2d                  LFFTW_PLAN_R2R_2D
#define lfftw_plan_r2r_3d                  LFFTW_PLAN_R2R_3D
#define lfftw_plan_with_nthreads           LFFTW_PLAN_WITH_NTHREADS
#define lfftw_print_plan                   LFFTW_PRINT_PLAN

#endif /* FFTW3_F77_MKL_H */
