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

#ifdef _GNU

#define dfftw_cleanup                      dfftw_cleanup__
#define dfftw_cleanup_threads              dfftw_cleanup_threads__
#define dfftw_destroy_plan                 dfftw_destroy_plan__
#define dfftw_execute                      dfftw_execute__
#define dfftw_execute_dft                  dfftw_execute_dft__
#define dfftw_execute_dft_c2r              dfftw_execute_dft_c2r__
#define dfftw_execute_dft_r2c              dfftw_execute_dft_r2c__
#define dfftw_execute_r2r                  dfftw_execute_r2r__
#define dfftw_execute_split_dft            dfftw_execute_split_dft__
#define dfftw_execute_split_dft_c2r        dfftw_execute_split_dft_c2r__
#define dfftw_execute_split_dft_r2c        dfftw_execute_split_dft_r2c__
#define dfftw_export_wisdom                dfftw_export_wisdom__
#define dfftw_flops                        dfftw_flops__
#define dfftw_forget_wisdom                dfftw_forget_wisdom__
#define dfftw_import_system_wisdom         dfftw_import_system_wisdom__
#define dfftw_import_wisdom                dfftw_import_wisdom__
#define dfftw_init_threads                 dfftw_init_threads__
#define dfftw_plan_dft                     dfftw_plan_dft__
#define dfftw_plan_dft_1d                  dfftw_plan_dft_1d__
#define dfftw_plan_dft_2d                  dfftw_plan_dft_2d__
#define dfftw_plan_dft_3d                  dfftw_plan_dft_3d__
#define dfftw_plan_dft_c2r                 dfftw_plan_dft_c2r__
#define dfftw_plan_dft_c2r_1d              dfftw_plan_dft_c2r_1d__
#define dfftw_plan_dft_c2r_2d              dfftw_plan_dft_c2r_2d__
#define dfftw_plan_dft_c2r_3d              dfftw_plan_dft_c2r_3d__
#define dfftw_plan_dft_r2c                 dfftw_plan_dft_r2c__
#define dfftw_plan_dft_r2c_1d              dfftw_plan_dft_r2c_1d__
#define dfftw_plan_dft_r2c_2d              dfftw_plan_dft_r2c_2d__
#define dfftw_plan_dft_r2c_3d              dfftw_plan_dft_r2c_3d__
#define dfftw_plan_guru_dft                dfftw_plan_guru_dft__
#define dfftw_plan_guru_dft_c2r            dfftw_plan_guru_dft_c2r__
#define dfftw_plan_guru_dft_r2c            dfftw_plan_guru_dft_r2c__
#define dfftw_plan_guru_r2r                dfftw_plan_guru_r2r__
#define dfftw_plan_guru_split_dft          dfftw_plan_guru_split_dft__
#define dfftw_plan_guru_split_dft_c2r      dfftw_plan_guru_split_dft_c2r__
#define dfftw_plan_guru_split_dft_r2c      dfftw_plan_guru_split_dft_r2c__
#define dfftw_plan_many_dft                dfftw_plan_many_dft__
#define dfftw_plan_many_dft_c2r            dfftw_plan_many_dft_c2r__
#define dfftw_plan_many_dft_r2c            dfftw_plan_many_dft_r2c__
#define dfftw_plan_many_r2r                dfftw_plan_many_r2r__
#define dfftw_plan_r2r                     dfftw_plan_r2r__
#define dfftw_plan_r2r_1d                  dfftw_plan_r2r_1d__
#define dfftw_plan_r2r_2d                  dfftw_plan_r2r_2d__
#define dfftw_plan_r2r_3d                  dfftw_plan_r2r_3d__
#define dfftw_plan_with_nthreads           dfftw_plan_with_nthreads__
#define dfftw_print_plan                   dfftw_print_plan__
#define sfftw_cleanup                      sfftw_cleanup__
#define sfftw_cleanup_threads              sfftw_cleanup_threads__
#define sfftw_destroy_plan                 sfftw_destroy_plan__
#define sfftw_execute                      sfftw_execute__
#define sfftw_execute_dft                  sfftw_execute_dft__
#define sfftw_execute_dft_c2r              sfftw_execute_dft_c2r__
#define sfftw_execute_dft_r2c              sfftw_execute_dft_r2c__
#define sfftw_execute_r2r                  sfftw_execute_r2r__
#define sfftw_execute_split_dft            sfftw_execute_split_dft__
#define sfftw_execute_split_dft_c2r        sfftw_execute_split_dft_c2r__
#define sfftw_execute_split_dft_r2c        sfftw_execute_split_dft_r2c__
#define sfftw_export_wisdom                sfftw_export_wisdom__
#define sfftw_flops                        sfftw_flops__
#define sfftw_forget_wisdom                sfftw_forget_wisdom__
#define sfftw_import_system_wisdom         sfftw_import_system_wisdom__
#define sfftw_import_wisdom                sfftw_import_wisdom__
#define sfftw_init_threads                 sfftw_init_threads__
#define sfftw_plan_dft                     sfftw_plan_dft__
#define sfftw_plan_dft_1d                  sfftw_plan_dft_1d__
#define sfftw_plan_dft_2d                  sfftw_plan_dft_2d__
#define sfftw_plan_dft_3d                  sfftw_plan_dft_3d__
#define sfftw_plan_dft_c2r                 sfftw_plan_dft_c2r__
#define sfftw_plan_dft_c2r_1d              sfftw_plan_dft_c2r_1d__
#define sfftw_plan_dft_c2r_2d              sfftw_plan_dft_c2r_2d__
#define sfftw_plan_dft_c2r_3d              sfftw_plan_dft_c2r_3d__
#define sfftw_plan_dft_r2c                 sfftw_plan_dft_r2c__
#define sfftw_plan_dft_r2c_1d              sfftw_plan_dft_r2c_1d__
#define sfftw_plan_dft_r2c_2d              sfftw_plan_dft_r2c_2d__
#define sfftw_plan_dft_r2c_3d              sfftw_plan_dft_r2c_3d__
#define sfftw_plan_guru_dft                sfftw_plan_guru_dft__
#define sfftw_plan_guru_dft_c2r            sfftw_plan_guru_dft_c2r__
#define sfftw_plan_guru_dft_r2c            sfftw_plan_guru_dft_r2c__
#define sfftw_plan_guru_r2r                sfftw_plan_guru_r2r__
#define sfftw_plan_guru_split_dft          sfftw_plan_guru_split_dft__
#define sfftw_plan_guru_split_dft_c2r      sfftw_plan_guru_split_dft_c2r__
#define sfftw_plan_guru_split_dft_r2c      sfftw_plan_guru_split_dft_r2c__
#define sfftw_plan_many_dft                sfftw_plan_many_dft__
#define sfftw_plan_many_dft_c2r            sfftw_plan_many_dft_c2r__
#define sfftw_plan_many_dft_r2c            sfftw_plan_many_dft_r2c__
#define sfftw_plan_many_r2r                sfftw_plan_many_r2r__
#define sfftw_plan_r2r                     sfftw_plan_r2r__
#define sfftw_plan_r2r_1d                  sfftw_plan_r2r_1d__
#define sfftw_plan_r2r_2d                  sfftw_plan_r2r_2d__
#define sfftw_plan_r2r_3d                  sfftw_plan_r2r_3d__
#define sfftw_plan_with_nthreads           sfftw_plan_with_nthreads__
#define sfftw_print_plan                   sfftw_print_plan__
#define lfftw_cleanup                      lfftw_cleanup__
#define lfftw_cleanup_threads              lfftw_cleanup_threads__
#define lfftw_destroy_plan                 lfftw_destroy_plan__
#define lfftw_execute                      lfftw_execute__
#define lfftw_execute_dft                  lfftw_execute_dft__
#define lfftw_execute_dft_c2r              lfftw_execute_dft_c2r__
#define lfftw_execute_dft_r2c              lfftw_execute_dft_r2c__
#define lfftw_execute_r2r                  lfftw_execute_r2r__
#define lfftw_execute_split_dft            lfftw_execute_split_dft__
#define lfftw_execute_split_dft_c2r        lfftw_execute_split_dft_c2r__
#define lfftw_execute_split_dft_r2c        lfftw_execute_split_dft_r2c__
#define lfftw_export_wisdom                lfftw_export_wisdom__
#define lfftw_flops                        lfftw_flops__
#define lfftw_forget_wisdom                lfftw_forget_wisdom__
#define lfftw_import_system_wisdom         lfftw_import_system_wisdom__
#define lfftw_import_wisdom                lfftw_import_wisdom__
#define lfftw_init_threads                 lfftw_init_threads__
#define lfftw_plan_dft                     lfftw_plan_dft__
#define lfftw_plan_dft_1d                  lfftw_plan_dft_1d__
#define lfftw_plan_dft_2d                  lfftw_plan_dft_2d__
#define lfftw_plan_dft_3d                  lfftw_plan_dft_3d__
#define lfftw_plan_dft_c2r                 lfftw_plan_dft_c2r__
#define lfftw_plan_dft_c2r_1d              lfftw_plan_dft_c2r_1d__
#define lfftw_plan_dft_c2r_2d              lfftw_plan_dft_c2r_2d__
#define lfftw_plan_dft_c2r_3d              lfftw_plan_dft_c2r_3d__
#define lfftw_plan_dft_r2c                 lfftw_plan_dft_r2c__
#define lfftw_plan_dft_r2c_1d              lfftw_plan_dft_r2c_1d__
#define lfftw_plan_dft_r2c_2d              lfftw_plan_dft_r2c_2d__
#define lfftw_plan_dft_r2c_3d              lfftw_plan_dft_r2c_3d__
#define lfftw_plan_guru_dft                lfftw_plan_guru_dft__
#define lfftw_plan_guru_dft_c2r            lfftw_plan_guru_dft_c2r__
#define lfftw_plan_guru_dft_r2c            lfftw_plan_guru_dft_r2c__
#define lfftw_plan_guru_r2r                lfftw_plan_guru_r2r__
#define lfftw_plan_guru_split_dft          lfftw_plan_guru_split_dft__
#define lfftw_plan_guru_split_dft_c2r      lfftw_plan_guru_split_dft_c2r__
#define lfftw_plan_guru_split_dft_r2c      lfftw_plan_guru_split_dft_r2c__
#define lfftw_plan_many_dft                lfftw_plan_many_dft__
#define lfftw_plan_many_dft_c2r            lfftw_plan_many_dft_c2r__
#define lfftw_plan_many_dft_r2c            lfftw_plan_many_dft_r2c__
#define lfftw_plan_many_r2r                lfftw_plan_many_r2r__
#define lfftw_plan_r2r                     lfftw_plan_r2r__
#define lfftw_plan_r2r_1d                  lfftw_plan_r2r_1d__
#define lfftw_plan_r2r_2d                  lfftw_plan_r2r_2d__
#define lfftw_plan_r2r_3d                  lfftw_plan_r2r_3d__
#define lfftw_plan_with_nthreads           lfftw_plan_with_nthreads__
#define lfftw_print_plan                   lfftw_print_plan__

#else

#define dfftw_cleanup                      dfftw_cleanup_
#define dfftw_cleanup_threads              dfftw_cleanup_threads_
#define dfftw_destroy_plan                 dfftw_destroy_plan_
#define dfftw_execute                      dfftw_execute_
#define dfftw_execute_dft                  dfftw_execute_dft_
#define dfftw_execute_dft_c2r              dfftw_execute_dft_c2r_
#define dfftw_execute_dft_r2c              dfftw_execute_dft_r2c_
#define dfftw_execute_r2r                  dfftw_execute_r2r_
#define dfftw_execute_split_dft            dfftw_execute_split_dft_
#define dfftw_execute_split_dft_c2r        dfftw_execute_split_dft_c2r_
#define dfftw_execute_split_dft_r2c        dfftw_execute_split_dft_r2c_
#define dfftw_export_wisdom                dfftw_export_wisdom_
#define dfftw_flops                        dfftw_flops_
#define dfftw_forget_wisdom                dfftw_forget_wisdom_
#define dfftw_import_system_wisdom         dfftw_import_system_wisdom_
#define dfftw_import_wisdom                dfftw_import_wisdom_
#define dfftw_init_threads                 dfftw_init_threads_
#define dfftw_plan_dft                     dfftw_plan_dft_
#define dfftw_plan_dft_1d                  dfftw_plan_dft_1d_
#define dfftw_plan_dft_2d                  dfftw_plan_dft_2d_
#define dfftw_plan_dft_3d                  dfftw_plan_dft_3d_
#define dfftw_plan_dft_c2r                 dfftw_plan_dft_c2r_
#define dfftw_plan_dft_c2r_1d              dfftw_plan_dft_c2r_1d_
#define dfftw_plan_dft_c2r_2d              dfftw_plan_dft_c2r_2d_
#define dfftw_plan_dft_c2r_3d              dfftw_plan_dft_c2r_3d_
#define dfftw_plan_dft_r2c                 dfftw_plan_dft_r2c_
#define dfftw_plan_dft_r2c_1d              dfftw_plan_dft_r2c_1d_
#define dfftw_plan_dft_r2c_2d              dfftw_plan_dft_r2c_2d_
#define dfftw_plan_dft_r2c_3d              dfftw_plan_dft_r2c_3d_
#define dfftw_plan_guru_dft                dfftw_plan_guru_dft_
#define dfftw_plan_guru_dft_c2r            dfftw_plan_guru_dft_c2r_
#define dfftw_plan_guru_dft_r2c            dfftw_plan_guru_dft_r2c_
#define dfftw_plan_guru_r2r                dfftw_plan_guru_r2r_
#define dfftw_plan_guru_split_dft          dfftw_plan_guru_split_dft_
#define dfftw_plan_guru_split_dft_c2r      dfftw_plan_guru_split_dft_c2r_
#define dfftw_plan_guru_split_dft_r2c      dfftw_plan_guru_split_dft_r2c_
#define dfftw_plan_many_dft                dfftw_plan_many_dft_
#define dfftw_plan_many_dft_c2r            dfftw_plan_many_dft_c2r_
#define dfftw_plan_many_dft_r2c            dfftw_plan_many_dft_r2c_
#define dfftw_plan_many_r2r                dfftw_plan_many_r2r_
#define dfftw_plan_r2r                     dfftw_plan_r2r_
#define dfftw_plan_r2r_1d                  dfftw_plan_r2r_1d_
#define dfftw_plan_r2r_2d                  dfftw_plan_r2r_2d_
#define dfftw_plan_r2r_3d                  dfftw_plan_r2r_3d_
#define dfftw_plan_with_nthreads           dfftw_plan_with_nthreads_
#define dfftw_print_plan                   dfftw_print_plan_
#define sfftw_cleanup                      sfftw_cleanup_
#define sfftw_cleanup_threads              sfftw_cleanup_threads_
#define sfftw_destroy_plan                 sfftw_destroy_plan_
#define sfftw_execute                      sfftw_execute_
#define sfftw_execute_dft                  sfftw_execute_dft_
#define sfftw_execute_dft_c2r              sfftw_execute_dft_c2r_
#define sfftw_execute_dft_r2c              sfftw_execute_dft_r2c_
#define sfftw_execute_r2r                  sfftw_execute_r2r_
#define sfftw_execute_split_dft            sfftw_execute_split_dft_
#define sfftw_execute_split_dft_c2r        sfftw_execute_split_dft_c2r_
#define sfftw_execute_split_dft_r2c        sfftw_execute_split_dft_r2c_
#define sfftw_export_wisdom                sfftw_export_wisdom_
#define sfftw_flops                        sfftw_flops_
#define sfftw_forget_wisdom                sfftw_forget_wisdom_
#define sfftw_import_system_wisdom         sfftw_import_system_wisdom_
#define sfftw_import_wisdom                sfftw_import_wisdom_
#define sfftw_init_threads                 sfftw_init_threads_
#define sfftw_plan_dft                     sfftw_plan_dft_
#define sfftw_plan_dft_1d                  sfftw_plan_dft_1d_
#define sfftw_plan_dft_2d                  sfftw_plan_dft_2d_
#define sfftw_plan_dft_3d                  sfftw_plan_dft_3d_
#define sfftw_plan_dft_c2r                 sfftw_plan_dft_c2r_
#define sfftw_plan_dft_c2r_1d              sfftw_plan_dft_c2r_1d_
#define sfftw_plan_dft_c2r_2d              sfftw_plan_dft_c2r_2d_
#define sfftw_plan_dft_c2r_3d              sfftw_plan_dft_c2r_3d_
#define sfftw_plan_dft_r2c                 sfftw_plan_dft_r2c_
#define sfftw_plan_dft_r2c_1d              sfftw_plan_dft_r2c_1d_
#define sfftw_plan_dft_r2c_2d              sfftw_plan_dft_r2c_2d_
#define sfftw_plan_dft_r2c_3d              sfftw_plan_dft_r2c_3d_
#define sfftw_plan_guru_dft                sfftw_plan_guru_dft_
#define sfftw_plan_guru_dft_c2r            sfftw_plan_guru_dft_c2r_
#define sfftw_plan_guru_dft_r2c            sfftw_plan_guru_dft_r2c_
#define sfftw_plan_guru_r2r                sfftw_plan_guru_r2r_
#define sfftw_plan_guru_split_dft          sfftw_plan_guru_split_dft_
#define sfftw_plan_guru_split_dft_c2r      sfftw_plan_guru_split_dft_c2r_
#define sfftw_plan_guru_split_dft_r2c      sfftw_plan_guru_split_dft_r2c_
#define sfftw_plan_many_dft                sfftw_plan_many_dft_
#define sfftw_plan_many_dft_c2r            sfftw_plan_many_dft_c2r_
#define sfftw_plan_many_dft_r2c            sfftw_plan_many_dft_r2c_
#define sfftw_plan_many_r2r                sfftw_plan_many_r2r_
#define sfftw_plan_r2r                     sfftw_plan_r2r_
#define sfftw_plan_r2r_1d                  sfftw_plan_r2r_1d_
#define sfftw_plan_r2r_2d                  sfftw_plan_r2r_2d_
#define sfftw_plan_r2r_3d                  sfftw_plan_r2r_3d_
#define sfftw_plan_with_nthreads           sfftw_plan_with_nthreads_
#define sfftw_print_plan                   sfftw_print_plan_
#define lfftw_cleanup                      lfftw_cleanup_
#define lfftw_cleanup_threads              lfftw_cleanup_threads_
#define lfftw_destroy_plan                 lfftw_destroy_plan_
#define lfftw_execute                      lfftw_execute_
#define lfftw_execute_dft                  lfftw_execute_dft_
#define lfftw_execute_dft_c2r              lfftw_execute_dft_c2r_
#define lfftw_execute_dft_r2c              lfftw_execute_dft_r2c_
#define lfftw_execute_r2r                  lfftw_execute_r2r_
#define lfftw_execute_split_dft            lfftw_execute_split_dft_
#define lfftw_execute_split_dft_c2r        lfftw_execute_split_dft_c2r_
#define lfftw_execute_split_dft_r2c        lfftw_execute_split_dft_r2c_
#define lfftw_export_wisdom                lfftw_export_wisdom_
#define lfftw_flops                        lfftw_flops_
#define lfftw_forget_wisdom                lfftw_forget_wisdom_
#define lfftw_import_system_wisdom         lfftw_import_system_wisdom_
#define lfftw_import_wisdom                lfftw_import_wisdom_
#define lfftw_init_threads                 lfftw_init_threads_
#define lfftw_plan_dft                     lfftw_plan_dft_
#define lfftw_plan_dft_1d                  lfftw_plan_dft_1d_
#define lfftw_plan_dft_2d                  lfftw_plan_dft_2d_
#define lfftw_plan_dft_3d                  lfftw_plan_dft_3d_
#define lfftw_plan_dft_c2r                 lfftw_plan_dft_c2r_
#define lfftw_plan_dft_c2r_1d              lfftw_plan_dft_c2r_1d_
#define lfftw_plan_dft_c2r_2d              lfftw_plan_dft_c2r_2d_
#define lfftw_plan_dft_c2r_3d              lfftw_plan_dft_c2r_3d_
#define lfftw_plan_dft_r2c                 lfftw_plan_dft_r2c_
#define lfftw_plan_dft_r2c_1d              lfftw_plan_dft_r2c_1d_
#define lfftw_plan_dft_r2c_2d              lfftw_plan_dft_r2c_2d_
#define lfftw_plan_dft_r2c_3d              lfftw_plan_dft_r2c_3d_
#define lfftw_plan_guru_dft                lfftw_plan_guru_dft_
#define lfftw_plan_guru_dft_c2r            lfftw_plan_guru_dft_c2r_
#define lfftw_plan_guru_dft_r2c            lfftw_plan_guru_dft_r2c_
#define lfftw_plan_guru_r2r                lfftw_plan_guru_r2r_
#define lfftw_plan_guru_split_dft          lfftw_plan_guru_split_dft_
#define lfftw_plan_guru_split_dft_c2r      lfftw_plan_guru_split_dft_c2r_
#define lfftw_plan_guru_split_dft_r2c      lfftw_plan_guru_split_dft_r2c_
#define lfftw_plan_many_dft                lfftw_plan_many_dft_
#define lfftw_plan_many_dft_c2r            lfftw_plan_many_dft_c2r_
#define lfftw_plan_many_dft_r2c            lfftw_plan_many_dft_r2c_
#define lfftw_plan_many_r2r                lfftw_plan_many_r2r_
#define lfftw_plan_r2r                     lfftw_plan_r2r_
#define lfftw_plan_r2r_1d                  lfftw_plan_r2r_1d_
#define lfftw_plan_r2r_2d                  lfftw_plan_r2r_2d_
#define lfftw_plan_r2r_3d                  lfftw_plan_r2r_3d_
#define lfftw_plan_with_nthreads           lfftw_plan_with_nthreads_
#define lfftw_print_plan                   lfftw_print_plan_

#endif

#endif /* FFTW3_F77_MKL_H */
