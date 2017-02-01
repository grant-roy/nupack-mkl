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
!   header      : file for fftw2_mkl.lib library with wrappers
!                 from this function to DFTI MKL computation functions
!   Content     : plan structures for float precision data,
!                 for double precision data and long double precision data
!*******************************************************************************
*/
#ifndef FFTW2_F77_MKL_H
#define FFTW2_F77_MKL_H

#ifdef _GNU
#define fftw_f77_create_plan fftw_f77_create_plan__
#define fftw_f77_one fftw_f77_one__
#define fftw_f77 fftw_f77__
#define fftw_f77_destroy_plan fftw_f77_destroy_plan__
#define fftwnd_f77_create_plan fftwnd_f77_create_plan__
#define fftw2d_f77_create_plan fftw2d_f77_create_plan__
#define fftw3d_f77_create_plan fftw3d_f77_create_plan__
#define fftwnd_f77 fftwnd_f77__
#define fftwnd_f77_one fftwnd_f77_one__
#define fftwnd_f77_destroy_plan fftwnd_f77_destroy_plan__
#define fftw_f77_threads_one fftw_f77_threads_one__
#define fftw_f77_threads fftw_f77_threads__
#define fftwnd_f77_threads fftwnd_f77_threads__
#define fftwnd_f77_threads_one fftwnd_f77_threads_one__
#define fftw_f77_threads_init fftw_f77_threads_init__

#define rfftw_f77_create_plan rfftw_f77_create_plan__
#define rfftw_f77_one rfftw_f77_one__
#define rfftw_f77 rfftw_f77__
#define rfftw_f77_destroy_plan rfftw_f77_destroy_plan__
#define rfftwnd_f77_create_plan rfftwnd_f77_create_plan__
#define rfftw2d_f77_create_plan rfftw2d_f77_create_plan__
#define rfftw3d_f77_create_plan rfftw3d_f77_create_plan__
#define rfftwnd_f77_real_to_complex rfftwnd_f77_real_to_complex__
#define rfftwnd_f77_one_real_to_complex rfftwnd_f77_one_real_to_complex__
#define rfftwnd_f77_complex_to_real rfftwnd_f77_complex_to_real__
#define rfftwnd_f77_one_complex_to_real rfftwnd_f77_one_complex_to_real__
#define rfftwnd_f77_destroy_plan rfftwnd_f77_destroy_plan__

#define fftw_f77_threads_one fftw_f77_threads_one__
#define fftw_f77_threads fftw_f77_threads__
#define fftwnd_f77_threads fftwnd_f77_threads__
#define fftwnd_f77_threads_one fftwnd_f77_threads_one__
#define rfftwnd_f77_threads_real_to_complex rfftwnd_f77_threads_real_to_complex__
#define rfftwnd_f77_threads_one_real_to_complex rfftwnd_f77_threads_one_real_to_complex__
#define rfftwnd_f77_threads_complex_to_real rfftwnd_f77_threads_complex_to_real__
#define rfftwnd_f77_threads_one_complex_to_real rfftwnd_f77_threads_one_complex_to_real__
#else
#define fftw_f77_create_plan fftw_f77_create_plan_
#define fftw_f77_one fftw_f77_one_
#define fftw_f77 fftw_f77_
#define fftw_f77_destroy_plan fftw_f77_destroy_plan_
#define fftwnd_f77_create_plan fftwnd_f77_create_plan_
#define fftw2d_f77_create_plan fftw2d_f77_create_plan_
#define fftw3d_f77_create_plan fftw3d_f77_create_plan_
#define fftwnd_f77 fftwnd_f77_
#define fftwnd_f77_one fftwnd_f77_one_
#define fftwnd_f77_destroy_plan fftwnd_f77_destroy_plan_
#define fftw_f77_threads_one fftw_f77_threads_one_
#define fftw_f77_threads fftw_f77_threads_
#define fftwnd_f77_threads fftwnd_f77_threads_
#define fftwnd_f77_threads_one fftwnd_f77_threads_one_
#define fftw_f77_threads_init fftw_f77_threads_init_

#define rfftw_f77_create_plan rfftw_f77_create_plan_
#define rfftw_f77_one rfftw_f77_one_
#define rfftw_f77 rfftw_f77_
#define rfftw_f77_destroy_plan rfftw_f77_destroy_plan_
#define rfftwnd_f77_create_plan rfftwnd_f77_create_plan_
#define rfftw2d_f77_create_plan rfftw2d_f77_create_plan_
#define rfftw3d_f77_create_plan rfftw3d_f77_create_plan_
#define rfftwnd_f77_real_to_complex rfftwnd_f77_real_to_complex_
#define rfftwnd_f77_one_real_to_complex rfftwnd_f77_one_real_to_complex_
#define rfftwnd_f77_complex_to_real rfftwnd_f77_complex_to_real_
#define rfftwnd_f77_one_complex_to_real rfftwnd_f77_one_complex_to_real_
#define rfftwnd_f77_destroy_plan rfftwnd_f77_destroy_plan_

#define fftw_f77_threads_one fftw_f77_threads_one_
#define fftw_f77_threads fftw_f77_threads_
#define fftwnd_f77_threads fftwnd_f77_threads_
#define fftwnd_f77_threads_one fftwnd_f77_threads_one_
#define rfftwnd_f77_threads_real_to_complex rfftwnd_f77_threads_real_to_complex_
#define rfftwnd_f77_threads_one_real_to_complex rfftwnd_f77_threads_one_real_to_complex_
#define rfftwnd_f77_threads_complex_to_real rfftwnd_f77_threads_complex_to_real_
#define rfftwnd_f77_threads_one_complex_to_real rfftwnd_f77_threads_one_complex_to_real_
#endif

#endif /* FFTW2_F77_MKL_H */
