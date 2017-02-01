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

#define fftw_f77_create_plan FFTW_F77_CREATE_PLAN
#define fftw_f77_one FFTW_F77_ONE
#define fftw_f77 FFTW_F77
#define fftw_f77_destroy_plan FFTW_F77_DESTROY_PLAN
#define fftwnd_f77_create_plan FFTWND_F77_CREATE_PLAN
#define fftw2d_f77_create_plan FFTW2D_F77_CREATE_PLAN
#define fftw3d_f77_create_plan FFTW3D_F77_CREATE_PLAN
#define fftwnd_f77 FFTWND_F77
#define fftwnd_f77_one FFTWND_F77_ONE
#define fftwnd_f77_destroy_plan FFTWND_F77_DESTROY_PLAN
#define fftw_f77_threads_one FFTW_F77_THREADS_ONE
#define fftw_f77_threads FFTW_F77_THREADS
#define fftwnd_f77_threads FFTWND_F77_THREADS
#define fftwnd_f77_threads_one FFTWND_F77_THREADS_ONE
#define fftw_f77_threads_init FFTW_F77_THREADS_INIT

#define rfftw_f77_create_plan RFFTW_F77_CREATE_PLAN
#define rfftw_f77_one RFFTW_F77_ONE
#define rfftw_f77 RFFTW_F77
#define rfftw_f77_destroy_plan RFFTW_F77_DESTROY_PLAN
#define rfftwnd_f77_create_plan RFFTWND_F77_CREATE_PLAN
#define rfftw2d_f77_create_plan RFFTW2D_F77_CREATE_PLAN
#define rfftw3d_f77_create_plan RFFTW3D_F77_CREATE_PLAN
#define rfftwnd_f77_real_to_complex RFFTWND_F77_REAL_TO_COMPLEX
#define rfftwnd_f77_one_real_to_complex RFFTWND_F77_ONE_REAL_TO_COMPLEX
#define rfftwnd_f77_complex_to_real RFFTWND_F77_COMPLEX_TO_REAL
#define rfftwnd_f77_one_complex_to_real RFFTWND_F77_ONE_COMPLEX_TO_REAL
#define rfftwnd_f77_destroy_plan RFFTWND_F77_DESTROY_PLAN

#define fftw_f77_threads_one FFTW_F77_THREADS_ONE
#define fftw_f77_threads FFTW_F77_THREADS
#define fftwnd_f77_threads FFTWND_F77_THREADS
#define fftwnd_f77_threads_one FFTWND_F77_THREADS_ONE
#define rfftwnd_f77_threads_real_to_complex RFFTWND_F77_THREADS_REAL_TO_COMPLEX
#define rfftwnd_f77_threads_one_real_to_complex RFFTWND_F77_THREADS_ONE_REAL_TO_COMPLEX
#define rfftwnd_f77_threads_complex_to_real RFFTWND_F77_THREADS_COMPLEX_TO_REAL
#define rfftwnd_f77_threads_one_complex_to_real RFFTWND_F77_THREADS_ONE_COMPLEX_TO_REAL

#endif /* FFTW2_F77_MKL_H */



