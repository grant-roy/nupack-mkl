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
#ifndef FFTW2_MKL_H
#define FFTW2_MKL_H

#include "mkl_dfti.h"
#include <stdlib.h>

#ifdef MKL_DOUBLE
    #define TYPE_PRECISION double
    #define DFTI_PRECISION DFTI_DOUBLE
#else
    #define TYPE_PRECISION float
    #define DFTI_PRECISION DFTI_SINGLE
    #define FFTW_ENABLE_FLOAT 1
#endif

typedef struct {
     DFTI_DESCRIPTOR_HANDLE mkl_desc;
     int sign;
     int inplace;
	 int rank;
     int istride;
     int ostride;
	 MKL_LONG* n;
}fftw_plan_mkl;

#endif /* FFTW2_MKL_H */
