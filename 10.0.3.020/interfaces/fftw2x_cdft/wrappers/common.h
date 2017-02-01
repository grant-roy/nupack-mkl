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
!******************************************************************************
!   Content:
!       Wrapper library building. Header file.
!       This library allows to use MKL CDFT routines through MPI FFTW interface.
!******************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include "mkl_cdft.h"


typedef enum {
	FFTW_FORWARD	=-1,
	FFTW_BACKWARD	= 1
} fftw_direction;

#define FFTW_REAL_TO_COMPLEX FFTW_FORWARD
#define FFTW_COMPLEX_TO_REAL FFTW_BACKWARD

typedef enum {
    FFTW_NORMAL_ORDER,
    FFTW_TRANSPOSED_ORDER
} fftwnd_mpi_output_order;

typedef struct {
	DFTI_DESCRIPTOR_DM_HANDLE h;
	fftw_direction dir;
} fftw_mpi_plan_struct;

typedef struct {
	DFTI_DESCRIPTOR_DM_HANDLE h;
	fftw_direction dir;
} fftwnd_mpi_plan_struct;

#define  FFTW_MEASURE  (1)
#define  FFTW_ESTIMATE (0)

#define  FFTW_SCRAMBLED_OUTPUT (16384)
#define  FFTW_SCRAMBLED_INPUT (8192)

#define fftw_mpi_plan fftw_mpi_plan_struct*
#define fftwnd_mpi_plan fftwnd_mpi_plan_struct*
#define rfftwnd_mpi_plan fftwnd_mpi_plan_struct*
