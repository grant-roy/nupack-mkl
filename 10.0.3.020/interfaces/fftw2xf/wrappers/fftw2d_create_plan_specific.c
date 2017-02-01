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
!   function    : fftw2d_create_plan_specific - interface FFTW initialization function
!   Content     : wrapper from this function to DFTI MKL initialization functions
!                 for double/float precision complex data
!   input data  : parameters for DFT
!   output data : pointer to FFTW plan
!*******************************************************************************
*/

#include "fftw.h"
#include "fftw2_mkl.h"

fftwnd_plan fftw2d_create_plan_specific(int nx, int ny, fftw_direction dir, int flags,
							 fftw_complex *in, int istride,
						     fftw_complex *out, int ostride) {

    MKL_LONG Status, i;
	MKL_LONG* nn;
	MKL_LONG strides[3];
    fftw_plan_mkl*  fftw_desc_mkl;

    fftw_desc_mkl = (fftw_plan_mkl*)malloc(sizeof(fftw_plan_mkl));
    if (fftw_desc_mkl == NULL) return NULL;

    fftw_desc_mkl->sign = dir;
    fftw_desc_mkl->inplace = flags & FFTW_IN_PLACE;
    fftw_desc_mkl->istride = istride;
    fftw_desc_mkl->ostride = ostride;

	nn = (MKL_LONG*)malloc(2 * sizeof(MKL_LONG));
    if (nn == NULL) {
        free(fftw_desc_mkl);
        return NULL;
    }
	nn[0] = (MKL_LONG)nx;
	nn[1] = (MKL_LONG)ny;

    fftw_desc_mkl->rank = 2;
    fftw_desc_mkl->n = nn;

    Status = DftiCreateDescriptor(&(fftw_desc_mkl->mkl_desc), DFTI_PRECISION, DFTI_COMPLEX, 2, nn);
	if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
        free(fftw_desc_mkl);
		free(nn);
        return NULL;
    }

	if (!(fftw_desc_mkl->inplace)) {
        Status = DftiSetValue( fftw_desc_mkl->mkl_desc, DFTI_PLACEMENT, DFTI_NOT_INPLACE);
        if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
            Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
            free(fftw_desc_mkl);
		    free(nn);
            return NULL;
		}
        strides[0] = 0;
		strides[2] = (MKL_LONG)ostride;
        strides[1] = nn[1] * strides[2];

        Status = DftiSetValue( fftw_desc_mkl->mkl_desc, DFTI_OUTPUT_STRIDES, strides);
        if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
            Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
            free(fftw_desc_mkl);
		    free(nn);
            return NULL;
		}

	}

    strides[0] = 0;
	strides[2] = (MKL_LONG)istride;
	strides[1] = nn[1] * strides[2];

    Status = DftiSetValue( fftw_desc_mkl->mkl_desc, DFTI_INPUT_STRIDES, strides);
    if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
        Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
        free(fftw_desc_mkl);
		free(nn);
        return NULL;
	}


    Status = DftiCommitDescriptor(fftw_desc_mkl->mkl_desc);
    if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
        Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
        free(fftw_desc_mkl);
		free(nn);
        return NULL;
    }

    return (fftwnd_plan)fftw_desc_mkl;
}
