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
!   function    : fftw_plan_many_dft - interface FFTW initialization function
!   Content     : wrapper from this function to DFTI MKL initialization functions
!                 for double precision complex data
!   input data  : parameters for DFT
!   output data : pointer to FFTW plan
!*******************************************************************************
*/

#include "fftw3.h"
#include "fftw3_mkl.h"

fftw_plan fftw_plan_many_dft(int rank, const int* n, int howmany,
							 fftw_complex *in, const int* inembed,
							 int istride, int idist,
						     fftw_complex *out, const int* onembed,
							 int ostride, int odist,
							 int sign, unsigned flags) {

    MKL_LONG Status, i;
	MKL_LONG* nn;
	MKL_LONG strides[8];
    fftw_plan_mkl*  fftw_desc_mkl;
    fftw_desc_mkl = (fftw_plan_mkl*)malloc(sizeof(fftw_plan_mkl));
    if (fftw_desc_mkl == NULL) return NULL;

	nn = (MKL_LONG*)malloc(rank * sizeof(MKL_LONG));
    if (nn == NULL) {
        free(fftw_desc_mkl);
        return NULL;
    }
    fftw_desc_mkl->in = (void*)in;
    fftw_desc_mkl->out = (void*)out;
    fftw_desc_mkl->tt_fftw_mkl = NULL;
    fftw_desc_mkl->sign = sign;
	for (i=0; i<rank; i++) {
	   nn[i] = (MKL_LONG)n[i];
	}
	if (rank == 1) {
        Status = DftiCreateDescriptor(&(fftw_desc_mkl->mkl_desc), DFTI_DOUBLE, DFTI_COMPLEX, (MKL_LONG)rank, nn[0]);
	} else {
        Status = DftiCreateDescriptor(&(fftw_desc_mkl->mkl_desc), DFTI_DOUBLE, DFTI_COMPLEX, (MKL_LONG)rank, nn);
    }
	if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
        free(fftw_desc_mkl);
        free(nn);
        return NULL;
    }

	if (in != out) {
        Status = DftiSetValue( fftw_desc_mkl->mkl_desc, DFTI_PLACEMENT, DFTI_NOT_INPLACE);
        if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
            Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
            free(fftw_desc_mkl);
            free(nn);
            return NULL;
		}
	} else {
        if (istride != ostride || idist != odist) {
            free(fftw_desc_mkl);
            free(nn);
            return NULL;
		}
	}

    Status = DftiSetValue( fftw_desc_mkl->mkl_desc, DFTI_NUMBER_OF_TRANSFORMS, (MKL_LONG)howmany);
    if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
        Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
        free(fftw_desc_mkl);
        free(nn);
        return NULL;
	}

    Status = DftiSetValue( fftw_desc_mkl->mkl_desc, DFTI_INPUT_DISTANCE, (MKL_LONG)idist);
    if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
        Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
        free(fftw_desc_mkl);
        free(nn);
        return NULL;
	}

    Status = DftiSetValue( fftw_desc_mkl->mkl_desc, DFTI_OUTPUT_DISTANCE, (MKL_LONG)odist);
    if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
        Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
        free(fftw_desc_mkl);
        free(nn);
        return NULL;
	}

    strides[0] = 0;
	strides[rank] = (MKL_LONG)istride;
	for (i = rank-1; i>0; i--) {
		if (inembed != NULL) {
		    strides[i] = (MKL_LONG)inembed[i] * strides[i+1];
		} else {
		    strides[i] = nn[i] * strides[i+1];
		}
	}

    Status = DftiSetValue( fftw_desc_mkl->mkl_desc, DFTI_INPUT_STRIDES, strides);
    if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
        Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
        free(fftw_desc_mkl);
        free(nn);
        return NULL;
	}

    strides[0] = 0;
	strides[rank] = (MKL_LONG)ostride;
	for (i = rank-1; i>0; i--) {
		if (onembed != NULL) {
		    strides[i] = (MKL_LONG)onembed[i] * strides[i+1];
		} else {
		    strides[i] = nn[i] * strides[i+1];
		}
	}

    Status = DftiSetValue( fftw_desc_mkl->mkl_desc, DFTI_OUTPUT_STRIDES, strides);
    if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
        Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
        free(fftw_desc_mkl);
        free(nn);
        return NULL;
	}

    Status = DftiCommitDescriptor(fftw_desc_mkl->mkl_desc);
    if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
        free(fftw_desc_mkl);
        Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
        free(nn);
        return NULL;
    }
    free(nn);
    return (fftw_plan)fftw_desc_mkl;
}
