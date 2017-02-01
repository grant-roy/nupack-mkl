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
!   function    : rfftw3d_create_plan - wrapper for interface FFTW function
!   Content     : wrapper returns NULL because there is no the same implementation
!               : of 3D REAL DFT in MKL
!   output data : NULL
!*******************************************************************************
*/

#include "rfftw.h"
#include "fftw2_mkl.h"

rfftwnd_plan rfftw3d_create_plan(int nx, int ny, int nz, fftw_direction dir, int flags) {

    MKL_LONG Status;
	MKL_LONG* nn;
	MKL_LONG strides_in[4];
	MKL_LONG strides_out[4];
    fftw_plan_mkl*  fftw_desc_mkl;

	fftw_desc_mkl = (fftw_plan_mkl*)malloc(sizeof(fftw_plan_mkl));
    if (fftw_desc_mkl == NULL) return NULL;

    if (dir == FFTW_REAL_TO_COMPLEX) {
	    fftw_desc_mkl->sign = FFTW_FORWARD;
	} else {
	    fftw_desc_mkl->sign = FFTW_BACKWARD;
	}
    fftw_desc_mkl->inplace = flags & FFTW_IN_PLACE;
    fftw_desc_mkl->rank = 3;
    fftw_desc_mkl->istride = 1;
    fftw_desc_mkl->ostride = 1;

	nn = (MKL_LONG*)malloc(3 * sizeof(MKL_LONG));
    if (nn == NULL) {
        free(fftw_desc_mkl);
        return NULL;
    }
	nn[0] = (MKL_LONG)nx;
	nn[1] = (MKL_LONG)ny;
	nn[2] = (MKL_LONG)nz;

    fftw_desc_mkl->n = nn;

	strides_in[0] = 0;
	strides_in[1] = (nn[2]/2+1) * nn[1];
	strides_in[2] = nn[2]/2+1;
	strides_in[3] = 1;

	strides_out[0] = 0;
	strides_out[1] = 2 * (nn[2]/2+1) * nn[1];
	strides_out[2] = 2 * (nn[2]/2+1);
	strides_out[3] = 1;

    Status = DftiCreateDescriptor(&(fftw_desc_mkl->mkl_desc), DFTI_PRECISION, DFTI_REAL, 3, nn);
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
	}

    Status = DftiSetValue( fftw_desc_mkl->mkl_desc, DFTI_CONJUGATE_EVEN_STORAGE, DFTI_COMPLEX_COMPLEX);
    if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
        Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
        free(fftw_desc_mkl);
		free(nn);
        return NULL;
	}

        if (dir == FFTW_REAL_TO_COMPLEX) {
			if (fftw_desc_mkl->inplace) {
				Status = DftiSetValue(fftw_desc_mkl->mkl_desc, DFTI_INPUT_STRIDES, strides_out);
                if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
                    Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
                    free(fftw_desc_mkl);
                    free(nn);
                    return NULL;
				}
			}
			Status = DftiSetValue(fftw_desc_mkl->mkl_desc, DFTI_OUTPUT_STRIDES, strides_in);
            if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
                Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
                free(fftw_desc_mkl);
                free(nn);
                return NULL;
			}
		} else {
			Status = DftiSetValue(fftw_desc_mkl->mkl_desc, DFTI_INPUT_STRIDES, strides_in);
            if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
                Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
                free(fftw_desc_mkl);
                free(nn);
                return NULL;
			}
			if (fftw_desc_mkl->inplace) {
				Status = DftiSetValue(fftw_desc_mkl->mkl_desc, DFTI_OUTPUT_STRIDES, strides_out);
                if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
                    Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
                    free(fftw_desc_mkl);
                    free(nn);
                    return NULL;
				}
			}
		}

	Status = DftiCommitDescriptor(fftw_desc_mkl->mkl_desc);
    if (! DftiErrorClass(Status, DFTI_NO_ERROR)) {
        Status = DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
        free(fftw_desc_mkl);
		free(nn);
        return NULL;
    }
    return (rfftwnd_plan)fftw_desc_mkl;
}
