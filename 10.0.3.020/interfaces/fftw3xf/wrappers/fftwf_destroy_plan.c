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
!   function    : fftwf_destroy_plan - interface FFTW destroy function
!   Content     : wrapper from this function to DFTI MKL destroy function
!                 for float precision complex data
!   input data  : pointer to FFTW plan
!*******************************************************************************
*/

#include "fftw3.h"
#include "fftw3_mkl.h"
#include "mkl_trig_transforms.h"

void
fftwf_destroy_plan(fftwf_plan plan)
{
    MKL_INT ir;
    fftw_plan_mkl*  fftw_desc_mkl;

    fftw_desc_mkl = (fftw_plan_mkl*)plan;

    if ((fftw_desc_mkl->tt_fftw_mkl)==(NULL))
    {
        DftiFreeDescriptor(&(fftw_desc_mkl->mkl_desc));
    }
    else
    {
        free_trig_transform(&(fftw_desc_mkl->mkl_desc),
                            ((fftw_desc_mkl->tt_fftw_mkl)->ipar),&ir);
        free(fftw_desc_mkl->tt_fftw_mkl);
    }

    free(fftw_desc_mkl);
    plan = NULL;

    return;
}
