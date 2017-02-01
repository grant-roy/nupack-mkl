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
!   function    : dfftw_plan_guru_dft - interface FFTW initialization function
!   Content     : wrapper from this function to DFTI MKL initialization functions
!                 for double precision complex data
!   input data  : parameters for DFT
!   output data : pointer to FFTW plan
!*******************************************************************************
*/

#include "fftw3.h"
#include "fftw3_mkl.h"

void dfftw_plan_guru_dft(fftw_plan* plan, int* rank, int* n, int* is, int* os,
			 int* howmany_rank, int* howmany_n, int* howmany_is, int* howmany_os,
			 fftw_complex *in, fftw_complex *out,
			 int* sign, unsigned* flags) {

int i;
fftw_iodim dims[7];
fftw_iodim howmany_dims[1];
    if (*rank > 7) { *plan = 0; return; }
    if (*howmany_rank > 1) { *plan = 0; return; }
    for (i=0; i < *rank; i++) {
        dims[i].n = n[*rank-i-1];
        dims[i].is = is[*rank-i-1];
        dims[i].os = os[*rank-i-1];
    }
    for (i=0; i < 1; i++) {
        howmany_dims[i].n = howmany_n[i];
        howmany_dims[i].is = howmany_is[i];
        howmany_dims[i].os = howmany_os[i];
    }

    *plan = fftw_plan_guru_dft(*rank, dims, *howmany_rank, howmany_dims,
			 in, out, *sign, *flags);
    return;
}

