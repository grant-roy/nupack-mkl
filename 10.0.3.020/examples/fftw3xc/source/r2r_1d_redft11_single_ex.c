/*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 2007-2008 Intel Corporation. All Rights Reserved.
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
!   Content:
!          MKL TT implementation through r2r FFTW interface (via wrappers) example
!          program (C-interface)
!
!   Real-to-real 1D transform of type REDFT11 (staggered2 cosine transform)  
!   for single precision data
!*********************************************************************************/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "fftw3.h"

#include "mkl_dfti_examples.h"

int main()  /* R2R_1D_REDFT11_SINGLE_EX */
{
	/*
	/   TT input parameters 
	*/
	fftwf_plan Desc_Handle;
	float*  x_in;
	int		 n=15;
	/* */
	float*  x_exp;
	float   Scale;
	float   maxerr;
	float   eps= SINGLE_EPS;

	if(LEGEND_PRINT) {  
		printf(" \n\n R2R_1D_REDFT11_SINGLE_EX           \n");
		printf(" Forward-Backward 1D real-to-real transform of type REDFT11\n");
		printf("(staggered2 cosine transform) for single precision data\n\n");
	}

	/*
	/   Allocate arrays for input and expected data 
	*/
	x_in  = (float*)malloc(n*sizeof(float));
	x_exp = (float*)malloc(n*sizeof(float));
	/*
	/   initialize x_in and copy to expected x_exp 
	*/

	zero_init_s(x_in, n);
	init_real_vectors_s(x_in, n);
	cblas_scopy(n, x_in, 1, x_exp, 1);

	/*
	/   Create TT descriptor for 1D single precision forward transform 
	*/
	Desc_Handle = fftwf_plan_r2r_1d( n, x_in, x_in, FFTW_REDFT11, FFTW_MEASURE);
	/*
	/   Compute TT
	*/
	fftwf_execute(Desc_Handle);
	/*
	/   Destroy TT descriptor 
	*/
	fftwf_destroy_plan(Desc_Handle);

	/*
	/   Set Scale number for Backward transform 
	*/
	Scale = 0.5/(float)n;
	/*
	/   Create TT descriptor for 1D single precision backward transform 
	*/
	Desc_Handle = fftwf_plan_r2r_1d( n, x_in, x_in, FFTW_REDFT11, FFTW_MEASURE);
	/*
	/   Compute TT
	*/
	fftwf_execute(Desc_Handle);
	/*
	/   Destroy TT descriptor 
	*/
	fftwf_destroy_plan(Desc_Handle);
	/*
	/   Result scaling 
	*/
	scaling_fr(x_in, Scale, n);

	/*
	/   Check result 
	*/
	maxerr = check_result_s(x_in, x_exp, n);
	if(ACCURACY_PRINT)                                                    
		printf("\n ACCURACY = %g\n\n", maxerr);

	if(maxerr < eps){
		printf(" TEST PASSED\n");
	} else {
		printf(" TEST FAILED\n");
	} 

	/*
	/   Free arrays for input and expected data 
	*/

	free(x_exp);
	free(x_in);

	return 0;
} 
