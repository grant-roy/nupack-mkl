/*
********************************************************************************
*                              INTEL CONFIDENTIAL
*   Copyright(C) 2004-2008 Intel Corporation. All Rights Reserved.
*   The source code contained  or  described herein and all documents related to
*   the source code ("Material") are owned by Intel Corporation or its suppliers
*   or licensors.  Title to the  Material remains with  Intel Corporation or its
*   suppliers and licensors. The Material contains trade secrets and proprietary
*   and  confidential  information of  Intel or its suppliers and licensors. The
*   Material  is  protected  by  worldwide  copyright  and trade secret laws and
*   treaty  provisions. No part of the Material may be used, copied, reproduced,
*   modified, published, uploaded, posted, transmitted, distributed or disclosed
*   in any way without Intel's prior express written permission.
*   No license  under any  patent, copyright, trade secret or other intellectual
*   property right is granted to or conferred upon you by disclosure or delivery
*   of the Materials,  either expressly, by implication, inducement, estoppel or
*   otherwise.  Any  license  under  such  intellectual property  rights must be
*   express and approved by Intel in writing.
*
********************************************************************************
*   Content : MKL DSS C example
*
********************************************************************************
*/
/*
**
** Example program to solve symmetric positive definite system of equations.
**
*/
#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include "mkl_dss.h"
/*
** Define the array and rhs vectors
*/
#define NROWS 		5
#define NCOLS 		5
#define NNONZEROS 	9
#define NRHS 		1
#if defined(MKL_ILP64)
#define MKL_INT long long
#else
#define MKL_INT int
#endif
static const MKL_INT nRows =     NROWS 	;
static const MKL_INT nCols =     NCOLS 	;
static const MKL_INT nNonZeros = NNONZEROS  ;
static const MKL_INT nRhs =      NRHS 	;
static _INTEGER_t rowIndex[NROWS+1] = { 1, 6, 7, 8, 9, 10 };
static _INTEGER_t columns[NNONZEROS] = { 1, 2, 3, 4, 5, 2, 3, 4, 5 };
static _DOUBLE_PRECISION_t values[NNONZEROS] = { 9, 1.5, 6, .75, 3, 0.5, 12, .625, 16 };
static _DOUBLE_PRECISION_t rhs[NCOLS] = { 1, 2, 3, 4, 5 };

MKL_INT main() {
	MKL_INT i;
	/* Allocate storage for the solver handle and the right-hand side. */
	_DOUBLE_PRECISION_t solValues[NROWS];
	_MKL_DSS_HANDLE_t handle;
	_INTEGER_t error;
	_CHARACTER_t statIn[] = "determinant";
	_DOUBLE_PRECISION_t statOut[5];
	MKL_INT opt = MKL_DSS_DEFAULTS;
	MKL_INT sym = MKL_DSS_SYMMETRIC;
	MKL_INT type = MKL_DSS_POSITIVE_DEFINITE;
/* --------------------- */
/* Initialize the solver */
/* --------------------- */
	error = dss_create(handle, opt );
	if ( error != MKL_DSS_SUCCESS ) goto printError;
/* ------------------------------------------- */
/* Define the non-zero structure of the matrix */
/* ------------------------------------------- */
	error = dss_define_structure(
		handle, sym, rowIndex, nRows, nCols,
		columns, nNonZeros );
	if ( error != MKL_DSS_SUCCESS ) goto printError;
/* ------------------ */
/* Reorder the matrix */
/* ------------------ */
	error = dss_reorder( handle, opt, 0);
	if ( error != MKL_DSS_SUCCESS ) goto printError;
/* ------------------ */
/* Factor the matrix */
/* ------------------ */
	error = dss_factor_real( handle, type, values );
	if ( error != MKL_DSS_SUCCESS ) goto printError;
/* ------------------------ */
/* Get the solution vector */
/* ------------------------ */
	error = dss_solve_real( handle, opt, rhs, nRhs, solValues );
	if ( error != MKL_DSS_SUCCESS ) goto printError;
/* ------------------------ */
/* Get the determinant (not for a diagonal matrix) */
/*--------------------------*/
	if ( nRows < nNonZeros ) {
		error = dss_statistics(handle, opt, statIn, statOut);
		if ( error != MKL_DSS_SUCCESS ) goto printError;
/*-------------------------*/
/* print determinant */
/*-------------------------*/
		printf(" determinant power is %g \n", statOut[0]);
		printf(" determinant base is %g \n", statOut[1]);
		printf(" Determinant is %g \n", (pow(10.0,statOut[0]))*statOut[1]);
	}
/* -------------------------- */
/* Deallocate solver storage */
/* -------------------------- */
	error = dss_delete( handle, opt );
	if ( error != MKL_DSS_SUCCESS ) goto printError;
/* ---------------------- */
/* Print solution vector */
/* ---------------------- */
	printf(" Solution array: ");
	for(i = 0; i< nCols; i++)
		printf(" %g", solValues[i] );
	printf("\n");
	exit(0);
printError:
	printf("Solver returned error code %d\n", error);
	exit(1);
}
