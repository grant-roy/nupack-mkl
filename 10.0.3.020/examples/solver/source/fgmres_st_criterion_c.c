/********************************************************************************
/*                              INTEL CONFIDENTIAL
/*   Copyright(C) 2005-2008 Intel Corporation. All Rights Reserved.
/*   The source code contained  or  described herein and all documents related to
/*   the source code ("Material") are owned by Intel Corporation or its suppliers
/*   or licensors.  Title to the  Material remains with  Intel Corporation or its
/*   suppliers and licensors. The Material contains trade secrets and proprietary
/*   and  confidential  information of  Intel or its suppliers and licensors. The
/*   Material  is  protected  by  worldwide  copyright  and trade secret laws and
/*   treaty  provisions. No part of the Material may be used, copied, reproduced,
/*   modified, published, uploaded, posted, transmitted, distributed or disclosed
/*   in any way without Intel's prior express written permission.
/*   No license  under any  patent, copyright, trade secret or other intellectual
/*   property right is granted to or conferred upon you by disclosure or delivery
/*   of the Materials,  either expressly, by implication, inducement, estoppel or
/*   otherwise.  Any  license  under  such  intellectual property  rights must be
/*   express and approved by Intel in writing.
/*
/********************************************************************************
/*  Content:
/*  Intel MKL RCI (P)FGMRES ((Preconditioned) Flexible Generalized Minimal
/*                                                       RESidual method) example
/********************************************************************************/

/*---------------------------------------------------------------------------
/*  Example program for solving non-symmetric indefinite system of equations
/*  Simple case: no preconditioning and only one stopping test
/*---------------------------------------------------------------------------*/

#include <stdio.h>
#include "mkl_rci.h"
#include "mkl_blas.h"
#include "mkl_spblas.h"
#define N 5
#define size 128

int main(void)
{

	/*---------------------------------------------------------------------------
	/* Define arrays for the upper triangle of the coefficient matrix
	/* Compressed sparse row storage is used for sparse representation
	/*---------------------------------------------------------------------------*/
	MKL_INT ia[6]={1,3,6,9,12,14};
	MKL_INT ja[13]={    1,        3,
						 1,   2,        4,
								2,   3,        5,
									  3,   4,   5,
											 4,   5  };
	double A[13]={ 1.0,     -1.0,
					  -1.0, 1.0,     -1.0,
					        1.0,-2.0,      1.0,
					            -1.0, 2.0,-1.0,
					                 -1.0,-3.0 };
	/*---------------------------------------------------------------------------
	/* Allocate storage for the ?par parameters and the solution/rhs/residual vectors
	/*---------------------------------------------------------------------------*/
	MKL_INT ipar[size];
	double dpar[size], tmp[N*(2*N+1)+(N*(N+9))/2+1];
	double expected_solution[N]={-1.0,1.0,0.0,1.0,-1.0};
	double rhs[N], b[N];
	double computed_solution[N];
	double residual[N];
	/*---------------------------------------------------------------------------
	/* Some additional variables to use with the RCI (P)FGMRES solver
	/*---------------------------------------------------------------------------*/
	MKL_INT itercount;
	MKL_INT RCI_request, i, ivar;
	double dvar;
	char cvar;

	printf("--------------------------------------------------\n");
	printf(" The SIMPLE example of usage of RCI FGMRES solver\n");
	printf("to solve a non-symmetric indefinite non-degenerate\n");
	printf("       algebraic system of linear equations\n");
	printf("--------------------------------------------------\n\n");
	/*---------------------------------------------------------------------------
	/* Initialize variables and the right hand side through matrix-vector product
	/*---------------------------------------------------------------------------*/
	ivar=N;
	cvar='N';
	mkl_dcsrgemv(&cvar, &ivar, A, ia, ja, expected_solution, rhs);
	/*---------------------------------------------------------------------------
	/* Save the right-hand side in vector b for future use
	/*---------------------------------------------------------------------------*/
	i=1;
	dcopy(&ivar, rhs, &i, b, &i);
	/*---------------------------------------------------------------------------
	/* Initialize the initial guess
	/*---------------------------------------------------------------------------*/
	for(i=0;i<N;i++)
	{
		computed_solution[i]=1.0;
	}
	/*---------------------------------------------------------------------------
	/* Initialize the solver
	/*---------------------------------------------------------------------------*/
	dfgmres_init(&ivar, computed_solution, rhs, &RCI_request, ipar, dpar, tmp);
	if (RCI_request!=0) goto FAILED;
	/*---------------------------------------------------------------------------
	/* Set the desired parameters:
	/* LOGICAL parameters:
	/* do not do the residual stopping test
	/* do request for the user defined stopping test
	/* do the check of the norm of the next generated vector automatically
	/* DOUBLE PRECISION parameters
	/* set the relative tolerance to 1.0D-3 instead of default value 1.0D-6
	/*---------------------------------------------------------------------------*/
	ipar[8]=0;
	ipar[9]=1;
	ipar[11]=1;
	dpar[0]=1.0E-3;
	/*---------------------------------------------------------------------------
	/* Check the correctness and consistency of the newly set parameters
	/*---------------------------------------------------------------------------*/
	dfgmres_check(&ivar, computed_solution, rhs, &RCI_request, ipar, dpar, tmp);
	if (RCI_request!=0) goto FAILED;
	/*---------------------------------------------------------------------------
	/* Print the info about the RCI FGMRES method
	/*---------------------------------------------------------------------------*/
	printf("Some info about the current run of RCI FGMRES method:\n\n");
	if (ipar[7])
	{
		printf("As ipar[7]=%d, the automatic test for the maximal number of iterations will be\n", ipar[7]);
		printf("performed\n");
	}
	else
	{
		printf("As ipar[7]=%d, the automatic test for the maximal number of iterations will be\n", ipar[7]);
		printf("skipped\n");
	}
	printf("+++\n");
	if (ipar[8])
	{
		printf("As ipar[8]=%d, the automatic residual test will be performed\n", ipar[8]);
	}
	else
	{
		printf("As ipar[8]=%d, the automatic residual test will be skipped\n", ipar[8]);
	}
	printf("+++\n");
	if (ipar[9])
	{
		printf("As ipar[9]=%d, the user-defined stopping test will be requested via\n", ipar[9]);
		printf("RCI_request=2\n");
	}
	else
	{
		printf("As ipar[9]=%d, the user-defined stopping test will not be requested, thus,\n", ipar[9]);
		printf("RCI_request will not take the value 2\n");
	}
	printf("+++\n");
	if (ipar[10])
	{
		printf("As ipar[10]=%d, the Preconditioned FGMRES iterations will be performed, thus,\n", ipar[10]);
		printf("the preconditioner action will be requested via RCI_request=3\n");
	}
	else
	{
		printf("As ipar[10]=%d, the Preconditioned FGMRES iterations will not be performed,\n", ipar[10]);
		printf("thus, RCI_request will not take the value 3\n");
	}
	printf("+++\n");
	if (ipar[11])
	{
		printf("As ipar[11]=%d, the automatic test for the norm of the next generated vector is\n", ipar[11]);
		printf("not equal to zero up to rounding and computational errors will be performed,\n");
		printf("thus, RCI_request will not take the value 4\n");
	}
	else
	{
		printf("As ipar[11]=%d, the automatic test for the norm of the next generated vector is\n", ipar[11]);
		printf("not equal to zero up to rounding and computational errors will be skipped,\n");
		printf("thus, the user-defined test will be requested via RCI_request=4\n");
	}
	printf("+++\n\n");
	/*---------------------------------------------------------------------------
	/* Compute the solution by RCI (P)FGMRES solver without preconditioning
	/* Reverse Communication starts here
	/*---------------------------------------------------------------------------*/
ONE:  dfgmres(&ivar, computed_solution, rhs, &RCI_request, ipar, dpar, tmp);
	/*---------------------------------------------------------------------------
	/* If RCI_request=0, then the solution was found with the required precision
	/*---------------------------------------------------------------------------*/
	if (RCI_request==0) goto COMPLETE;
	/*---------------------------------------------------------------------------
	/* If RCI_request=1, then compute the vector A*tmp[ipar[21]-1]
	/* and put the result in vector tmp[ipar[22]-1]
	/*---------------------------------------------------------------------------
	/* NOTE that ipar[21] and ipar[22] contain FORTRAN style addresses,
	/* therefore, in C code it is required to subtract 1 from them to get C style
	/* addresses
	/*---------------------------------------------------------------------------*/
	if (RCI_request==1)
	{
		mkl_dcsrgemv(&cvar, &ivar, A, ia, ja, &tmp[ipar[21]-1], &tmp[ipar[22]-1]);
		goto ONE;
	}
	/*---------------------------------------------------------------------------
	/* If RCI_request=2, then do the user-defined stopping test
	/* The residual stopping test for the computed solution is performed here
	/* NOTE: from this point vector rhs[N] is no longer containing the right-hand
	/* side of the problem! It contains the current FGMRES approximation to the
	/* solution. If you need to keep the right-hand side, save it in some other
	/* vector before the call to dfgmres routine. Here we saved it in vector b[N]
	/*---------------------------------------------------------------------------*/
	if (RCI_request==2)
	{
		/* Request to the dfgmres_get routine to put the solution into rhs[N] via ipar[12] */
		/* WARNING: beware that the call to dfgmres_get routine with ipar[12]=0 at this stage may
		/* destroy the convergence of the FGMRES method, therefore, only advanced users should
		/* exploit this option with care */
		ipar[12]=1;
		/* Get the current computed solution in the vector rhs[N] */
		dfgmres_get(&ivar, computed_solution, rhs, &RCI_request, ipar, dpar, tmp, &itercount);
		/* Compute the current true residual via MKL (Sparse) BLAS routines */
		mkl_dcsrgemv(&cvar, &ivar, A, ia, ja, rhs, residual);
		dvar=-1.0E0;
		i=1;
		daxpy(&ivar, &dvar, b, &i, residual, &i);
		dvar=dnrm2(&ivar,residual,&i);
		if (dvar<1.0E-3) goto COMPLETE;
		else goto ONE;
	}
	/*---------------------------------------------------------------------------
	/* If RCI_request=anything else, then dfgmres subroutine failed
	/* to compute the solution vector: computed_solution[N]
	/*---------------------------------------------------------------------------*/
	else
	{
		goto FAILED;
	}
	/*---------------------------------------------------------------------------
	/* Reverse Communication ends here
	/* Get the current iteration number and the FGMRES solution (DO NOT FORGET to
	/* call dfgmres_get routine as computed_solution is still containing
	/* the initial guess!). Request to dfgmres_get to put the solution
	/* into vector computed_solution[N] via ipar[12]
	/*---------------------------------------------------------------------------*/
COMPLETE:   ipar[12]=0;
	dfgmres_get(&ivar, computed_solution, rhs, &RCI_request, ipar, dpar, tmp, &itercount);
	/*---------------------------------------------------------------------------
	/* Print solution vector: computed_solution[N] and the number of iterations: itercount
	/*---------------------------------------------------------------------------*/
	printf(" The system has been SUCCESSFULLY solved \n");
	printf("\n The following solution has been obtained: \n");
	for (i=0;i<N;i++)
	{
		printf("computed_solution[%d]=",i);
		printf("%e\n",computed_solution[i]);
	}
	printf("\n The expected solution is: \n");
	for (i=0;i<N;i++)
	{
		printf("expected_solution[%d]=",i);
		printf("%e\n",expected_solution[i]);
	}
	printf("\n Number of iterations: %d",itercount);
	goto SUCCEDED;
FAILED: printf("The solver has returned the ERROR code %d", RCI_request);

SUCCEDED: return 0;
}
