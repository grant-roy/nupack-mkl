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
*   Content : TR Solver C example
*
********************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "mkl_rci.h"
#include "mkl_types.h"


/* nonlinear least square problem with boundary constraints */
int main ()
{
	/* user’s objective function */
	extern void extendet_powell (MKL_INT *, MKL_INT *, double*, double*);
	/* n - number of function variables
	   m - dimension of function value */
	MKL_INT			n = 4, m = 4;
	/* precisions for stop-criteria (see manual for more detailes) */
	double	eps[6];
	/* solution vector. contains values x for f(x) */
	double	*x;
	/* iter1 - maximum number of iterations
	   iter2 - maximum number of iterations of calculation of trial-step */
	MKL_INT			iter1 = 1000, iter2 = 100;
	/* initial step bound */
	double	rs = 0.0;
	/* reverse communication interface parameter */
	MKL_INT			RCI_Request;
	/* controls of rci cycle */
	MKL_INT			successful;
	/* function (f(x)) value vector */
	double *fvec;
	/* jacobi matrix */
	double *fjac;
	/* lower and upper bounds */
	double	*LW, *UP;
	/* number of iterations */
	MKL_INT			iter;
	/* number of stop-criterion */
	MKL_INT			st_cr;
	/* initial and final residauls */
	double r1, r2;
	/* TR solver handle */
	_TRNSPBC_HANDLE_t handle;
	/* cycle’s counter */
	MKL_INT i;

	/* memory allocation */
	x = (double*) malloc (sizeof (double)*n);
	fvec = (double*) malloc (sizeof (double)*m);
	fjac = (double*) malloc (sizeof (double)*m*n);
	LW = (double*) malloc (sizeof (double)*n);
	UP = (double*) malloc (sizeof (double)*n);
	/* set precisions for stop-criteria */
	for (i = 0; i < 6; i++)
	{
		eps [i] = 0.00001;
	}
	/* set the initial guess */
    for (i = 0; i < n/4; i++)
	{
        x [4*i]		=  3.0;
        x [4*i + 1] = -1.0;
        x [4*i + 2] =  0.0;
        x [4*i + 3] =  1.0;
	}
	/* set initial values */
	for (i = 0; i < m; i++)
		fvec [i] = 0.0;
	for (i = 0; i < m*n; i++)
		fjac [i] = 0.0;
	/* set bounds */
	for (i = 0; i < n/4; i++)
	{
		LW [4*i]		=   0.1;
		LW [4*i + 1]	= -20.0;
		LW [4*i + 2]	=  -1.0;
		LW [4*i + 3]	=  -1.0;
		UP [4*i]		= 100.0;
		UP [4*i + 1]	=  20.0;
		UP [4*i + 2]	=   1.0;
		UP [4*i + 3]	=  50.0;
	}
	/* initialize solver (allocate mamory, set initial values)
		handle	in/out:	TR solver handle
		n       in:     number of function variables
		m       in:     dimension of function value
		x       in:     solution vector. contains values x for f(x)
		LW		in:		lower bound
		UP		in:		upper bound
		eps     in:     precisions for stop-criteria
		iter1   in:     maximum number of iterations
		iter2   in:     maximum number of iterations of calculation of trial-step
		rs      in:     initial step bound */
	if (dtrnlspbc_init (&handle, &n, &m, x, LW, UP, eps, &iter1, &iter2, &rs) != TR_SUCCESS)
	{
		/* if function does not complete successful then print error message */
		printf ("| error in dtrnlspbc_init\n");
		/* and exit */
		return 0;
	}
	/* set initial rci cycle variables */
    RCI_Request = 0;
	successful = 0;
	/* rci cycle */
    while (successful == 0)
	{
		/* call tr solver
			handle		in/out:	tr solver handle
			fvec		in:     vector
			fjac		in:     jacobi matrix
			RCI_request in/out:	return number which denote next step for performing */
		if (dtrnlspbc_solve (&handle, fvec, fjac, &RCI_Request) != TR_SUCCESS)
		{
			/* if function does not complete successful then print error message */
			printf ("| error in dtrnlspbc_solve\n");
			/* and exit */
			return 0;
		}
		/* according with rci_request value we do next step */
		if (RCI_Request == -1 ||
			RCI_Request == -2 ||
			RCI_Request == -3 ||
			RCI_Request == -4 ||
			RCI_Request == -5 ||
			RCI_Request == -6)
			/* exit rci cycle */
			successful = 1;
	    if (RCI_Request == 1)
		{
			/* recalculate function value
				m		in:     dimension of function value
				n		in:     number of function variables
				x		in:     solution vector
				fvec    out:    function value f(x) */
            extendet_powell (&m, &n, x, fvec);
		}
		if (RCI_Request == 2)
		{
			/* compute jacobi matrix
				extendet_powell	in:     external objective function
				n               in:     number of function variables
				m               in:     dimension of function value
				fjac            out:    jacobi matrix
				x               in:     solution vector
				jac_eps         in:     jacobi calculation precision */
			if (djacobi (extendet_powell, &n, &m, fjac, x, eps) != TR_SUCCESS)
			{
				/* if function does not complete successful then print error message */
				printf ("| error in djacobi\n");
				/* and exit */
				return 0;
			}
		}
	}
	/* get solution statuses
		handle            in:	TR solver handle
		iter              out:	number of iterations
		st_cr             out:	number of stop criterion
		r1                out:	initial residuals
		r2                out:	final residuals */
	if (dtrnlspbc_get (&handle, &iter, &st_cr, &r1, &r2) != TR_SUCCESS)
	{
		/* if function does not complete successful then print error message */
		printf ("| error in dtrnlspbc_get\n");
		/* and exit */
		return 0;
	}
	/* free handle memory */
	if (dtrnlspbc_delete (&handle) != TR_SUCCESS)
	{
		/* if function does not complete successful then print error message */
		printf ("| error in dtrnlspbc_delete\n");
		/* and exit */
		return 0;
	}
	/* free allocated memory */
	free (x);
	free (fvec);
	free (fjac);
	free (LW);
	free (UP);
	/* if final residual less then required precision then print pass */
    if (r2 < 0.1)
		printf ("|         dtrnlspbc powell..........PASS\n");
    else
		printf ("|         dtrnlspbc powell..........FAILED\n");
    return 0;
}

/* nonlinear system equations without constraints */
/* routine for extendet powell function calculation
   m     in:     dimension of function value
   n     in:     number of function variables
   x     in:     vector for function calculating
   f     out:    function value f(x) */
void extendet_powell (MKL_INT *m, MKL_INT *n, double *x, double *f)
{
	MKL_INT i;
	for (i = 0; i < (*n)/4; i++)
	{
		f [4*i] = x [4*i] + 10.0*x [4*i + 1];
		f [4*i + 1] = 2.2360679774997896964091736687313*(x [4*i + 2] - x [4*i + 3]);
		f [4*i + 2] = (x [4*i + 1] - 2.0*x [4*i + 2])*(x [4*i + 1] - 2.0*x [4*i + 2]);
		f [4*i + 3] = 3.1622776601683793319988935444327*(x [4*i] - x [4*i + 3])*(x [4*i] - x [4*i + 3]);
	}
	return;
}
