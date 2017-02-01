/*******************************************************************************
/*                              INTEL CONFIDENTIAL
/*   Copyright(C) 2006-2008 Intel Corporation. All Rights Reserved.
/*   The source code contained  or  described herein and all documents related to
/*   the source code ("Material") are owned by Intel Corporation or its suppliers
/*   or licensors.  Title to the  Material remains with  Intel Corporation or its
/*   suppliers and licensors. The Material contains trade secrets and proprietary
/*   and  confidential  information of  Intel or its suppliers and licensors. The
/*   Material  is  protected  by  worldwide  copyright  and trade secret laws and
/*   treaty  provisions. No part of the Material may be used, copied, reproduced,
/*   modified, published, uploaded, posted, transmitted, distributed or disclosed
/*   in any way without Intel");s prior express written permission.
/*   No license  under any  patent, copyright, trade secret or other intellectual
/*   property right is granted to or conferred upon you by disclosure or delivery
/*   of the Materials,  either expressly, by implication, inducement, estoppel or
/*   otherwise.  Any  license  under  such  intellectual property  rights must be
/*   express and approved by Intel in writing.
/*
/*******************************************************************************
/*  Content:
/*  C real example of solving 2D Laplace problem in a
/*  rectangular domain using MKL Poisson Library
/*
/*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/* Include Poisson Library header files */
#include "mkl_poisson.h"

int main(void)
{
	MKL_INT nx=6, ny=6;

	MKL_INT ix, iy, i, stat;
	MKL_INT ipar[128];
	float ax, bx, ay, by, lx, ly, hx, hy, xi, yi, c1;
	float *spar, *f, *u, *bd_ax, *bd_bx, *bd_ay, *bd_by;
	float q;
	DFTI_DESCRIPTOR_HANDLE xhandle = 0;
	char *BCtype;

	/* Printing the header for the example */
	printf("\n Example of use of MKL Poisson Library\n");
	printf(" **********************************************\n\n");
	printf(" This example gives the solution of the 2D Laplace problem\n");
	printf(" with the equation u_xx+u_yy=0, 0<x<1, 0<y<1,\n");
	printf(" with the following boundary conditions:\n");
	printf(" u(0,y)=  y^4\n");
	printf(" u(1,y)=1+y^4-6*(y^2)\n");
	printf(" u(x,0)=x^4\n");
	printf(" u(x,1)=x^4+1-6*(x^2)\n");
	printf(" -----------------------------------------------------------------------\n");
	printf(" In general, the error should be of order O(1.0/nx^2+1.0/ny^2)\n");
	printf(" For this example, the value of nx=ny is %d\n", nx);
	printf(" The approximation error should be of order 5.0e-03, if everything is OK\n");
	printf(" -----------------------------------------------------------------------\n");
	printf("                      real COMPUTATIONS                     \n");
	printf(" =======================================================================\n\n");

	spar=(float*)malloc((13*nx/2+7)*sizeof(float));
	f=(float*)malloc((nx+1)*(ny+1)*sizeof(float));
	u=(float*)malloc((nx+1)*(ny+1)*sizeof(float));
	bd_ax=(float*)malloc((ny+1)*sizeof(float));
	bd_bx=(float*)malloc((ny+1)*sizeof(float));
	bd_ay=(float*)malloc((nx+1)*sizeof(float));
	bd_by=(float*)malloc((nx+1)*sizeof(float));

	/* Defining the rectangular domain 0<x<1, 0<y<1 for 2D Laplace Solver */
	ax=0.0E0;
	bx=1.0E0;
	ay=0.0E0;
	by=1.0E0;

	/*******************************************************************************
	Setting the coefficient q to 0.
	Note that this is the first of two steps on the way to use Helmholtz Solver
	to solve Laplace problem!
	*******************************************************************************/
	q=0.0E0;

	/* Computing the mesh size hx in x-direction */
	lx=bx-ax;
	hx=lx/nx;
	/* Computing the mesh size hy in y-direction */
	ly=by-ay;
	hy=ly/ny;

	/* Filling in the values of the TRUE solution u(x,y)=x^4+y^4-6*(y^2)*(x^2)
	in the mesh points into the array u
	*******************************************************************************
	Filling in the right-hand side f(x,y)=0.0 in the mesh points into the array f.
	Note that this the second of two steps on the way to use Helmholtz Solver
	to solve Laplace problem/* Current implementation of Poisson Library requires
	the array f for the solution of Laplace problem
	*******************************************************************************
	Here we are using the mesh sizes hx and hy computed before to compute
	the coordinates (xi,yi) of the mesh points */
	for(iy=0;iy<=ny;iy++)
	{
		for(ix=0;ix<=nx;ix++)
		{
			xi=hx*ix/lx;
			yi=hy*iy/ly;

			u[ix+iy*(nx+1)]=xi*xi*xi*xi+yi*yi*yi*yi-6.0E0*(yi*yi)*(xi*xi);
			f[ix+iy*(nx+1)]=0.0E0;
		}
	}

	/* Setting the type of the boundary conditions on each side of the rectangular domain:
	On the boundary laying on the line x=0(=ax) Dirichlet boundary condition will be used
	On the boundary laying on the line x=1(=bx) Dirichlet boundary condition will be used
	On the boundary laying on the line y=0(=ay) Dirichlet boundary condition will be used
	On the boundary laying on the line y=1(=by) Dirichlet boundary condition will be used */
	BCtype = "DDDD";

	/* Setting the values of the boundary function G(x,y) that is equal to the TRUE solution
	in the mesh points laying on Dirichlet boundaries */
	for(iy=0;iy<=ny;iy++)
	{
		yi=hy*iy/ly;
		bd_ax[iy]=yi*yi*yi*yi;
		bd_bx[iy]=1.0E0-6.0E0*yi*yi+yi*yi*yi*yi;
	}
	for(ix=0;ix<=nx;ix++)
	{
		xi=hx*ix/lx;
		bd_ay[ix]=xi*xi*xi*xi;
		bd_by[ix]=1.0E0-6.0E0*xi*xi+xi*xi*xi*xi;
	}

	/* Initializing ipar array to make it free from garbage */
	for(i=0;i<128;i++)
	{
		ipar[i]=0;
	}

	/* Initializing simple data structures of Poisson Library for 2D Laplace Solver */
	s_init_Helmholtz_2D(&ax, &bx, &ay, &by, &nx, &ny, BCtype, &q, ipar, spar, &stat);
	if (stat!=0) goto FAILURE;

	/* Initializing complex data structures of Poisson Library for 2D Laplace Solver
	NOTE: Right-hand side f may be altered after the Commit step. If you want to keep it,
	you should save it in another memory location! */
	s_commit_Helmholtz_2D(f, bd_ax, bd_bx, bd_ay, bd_by, &xhandle, ipar, spar, &stat);
	if (stat!=0) goto FAILURE;

	/* Computing the approximate solution of 2D Laplace problem
	NOTE: Boundary data stored in the arrays bd_ax, bd_bx, bd_ay, bd_by should not be changed
	between the Commit step and the subsequent call to the Solver routine/*
	Otherwise the results may be wrong. */
	s_Helmholtz_2D(f, bd_ax, bd_bx, bd_ay, bd_by, &xhandle, ipar, spar, &stat);
	if (stat!=0) goto FAILURE;

	/* Cleaning the memory used by xhandle */
	free_Helmholtz_2D(&xhandle, ipar, &stat);
	if (stat!=0) goto FAILURE;
	/* Now we can use xhandle to solve another 2D Laplace problem */

	/* Printing the results */
	printf("The number of mesh intervals in x-direction is nx=%d\n", nx);
	printf("The number of mesh intervals in y-direction is ny=%d\n\n", ny);

	/* Watching the error along the line x=hx */
	ix=1;
	c1=0.0;
	for(iy=0;iy<=ny;iy++)
	{
		printf("In the mesh point (%5.3f,%5.3f) the error between the computed and the true solution is equal to %10.3e\n", ix*hx, iy*hy, f[ix+iy*(nx+1)]-u[ix+iy*(nx+1)]);
		if (c1<fabs(f[ix+iy*(nx+1)]-u[ix+iy*(nx+1)])) c1 = fabs(f[ix+iy*(nx+1)]-u[ix+iy*(nx+1)]);
	}
	if (c1>5.0e-03)
	{
		printf("The computed solution seems to be inaccurate. ");
		goto FAILURE;
	}

	/* Success message to print if everything is OK */
	printf("\n real 2D Laplace example has successfully PASSED\n");
	printf(" through all steps of computation!\n");

	/* Jumping over failure message */
	goto SUCCESS;

	/* Failure message to print if something went wrong */
FAILURE: printf("\nsingle 2D Laplace example FAILED to compute the solution...\n");

SUCCESS: return 0;

	/* End of the example code */
}
