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
/*  C double precision example of solving 2D Poisson problem in a
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
	double pi=3.14159265358979324;

	MKL_INT ix, iy, i, stat;
	MKL_INT ipar[128];
	double ax, bx, ay, by, lx, ly, hx, hy, xi, yi, cx, cy, c1;
	double *dpar, *f, *u, *bd_ax, *bd_bx, *bd_ay, *bd_by;
	double q;
	DFTI_DESCRIPTOR_HANDLE xhandle = 0;
	char *BCtype;

	/* Printing the header for the example */
	printf("\n Example of use of MKL Poisson Library\n");
	printf(" **********************************************\n\n");
	printf(" This example gives the solution of 2D Poisson problem\n");
	printf(" with the equation -u_xx-u_yy=f(x,y), 0<x<1, 0<y<1,\n");
	printf(" f(x,y)=(8*pi*pi)*sin(2*pi*x)*sin(2*pi*y),\n");
	printf(" and with the following boundary conditions:\n");
	printf("  u(0,y)=u(1,y)=1 (Dirichlet boundary conditions),\n");
	printf(" -u_y(x,0)=-2.0*pi*sin(2*pi*x) (Neumann boundary condition),\n");
	printf("  u_y(x,1)= 2.0*pi*sin(2*pi*x) (Neumann boundary condition).\n");
	printf(" -----------------------------------------------------------------------\n");
	printf(" In general, the error should be of order O(1.0/nx^2+1.0/ny^2)\n");
	printf(" For this example, the value of nx=ny is %d\n", nx);
	printf(" The approximation error should be of order 1.0e-01, if everything is OK\n");
	printf(" -----------------------------------------------------------------------\n");
	printf("                      DOUBLE PRECISION COMPUTATIONS                     \n");
	printf(" =======================================================================\n\n");

	dpar=(double*)malloc((13*nx/2+7)*sizeof(double));
	f=(double*)malloc((nx+1)*(ny+1)*sizeof(double));
	u=(double*)malloc((nx+1)*(ny+1)*sizeof(double));
	bd_ax=(double*)malloc((ny+1)*sizeof(double));
	bd_bx=(double*)malloc((ny+1)*sizeof(double));
	bd_ay=(double*)malloc((nx+1)*sizeof(double));
	bd_by=(double*)malloc((nx+1)*sizeof(double));

	/* Defining the rectangular domain 0<x<1, 0<y<1 for 2D Poisson Solver */
	ax=0.0E0;
	bx=1.0E0;
	ay=0.0E0;
	by=1.0E0;

	/*******************************************************************************
	Setting the coefficient q to 0.
	Note that this is the way to use Helmholtz Solver to solve Poisson problem!
	*******************************************************************************/
	q=0.0E0;

	/* Computing the mesh size hx in x-direction */
	lx=bx-ax;
	hx=lx/nx;
	/* Computing the mesh size hy in y-direction */
	ly=by-ay;
	hy=ly/ny;

	/* Filling in the values of the TRUE solution u(x,y)=sin(2*pi*x)*sin(2*pi*y)+1
	in the mesh points into the array u
	Filling in the right-hand side f(x,y)=(8*pi*pi+q)*sin(2*pi*x)*sin(2*pi*y)+q
	in the mesh points into the array f.
	We choose the right-hand side to correspond to the TRUE solution of Poisson equation.
	Here we are using the mesh sizes hx and hy computed before to compute
	the coordinates (xi,yi) of the mesh points */
	for(iy=0;iy<=ny;iy++)
	{
		for(ix=0;ix<=nx;ix++)
		{
			xi=hx*ix/lx;
			yi=hy*iy/ly;

			cx=sin(2*pi*xi);
			cy=sin(2*pi*yi);

			u[ix+iy*(nx+1)]=1.0E0*cx*cy;
			f[ix+iy*(nx+1)]=(8.0E0*pi*pi)*u[ix+iy*(nx+1)];
			u[ix+iy*(nx+1)]=u[ix+iy*(nx+1)]+1.0E0;
		}
	}

	/* Setting the type of the boundary conditions on each side of the rectangular domain:
	On the boundary laying on the line x=0(=ax) Dirichlet boundary condition will be used
	On the boundary laying on the line x=1(=bx) Dirichlet boundary condition will be used
	On the boundary laying on the line y=0(=ay) Neumann boundary condition will be used
	On the boundary laying on the line y=1(=by) Neumann boundary condition will be used */
	BCtype = "DDNN";

	/* Setting the values of the boundary function G(x,y) that is equal to the TRUE solution
	in the mesh points laying on Dirichlet boundaries */
	for(iy=0;iy<=ny;iy++)
	{
		bd_ax[iy]=1.0E0;
		bd_bx[iy]=1.0E0;
	}
	/* Setting the values of the boundary function g(x,y) that is equal to the normal derivative
	of the TRUE solution in the mesh points laying on Neumann boundaries */
	for(ix=0;ix<=nx;ix++)
	{
		bd_ay[ix]=-2.0*pi*sin(2*pi*ix/nx);
		bd_by[ix]= 2.0*pi*sin(2*pi*ix/nx);
	}

	/* Initializing ipar array to make it free from garbage */
	for(i=0;i<128;i++)
	{
		ipar[i]=0;
	}

	/* Initializing simple data structures of Poisson Library for 2D Poisson Solver */
	d_init_Helmholtz_2D(&ax, &bx, &ay, &by, &nx, &ny, BCtype, &q, ipar, dpar, &stat);
	if (stat!=0) goto FAILURE;

	/* Initializing complex data structures of Poisson Library for 2D Poisson Solver
	NOTE: Right-hand side f may be altered after the Commit step. If you want to keep it,
	you should save it in another memory location! */
	d_commit_Helmholtz_2D(f, bd_ax, bd_bx, bd_ay, bd_by, &xhandle, ipar, dpar, &stat);
	if (stat!=0) goto FAILURE;

	/* Computing the approximate solution of 2D Poisson problem
	NOTE: Boundary data stored in the arrays bd_ax, bd_bx, bd_ay, bd_by should not be changed
	between the Commit step and the subsequent call to the Solver routine/*
	Otherwise the results may be wrong. */
	d_Helmholtz_2D(f, bd_ax, bd_bx, bd_ay, bd_by, &xhandle, ipar, dpar, &stat);
	if (stat!=0) goto FAILURE;

	/* Cleaning the memory used by xhandle */
	free_Helmholtz_2D(&xhandle, ipar, &stat);
	if (stat!=0) goto FAILURE;
	/* Now we can use xhandle to solve another 2D Poisson problem*/

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
	if (c1>1.0e-01)
	{
		printf("The computed solution seems to be inaccurate. ");
		goto FAILURE;
	}

	/* Success message to print if everything is OK */
	printf("\n Double precision 2D Poisson example has successfully PASSED\n");
	printf(" through all steps of computation!\n");

	/* Jumping over failure message */
	goto SUCCESS;

	/* Failure message to print if something went wrong */
FAILURE: printf("\nDouble precision 2D Poisson example FAILED to compute the solution...\n");

SUCCESS: return 0;

	/* End of the example code */
}
