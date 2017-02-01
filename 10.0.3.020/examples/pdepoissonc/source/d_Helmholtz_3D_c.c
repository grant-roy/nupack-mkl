/*******************************************************************************
/*                              INTEL CONFIDENTIAL
/*  Copyright(C) 2006-2008 Intel Corporation. All Rights Reserved.
/*  The source code contained  or  described herein and all documents related to
/*  the source code ("Material") are owned by Intel Corporation or its suppliers
/*  or licensors.  Title to the  Material remains with  Intel Corporation or its
/*  suppliers and licensors. The Material contains trade secrets and proprietary
/*  and  confidential  information of  Intel or its suppliers and licensors. The
/*  Material  is  protected  by  worldwide  copyright  and trade secret laws and
/*  treaty  provisions. No part of the Material may be used, copied, reproduced,
/*  modified, published, uploaded, posted, transmitted, distributed or disclosed
/*  in any way without Intel");s prior express written permission.
/*  No license  under any  patent, copyright, trade secret or other intellectual
/*  property right is granted to or conferred upon you by disclosure or delivery
/*  of the Materials,  either expressly, by implication, inducement, estoppel or
/*  otherwise.  Any  license  under  such  intellectual property  rights must be
/*  express and approved by Intel in writing.
/*
/*******************************************************************************
/*  Content:
/*  C double precision precision example of solving 3D Helmholtz problem in a
/*  parallelepiped domain using MKL Poisson Library
/*
/*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/* Include Poisson Library header files */
#include "mkl_poisson.h"

int main(void)
{

	MKL_INT nx=4, ny=4, nz=4;
	double pi=3.14159265358979324;

	MKL_INT ix, iy, iz, i, stat;
	MKL_INT ipar[128];
	double ax, bx, ay, by, az, bz, lx, ly, lz, hx, hy, hz, xi, yi, zi, cx, cy, cz, c1;
	double *dpar, *f, *u, *bd_ax, *bd_bx, *bd_ay, *bd_by, *bd_az, *bd_bz;
	double q;
	DFTI_DESCRIPTOR_HANDLE xhandle = 0;
	DFTI_DESCRIPTOR_HANDLE yhandle = 0;
	char *BCtype;

	/* Printing the header for the example */
	printf("\n Example of use of MKL Poisson Library\n");
	printf(" **********************************************\n\n");
	printf(" This example gives the solution of 3D Helmholtz problem\n");
	printf(" with the equation -u_xx-u_yy-u_zz+u=f(x,y,z), 0<x<1, 0<y<1, 0<z<1,\n");
	printf(" f(x,y,z)=(12*pi*pi+1)*sin(2*pi*x)*sin(2*pi*y)*sin(2*pi*z)+1,\n");
	printf(" and with the following boundary conditions:\n");
	printf("  u(0,y,z)=u(1,y,z)=1 (Dirichlet boundary conditions),\n");
	printf(" -u_y(x,0,z)=-2.0*pi*sin(2*pi*x)*sin(2*pi*z) (Neumann boundary condition),\n");
	printf("  u_y(x,1,z)= 2.0*pi*sin(2*pi*x)*sin(2*pi*z) (Neumann boundary condition),\n");
	printf(" -u_z(x,y,0)=-2.0*pi*sin(2*pi*x)*sin(2*pi*y) (Neumann boundary condition),\n");
	printf("  u_z(x,y,1)= 2.0*pi*sin(2*pi*x)*sin(2*pi*y) (Neumann boundary condition).\n");
	printf(" -----------------------------------------------------------------------\n");
	printf(" In general, the error should be of order O(1.0/nx^2+1.0/ny^2+1.0/nz^2)\n");
	printf(" For this example, the value of nx=ny=nz is %d\n", nx);
	printf(" The approximation error should be of order 5.0e-01, if everything is OK\n");
	printf(" -----------------------------------------------------------------------\n");
	printf("                      DOUBLE PRECISION COMPUTATIONS                     \n");
	printf(" =======================================================================\n\n");

	dpar=(double*)malloc((13*(nx+ny)/2+9)*sizeof(double));
	f=(double*)malloc((nx+1)*(ny+1)*(nz+1)*sizeof(double));
	u=(double*)malloc((nx+1)*(ny+1)*(nz+1)*sizeof(double));
	bd_ax=(double*)malloc((ny+1)*(nz+1)*sizeof(double));
	bd_bx=(double*)malloc((ny+1)*(nz+1)*sizeof(double));
	bd_ay=(double*)malloc((nx+1)*(nz+1)*sizeof(double));
	bd_by=(double*)malloc((nx+1)*(nz+1)*sizeof(double));
	bd_az=(double*)malloc((nx+1)*(ny+1)*sizeof(double));
	bd_bz=(double*)malloc((nx+1)*(ny+1)*sizeof(double));

	/* Defining the parallelepiped domain 0<x<1, 0<y<1, 0<z<1 for 3D Helmholtz Solver */
	ax=0.0E0;
	bx=1.0E0;
	ay=0.0E0;
	by=1.0E0;
	az=0.0E0;
	bz=1.0E0;

	/* Setting the coefficient q to 1 */
	q=1.0E0;

	/* Computing the mesh size hx in x-direction */
	lx=bx-ax;
	hx=lx/nx;
	/* Computing the mesh size hy in y-direction */
	ly=by-ay;
	hy=ly/ny;
	/* Computing the mesh size hx in z-direction */
	lz=bz-az;
	hz=lz/nz;

	/* Filling in the values of the TRUE solution u(x,y,z)=sin(2*pi*x)*sin(2*pi*y)*sin(2*pi*z)+1
	in the mesh points into the array u
	Filling in the right-hand side f(x,y,z)=(12*pi*pi+q)*sin(2*pi*x)*sin(2*pi*y)*sin(2*pi*z)+q
	in the mesh points into the array f.
	We choose the right-hand side to correspond to the TRUE solution of Helmholtz equation.
	Here we are using the mesh sizes hx, hy, and hz computed before to compute
	the coordinates (xi,yi,zi) of the mesh points */
	for(iz=0;iz<=nz;iz++)
	{
		for(iy=0;iy<=ny;iy++)
		{
			for(ix=0;ix<=nx;ix++)
			{
				xi=hx*ix/lx;
				yi=hy*iy/ly;
				zi=hz*iz/lz;

				cx=sin(2*pi*xi);
				cy=sin(2*pi*yi);
				cz=sin(2*pi*zi);

				u[ix+iy*(nx+1)+iz*(nx+1)*(ny+1)]=1.0E0*cx*cy*cz;
				f[ix+iy*(nx+1)+iz*(nx+1)*(ny+1)]=((12.0E0*pi*pi)+q)*u[ix+iy*(nx+1)+iz*(nx+1)*(ny+1)]+q;
				u[ix+iy*(nx+1)+iz*(nx+1)*(ny+1)]=u[ix+iy*(nx+1)+iz*(nx+1)*(ny+1)]+1.0E0;
			}
		}
	}

	/* Setting the type of the boundary conditions on each surface of the parallelepiped domain:
	On the boundary laying on the plane x=0(=ax) Dirichlet boundary condition will be used
	On the boundary laying on the plane x=1(=bx) Dirichlet boundary condition will be used
	On the boundary laying on the plane y=0(=ay) Neumann boundary condition will be used
	On the boundary laying on the plane y=1(=by) Neumann boundary condition will be used
	On the boundary laying on the plane z=0(=az) Neumann boundary condition will be used
	On the boundary laying on the plane z=1(=bz) Neumann boundary condition will be used */
	BCtype = "DDNNNN";

	/* Setting the values of the boundary function G(x,y,z) that is equal to the TRUE solution
	in the mesh points laying on Dirichlet boundaries */
	for(iy=0;iy<=ny;iy++)
	{
		for(iz=0;iz<=nz;iz++)
		{
			bd_ax[iy+iz*(ny+1)]=1.0E0;
			bd_bx[iy+iz*(ny+1)]=1.0E0;
		}
	}
	/* Setting the values of the boundary function g(x,y,z) that is equal to the normal derivative
	of the TRUE solution in the mesh points laying on Neumann boundaries */
	for(ix=0;ix<=nx;ix++)
	{
		for(iz=0;iz<=nz;iz++)
		{
			bd_ay[ix+iz*(nx+1)]=-2.E0*pi*sin(2*pi*ix/nx)*sin(2*pi*iz/nz);
			bd_by[ix+iz*(nx+1)]= 2.E0*pi*sin(2*pi*ix/nx)*sin(2*pi*iz/nz);
		}
	}
	for(ix=0;ix<=nx;ix++)
	{
		for(iy=0;iy<=ny;iy++)
		{
			bd_az[ix+iy*(nx+1)]=-2.E0*pi*sin(2*pi*ix/nx)*sin(2*pi*iy/ny);
			bd_bz[ix+iy*(nx+1)]= 2.E0*pi*sin(2*pi*ix/nx)*sin(2*pi*iy/ny);
		}
	}

	/* Initializing ipar array to make it free from garbage */
	for(i=0;i<128;i++)
	{
		ipar[i]=0;
	}

	/* Initializing simple data structures of Poisson Library for 3D Helmholtz Solver */
	d_init_Helmholtz_3D(&ax, &bx, &ay, &by, &az, &bz, &nx, &ny, &nz, BCtype, &q, ipar, dpar, &stat);
	if (stat!=0) goto FAILURE;

	/* Initializing complex data structures of Poisson Library for 3D Helmholtz Solver
	NOTE: Right-hand side f may be altered after the Commit step. If you want to keep it,
	you should save it in another memory location! */
	d_commit_Helmholtz_3D(f, bd_ax, bd_bx, bd_ay, bd_by, bd_az, bd_bz, &xhandle, &yhandle, ipar, dpar, &stat);
	if (stat!=0) goto FAILURE;

	/* Computing the approximate solution of 3D Helmholtz problem
	NOTE: Boundary data stored in the arrays bd_ax, bd_bx, bd_ay, bd_by, bd_az, bd_bz should not be changed
	between the Commit step and the subsequent call to the Solver routine!
	Otherwise the results may be wrong. */
	d_Helmholtz_3D(f, bd_ax, bd_bx, bd_ay, bd_by, bd_az, bd_bz, &xhandle, &yhandle, ipar, dpar, &stat);
	if (stat!=0) goto FAILURE;

	/* Cleaning the memory used by xhandle and yhandle */
	free_Helmholtz_3D(&xhandle, &yhandle, ipar, &stat);
	if (stat!=0) goto FAILURE;
	/* Now we can use xhandle and yhandle to solve another 3D Helmholtz problem */

	/* Printing the results */
	printf("The number of mesh intervals in x-direction is nx=%d\n", nx);
	printf("The number of mesh intervals in y-direction is ny=%d\n", ny);
	printf("The number of mesh intervals in z-direction is nz=%d\n\n", nz);

	/* Watching the error in the plane x=hx */
	ix=1;
	c1=0.0;
	for(iy=0;iy<=ny;iy++)
	{
		for(iz=0;iz<=nz;iz++)
		{
			printf("In the mesh point (%4.2f,%4.2f,%4.2f) the error between the computed and the true solution is equal to %10.3e\n", ix*hx, iy*hy, iz*hz, f[ix+iy*(nx+1)+iz*(nx+1)*(ny+1)]-u[ix+iy*(nx+1)+iz*(nx+1)*(ny+1)]);
			if (c1<fabs(f[ix+iy*(nx+1)+iz*(nx+1)*(ny+1)]-u[ix+iy*(nx+1)+iz*(nx+1)*(ny+1)])) c1 = fabs(f[ix+iy*(nx+1)+iz*(nx+1)*(ny+1)]-u[ix+iy*(nx+1)+iz*(nx+1)*(ny+1)]);
		}
	}
	if (c1>5.0e-01)
	{
		printf("The computed solution seems to be inaccurate. ");
		goto FAILURE;
	}

	/* Success message to print if everything is OK */
	printf("\n Double precision 3D Helmholtz example has successfully PASSED\n");
	printf(" through all steps of computation!\n");

	/* Jumping over failure message */
	goto SUCCESS;

	/* Failure message to print if something went wrong */
FAILURE: printf("\nDouble precision 3D Helmholtz example FAILED to compute the solution...\n");

SUCCESS: return 0;

	/* End of the example code */
}
