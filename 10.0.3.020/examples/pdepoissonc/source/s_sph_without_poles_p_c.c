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
/*   in any way without Intel's prior express written permission.
/*   No license  under any  patent, copyright, trade secret or other intellectual
/*   property right is granted to or conferred upon you by disclosure or delivery
/*   of the Materials,  either expressly, by implication, inducement, estoppel or
/*   otherwise.  Any  license  under  such  intellectual property  rights must be
/*   express and approved by Intel in writing.
/*
/*******************************************************************************
/*  Content:
/*  C single precision example of solving Helmholtz problem on a whole sphere
/*  excluding poles using MKL Poisson Library
/*
/*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/* Include Poisson Library header files */
#include "mkl_poisson.h"

int main(void)
{
	/* Note that the size of the transform np must be even !!! */
	MKL_INT np=8, nt=8;
	float pi=3.14159265358979324;

	MKL_INT ip, it, i, stat;
	MKL_INT ipar[128];
	float ap, bp, at, bt, lp, lt, hp, ht, theta_i, ct, c1;
	float *spar, *f, *u;
	float q;
	DFTI_DESCRIPTOR_HANDLE handle_s = 0;
	DFTI_DESCRIPTOR_HANDLE handle_c = 0;

	/* Printing the header for the example */
	printf("\n Example of use of MKL Poisson Library\n");
	printf(" **********************************************\n\n");
	printf(" This example gives the solution of Helmholtz problem on a sphere\n");
	printf(" excluding poles 0<p<2*pi, 0.1<t<pi-0.1, with Helmholtz coefficient q=1\n");
	printf(" and right-hand side f(p,t)=(t-0.1)*(pi-0.1-t)+2-(pi-2*t)*cos(t)/sin(t)\n");
	printf(" -----------------------------------------------------------------------\n");
	printf(" In general, the error should be of order O(1.0/np^2+1.0/nt^2)\n");
	printf(" For this example, the value of np=nt is %d\n", np);
	printf(" The approximation error should be of order 3.5e-02, if everything is OK\n");
	printf(" -----------------------------------------------------------------------\n");
	printf("   Note that np should be even to solve the PERIODIC problem!\n");
	printf(" -----------------------------------------------------------------------\n");
	printf("                      SINGLE PRECISION COMPUTATIONS                     \n");
	printf(" =======================================================================\n\n");

	spar=(float*)malloc((5*np/2+nt+10)*sizeof(float));
	f=(float*)malloc((np+1)*(nt+1)*sizeof(float));
	u=(float*)malloc((np+1)*(nt+1)*sizeof(float));

	/* Defining the rectangular domain on a sphere 0<p<2*pi, 0.1<t<pi-0.1 for Helmholtz Solver on a sphere */
	/* Poisson Library will automatically detect that this problem is periodic along phi-direction only */
	ap=0.0E0;
	bp=2*pi;
	at=0.1E0;
	bt=pi-0.1E0;

	/* Setting the coefficient q to 1.0E0 for Helmholtz problem */
	/* If you like to solve Poisson problem, please set q to 0.0E0 */
	q=1.0E0;

	/* Computing the mesh size hp in phi-direction */
	lp=bp-ap;
	hp=lp/np;
	/* Computing the mesh size ht in theta-direction */
	lt=bt-at;
	ht=lt/nt;

	/* Filling in the values of the TRUE solution u(p,t)=(t-0.1)*(pi-0.1-t)
	in the mesh points into the array u
	Filling in the right-hand side f(p,t)=(t-0.1)*(pi-0.1-t)+2-(pi-2*t)*cos(t)/sin(t)
	in the mesh points into the array f.
	We choose the right-hand side to correspond to the TRUE solution of Helmholtz equation on a sphere.
	Here we are using the mesh sizes hp and ht computed before to compute
	the coordinates (phi_i,theta_i) of the mesh points */
	for(it=0;it<=nt;it++)
	{
		for(ip=0;ip<=np;ip++)
		{
			theta_i=ht*it+at;
			ct=(theta_i-at)*(bt-theta_i);
			u[ip+it*(np+1)]=ct;
			f[ip+it*(np+1)]=q*ct+2.0E0-(cos(theta_i)/sin(theta_i))*(at+bt-2*theta_i);
		}
	}

	for(ip=0;ip<=np;ip++)
	{
		f[ip          ]=0.0E0;
		f[ip+nt*(np+1)]=0.0E0;
	}

	/* Initializing ipar array to make it free from garbage */
	for(i=0;i<128;i++)
	{
		ipar[i]=0;
	}

	/* Initializing simple data structures of Poisson Library for Helmholtz Solver on a sphere */
	/* As we are looking for the solution on a whole interval over phi, this is a PERIDOC problem */
	/* Therefore, the routines ending with "_p" are used to find the solution */
	s_init_sph_p(&ap,&bp,&at,&bt,&np,&nt,&q,ipar,spar,&stat);
	if (stat!=0) goto FAILURE;

	/* Initializing complex data structures of Poisson Library for Helmholtz Solver on a sphere
	NOTE: Right-hand side f may be altered after the Commit step. If you want to keep it,
	you should save it in another memory location! */
	s_commit_sph_p(f,&handle_s,&handle_c,ipar,spar,&stat);
	if (stat!=0) goto FAILURE;

	/* Computing the approximate solution of Helmholtz problem on a sphere without poles */
	s_sph_p(f,&handle_s,&handle_c,ipar,spar,&stat);
	if (stat!=0) goto FAILURE;

	/* Cleaning the memory used by handle_s and handle_c */
	free_sph_p(&handle_s,&handle_c,ipar,&stat);
	if (stat!=0) goto FAILURE;
	/* Now we can use handle_s and handle_c to solve another Helmholtz problem */
	/* after a proper initialization */

	/* Printing the results */
	printf("The number of mesh intervals in phi-direction is np=%d\n", np);
	printf("The number of mesh intervals in theta-direction is nt=%d\n\n", nt);

	/* Watching the error along the line phi=hp */
	ip=1;
	c1=0.0;
	for(it=0;it<=nt;it++)
	{
		printf("In the mesh point (%5.3f,%5.3f) the error between the computed and the true solution is equal to %10.3e\n", ip*hp, it*ht, f[ip+it*(np+1)]-u[ip+it*(np+1)]);
		if (c1<fabs(f[ip+it*(np+1)]-u[ip+it*(np+1)])) c1 = fabs(f[ip+it*(np+1)]-u[ip+it*(np+1)]);
	}

	if (c1>3.5e-02)
	{
		printf("The computed solution seems to be inaccurate. ");
		goto FAILURE;
	}
	/* Success message to print if everything is OK */
	printf("\n Single precision Helmholtz example on a sphere without poles has successfully PASSED\n");
	printf(" through all steps of computation!\n");

	/* Jumping over failure message */
	goto SUCCESS;

	/* Failure message to print if something went wrong */
FAILURE: printf("\nSingle precision Helmholtz example on a sphere without poles has FAILED to compute the solution...\n");

SUCCESS: return 0;

	/* End of the example code */
}
