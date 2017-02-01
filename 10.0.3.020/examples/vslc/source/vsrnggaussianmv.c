/*******************************************************************************
!                             INTEL CONFIDENTIAL
!  Copyright(C) 2003-2008 Intel Corporation. All Rights Reserved.
!  The source code contained  or  described herein and all documents related to
!  the source code ("Material") are owned by Intel Corporation or its suppliers
!  or licensors.  Title to the  Material remains with  Intel Corporation or its
!  suppliers and licensors. The Material contains trade secrets and proprietary
!  and  confidential  information of  Intel or its suppliers and licensors. The
!  Material  is  protected  by  worldwide  copyright  and trade secret laws and
!  treaty  provisions. No part of the Material may be used, copied, reproduced,
!  modified, published, uploaded, posted, transmitted, distributed or disclosed
!  in any way without Intel's prior express written permission.
!  No license  under any  patent, copyright, trade secret or other intellectual
!  property right is granted to or conferred upon you by disclosure or delivery
!  of the Materials,  either expressly, by implication, inducement, estoppel or
!  otherwise.  Any  license  under  such  intellectual property  rights must be
!  express and approved by Intel in writing.
!
!*******************************************************************************
!  Content:
!    vsRngGaussianMV  Example Program Text
!******************************************************************************/

#include <stdio.h>
#include <math.h>

#include "mkl.h"
#include "errcheck.inc"

//#define TL      // Uncomment this define to test Cholesky factorization

#define BRNG    VSL_BRNG_MCG31 // VSL basic generator to be used
#define SEED    7777777        // Initial value for stream initialization
#define NDIM    3              // Number of dimensions
#define N       10000          // Number of NDIM-dimensional vectors to generate
#define NN      5              // Number of NDIM-dimensional vectors to print

/*****************************************************
 Variance-covariance matrix for test
 (should be symmetric,positive-definite)
*****************************************************/
/* This is full storage for spotrf subroutine */
static float C[NDIM][NDIM]={ 16.0f, 8.0f,  4.0f,
                             8.0f, 13.0f, 17.0f,
                             4.0f, 17.0f, 62.0f  };

/* This is packed storage for spptrf subroutine */
static float C1[NDIM*(NDIM+1)/2]={ 16.0f, 8.0f, 4.0f, 13.0f, 17.0f, 62.0f };


/*****************************************************
 Variance-covariance matrix for test
 (should be symmetric,positive-definite)
*****************************************************/
static float a[NDIM]={ 3.0f, 5.0f, 2.0f };
static float r[N][NDIM];

main()
{
    VSLStreamStatePtr stream;

    float  TT[NDIM][NDIM]; // T*T'. Should be equal to var.-covar. matrix
    float  CC[NDIM][NDIM]; // Copy of C
    int    i, j, k, errcode;

    /* Following variables are used in Cholesky factorization subroutine */
    char    uplo;
    MKL_INT n;
    float*  T;
    MKL_INT lda;
    MKL_INT info;

    /* Sample characteristics */
    double dbMeanX, dbMeanY, dbMeanZ;
    double dbVarX, dbVarY, dbVarZ;
    double dbCovXY, dbCovXZ, dbCovYZ;

    double dbSX, dbSY, dbSZ, dbS2X, dbS2Y, dbS2Z, dbSXY, dbSXZ, dbSYZ;

    double dn = (double)N;
    double SX, SY, SZ;
    double D2X, D2Y, D2Z;
    double QX, QY, QZ;
    double DeltaMX,DeltaMY,DeltaMZ;
    double DeltaDX,DeltaDY,DeltaDZ;

    /*****************************************************
     Printing variance-covariance matrix and mean vector
    *****************************************************/
    printf("Variance-covariance matrix C:\n");
    for(i=0;i<NDIM;i++) {
        for(j=0;j<NDIM;j++) {
            CC[i][j] = C[i][j];
            printf("%4.1f ",C[i][j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Mean vector a:\n");
    for(i=0;i<NDIM;i++) {
        printf("%4.1f ",a[i]);
    }
    printf("\n\n");

    /*****************************************************
     Cholesky factorization
    *****************************************************/
    printf("VSL_MATRIX_STORAGE_FULL\n");
    printf("-----------------------\n\n");

    /* Full maxtrix storage (SPOTRF subroutine) */
    uplo = 'U'; // Due to FORTRAN stores matrices by columns while C by rows, ->
                // -> 'U' defines lower triangular matrix
    n=NDIM;
    lda=NDIM;
    T=(float*)C;

    spotrf( &uplo, &n, T, &lda, &info );

#if defined(TL)
    for(i=0;i<NDIM;i++) {
        for(j=0;j<NDIM;j++) {
            if(j>i) C[i][j] = 0.0f;
        }
    }

    /* Multiply T*T' */
    for(i=0;i<NDIM;i++) {
        for(j=0;j<NDIM;j++) {
            TT[i][j]=0.0;
            for(k=0;k<NDIM;k++) {
                TT[i][j]+=C[i][k]*C[j][k];
            }
        }
    }

    /* Printing T and TT matrices */
    printf("SPOTRF subroutine\n");
    printf("*****************\n");
    printf("Matrix T:              Matrix T*T' (should be equal to C):\n");
    for(i=0;i<NDIM;i++) {
        for(j=0;j<NDIM;j++) {
            printf("%5.2f ", C[i][j]);
        }
        printf("     ");
        for(j=0;j<NDIM;j++) {
            printf("%4.1f ", TT[i][j]);
        }
        printf("\n");
    }
    printf("\n");
#endif

    /*****************************************************
     Stream initialization
    *****************************************************/
    errcode = vslNewStream( &stream, BRNG, SEED );
    CheckVslError( errcode );

    /*****************************************************
     Generating random numbers
     from multivariate normal distribution
    *****************************************************/
    errcode = vsRngGaussianMV( VSL_METHOD_SGAUSSIANMV_BOXMULLER2, stream, N, (float *)r,NDIM, VSL_MATRIX_STORAGE_FULL, a, T );
    CheckVslError( errcode );

    /*****************************************************
     Printing random numbers
    *****************************************************/
    printf("Results (first %d of %d):\n", NN, N);
    printf("---------------------------\n");
    for(i=0;i<NN;i++) {
        printf("r[%d]=(",i);
        for(j=0;j<NDIM;j++) {
            printf("%5.2f ",r[i][j]);
        }
        printf(")\n");
    }
    printf("\n");

    /*****************************************************
     Testing sample characteristics
    *****************************************************/
    dbSX=dbSY=dbSZ=dbS2X=dbS2Y=dbS2Z=dbSXY=dbSXZ=dbSYZ=0.0;
    for(i=0;i<N;i++) {
        dbSX += r[i][0];
        dbS2X += r[i][0]*r[i][0];
        dbSY += r[i][1];
        dbS2Y += r[i][1]*r[i][1];
        dbSZ += r[i][2];
        dbS2Z += r[i][2]*r[i][2];
        dbSXY += r[i][0]*r[i][1];
        dbSXZ += r[i][0]*r[i][2];
        dbSYZ += r[i][1]*r[i][2];
    }
    dbMeanX = dbSX/(double)N;
    dbMeanY = dbSY/(double)N;
    dbMeanZ = dbSZ/(double)N;
    dbVarX = dbS2X/(double)N-dbMeanX*dbMeanX;
    dbVarY = dbS2Y/(double)N-dbMeanY*dbMeanY;
    dbVarZ = dbS2Z/(double)N-dbMeanZ*dbMeanZ;
    dbCovXY = dbSXY/(double)N-dbMeanX*dbMeanY;
    dbCovXZ = dbSXZ/(double)N-dbMeanX*dbMeanZ;
    dbCovYZ = dbSYZ/(double)N-dbMeanY*dbMeanZ;

    /* Printing */
    printf("Sample characteristics:\n");
    printf("-----------------------\n");
    printf("       Sample           Theory\n");
    printf("Mean :(%4.1f,%4.1f,%4.1f) (%4.1f,%4.1f,%4.1f)\n",dbMeanX,dbMeanY,dbMeanZ,a[0],a[1],a[2]);
    printf("Var. :(%4.1f,%4.1f,%4.1f) (%4.1f,%4.1f,%4.1f)\n",dbVarX,dbVarY,dbVarZ,CC[0][0],CC[1][1],CC[2][2]);
    printf("CovXY: %4.1f               %4.1f\n",dbCovXY,CC[0][1]);
    printf("CovXZ: %4.1f               %4.1f\n",dbCovXZ,CC[0][2]);
    printf("CovYZ: %4.1f               %4.1f\n\n\n",dbCovYZ,CC[1][2]);

    /* Checking results */
    D2X = CC[0][0]*CC[0][0];
    D2Y = CC[1][1]*CC[1][1];
    D2Z = CC[2][2]*CC[2][2];

    QX  = 3.0*D2X;
    QY  = 3.0*D2Y;
    QZ  = 3.0*D2Z;

    SX  = ((QX-D2X)/dn)-(2*(QX-2*D2X)/(dn*dn))+((QX-3*D2X)/(dn*dn*dn));
    SY  = ((QY-D2Y)/dn)-(2*(QY-2*D2Y)/(dn*dn))+((QY-3*D2Y)/(dn*dn*dn));
    SZ  = ((QZ-D2Z)/dn)-(2*(QZ-2*D2Z)/(dn*dn))+((QZ-3*D2Z)/(dn*dn*dn));

    DeltaMX = (a[0]-dbMeanX)/sqrt(CC[0][0]/dn);
    DeltaMY = (a[1]-dbMeanY)/sqrt(CC[1][1]/dn);
    DeltaMZ = (a[2]-dbMeanZ)/sqrt(CC[2][2]/dn);

    DeltaDX = (CC[0][0]-dbVarX)/sqrt(SX);
    DeltaDY = (CC[1][1]-dbVarY)/sqrt(SY);
    DeltaDZ = (CC[2][2]-dbVarZ)/sqrt(SZ);

    printf("\n");
    if(DeltaMX>3.0 || DeltaDX>3.0 || DeltaMY>3.0 || DeltaDY>3.0 || DeltaMZ>3.0 || DeltaDZ>3.0)
    {
        printf ("Error: sample moments\n");
        printf ("are disagreed with theory\n");
        printf ("     DeltaM: %7.3f %7.3f %7.3f \n", DeltaMX, DeltaMY, DeltaMZ);
        printf ("     DeltaD: %7.3f %7.3f %7.3f \n", DeltaDX, DeltaDY, DeltaDZ);
        printf ("     ( at least one of the Deltas > 3.0)\n");
        return 1;
    }
    else
    {
        printf ("Sample moments\n");
        printf ("are agreed with theory\n");
        printf ("     DeltaM: %7.3f %7.3f %7.3f \n", DeltaMX, DeltaMY, DeltaMZ);
        printf ("     DeltaD: %7.3f %7.3f %7.3f \n", DeltaDX, DeltaDY, DeltaDZ);
        printf ("     ( All Deltas < 3.0)\n");
    }
    printf("\n");

    /*****************************************************
     Stream finalization
    *****************************************************/
    errcode = vslDeleteStream( &stream );
    CheckVslError( errcode );

    /*****************************************************
     Cholesky factorization
    *****************************************************/
    /* Packed maxtrix storage (SPPTRF subroutine) */
    uplo='L';
    n=NDIM;
    T=(float*)C1;
    k=0;

    spptrf( &uplo, &n, T, &info );

    printf("VSL_MATRIX_STORAGE_PACKED\n");
    printf("-------------------------\n\n");

#if defined(TL)
    /* Printing T (lower triangular matrix) */
    for(j=0;j<NDIM;j++) {
        for(i=0;i<NDIM;i++) {
            if (i<j) C[i][j]=0.0f;
            else C[i][j]=T[k++];
        }
    }

    /* Multiply T*T' */
    for(i=0;i<NDIM;i++) {
        for(j=0;j<NDIM;j++) {
            TT[i][j]=0.0;
            for(k=0;k<NDIM;k++) {
                TT[i][j]+=C[i][k]*C[j][k];
            }
        }
    }

    /* Printing matrix TT */
    printf("SPPTRF subroutine\n");
    printf("*****************\n");
    printf("Matrix T:              Matrix T*T' (should be equal to C):\n");
    for(i=0;i<NDIM;i++) {
        for(j=0;j<NDIM;j++) {
            printf("%5.2f ",C[i][j]);
        }
        printf("     ");
        for(j=0;j<NDIM;j++) {
            printf("%4.1f ", TT[i][j]);
        }
        printf("\n");
    }
    printf("\n");
#endif

    /*****************************************************
     Stream initialization
    *****************************************************/
    errcode = vslNewStream( &stream, BRNG, SEED );
    CheckVslError( errcode );

    /*****************************************************
     Generating random numbers
     from multivariate normal distribution
    *****************************************************/
    errcode = vsRngGaussianMV( VSL_METHOD_SGAUSSIANMV_BOXMULLER2, stream, N, (float *)r,NDIM, VSL_MATRIX_STORAGE_PACKED, a, T );
    CheckVslError( errcode );

    /*****************************************************
     Printing random numbers
    *****************************************************/
    printf("Results (first %d of %d):\n", NN, N);
    printf("---------------------------\n");
    for(i=0;i<NN;i++) {
        printf("r[%d]=(",i);
        for(j=0;j<NDIM;j++) {
            printf("%5.2f ",r[i][j]);
        }
        printf(")\n");
    }
    printf("\n");

    /*****************************************************
     Testing sample characteristics
    *****************************************************/
    dbSX=dbSY=dbSZ=dbS2X=dbS2Y=dbS2Z=dbSXY=dbSXZ=dbSYZ=0.0;
    for(i=0;i<N;i++) {
        dbSX += r[i][0];
        dbS2X += r[i][0]*r[i][0];
        dbSY += r[i][1];
        dbS2Y += r[i][1]*r[i][1];
        dbSZ += r[i][2];
        dbS2Z += r[i][2]*r[i][2];
        dbSXY += r[i][0]*r[i][1];
        dbSXZ += r[i][0]*r[i][2];
        dbSYZ += r[i][1]*r[i][2];
    }
    dbMeanX = dbSX/(double)N;
    dbMeanY = dbSY/(double)N;
    dbMeanZ = dbSZ/(double)N;
    dbVarX = dbS2X/(double)N-dbMeanX*dbMeanX;
    dbVarY = dbS2Y/(double)N-dbMeanY*dbMeanY;
    dbVarZ = dbS2Z/(double)N-dbMeanZ*dbMeanZ;
    dbCovXY = dbSXY/(double)N-dbMeanX*dbMeanY;
    dbCovXZ = dbSXZ/(double)N-dbMeanX*dbMeanZ;
    dbCovYZ = dbSYZ/(double)N-dbMeanY*dbMeanZ;

    /* Printing */
    printf("Sample characteristics:\n");
    printf("-----------------------\n");
    printf("       Sample           Theory\n");
    printf("Mean :(%4.1f,%4.1f,%4.1f) (%4.1f,%4.1f,%4.1f)\n",dbMeanX,dbMeanY,dbMeanZ,a[0],a[1],a[2]);
    printf("Var. :(%4.1f,%4.1f,%4.1f) (%4.1f,%4.1f,%4.1f)\n",dbVarX,dbVarY,dbVarZ,CC[0][0],CC[1][1],CC[2][2]);
    printf("CovXY: %4.1f               %4.1f\n",dbCovXY,CC[0][1]);
    printf("CovXZ: %4.1f               %4.1f\n",dbCovXZ,CC[0][2]);
    printf("CovYZ: %4.1f               %4.1f\n",dbCovYZ,CC[1][2]);

    /* Checking results */
    DeltaMX = (a[0]-dbMeanX)/sqrt(CC[0][0]/dn);
    DeltaMY = (a[1]-dbMeanY)/sqrt(CC[1][1]/dn);
    DeltaMZ = (a[2]-dbMeanZ)/sqrt(CC[2][2]/dn);

    DeltaDX = (CC[0][0]-dbVarX)/sqrt(SX);
    DeltaDY = (CC[1][1]-dbVarY)/sqrt(SY);
    DeltaDZ = (CC[2][2]-dbVarZ)/sqrt(SZ);

    printf("\n");
    if(DeltaMX>3.0 || DeltaDX>3.0 || DeltaMY>3.0 || DeltaDY>3.0 || DeltaMZ>3.0 || DeltaDZ>3.0)
    {
        printf ("Error: sample moments\n");
        printf ("are disagreed with theory\n");
        printf ("     DeltaM: %7.3f %7.3f %7.3f \n", DeltaMX, DeltaMY, DeltaMZ);
        printf ("     DeltaD: %7.3f %7.3f %7.3f \n", DeltaDX, DeltaDY, DeltaDZ);
        printf ("     ( at least one of the Deltas > 3.0)\n");
        return 1;
    }
    else
    {
        printf ("Sample moments\n");
        printf ("are agreed with theory\n");
        printf ("     DeltaM: %7.3f %7.3f %7.3f \n", DeltaMX, DeltaMY, DeltaMZ);
        printf ("     DeltaD: %7.3f %7.3f %7.3f \n", DeltaDX, DeltaDY, DeltaDZ);
        printf ("     ( All Deltas < 3.0)\n");
    }

    /*****************************************************
     Stream finalization
    *****************************************************/
    errcode = vslDeleteStream( &stream );
    CheckVslError( errcode );

    return 0;
}
