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
!******************************************************************************/

import com.intel.mkl.*;

public final class vsrnggaussianmv {
    /**
     * Incarnation prohibited.
     */
    private vsrnggaussianmv() {}

    /**
     * Individual name of this test.
     */
    private final static String test_name = "vsrnggaussianmv";

    /**
     * No command-line options.
     */
    public static void main(String[] args) {
        try {
            test();
        } catch (Exception statusException) {
            statusException.printStackTrace(System.out);
            System.out.println("\nTEST FAILED");
            System.exit(1);
        }
        System.out.println("\nTEST PASSED");
    }

    /**
     * Demonstrate the VSL functions and test them.
     */
    public static void test() throws Exception
    {
		// Parameters
        final int SEED = 7777777;			// Initial value for stream initialization
        final int BRNG = VSL.BRNG_MCG31;	// VSL basic generator to be used
        final int METHOD = VSL.METHOD_DGAUSSIANMV_BOXMULLER2;
        final int STORAGE_FULL = VSL.MATRIX_STORAGE_FULL;
        final int STORAGE_PACK = VSL.MATRIX_STORAGE_PACKED;
        final int NDIM = 3;					// Number of dimensions
        final int N = 1000;					// Number of NDIM-dimensional vectors to generate
        final int NN = 5;					// Number of NDIM-dimensional vectors to print

		/*****************************************************
		 Variance-covariance matrix for test
		 (should be symmetric,positive-definite)
		*****************************************************/
		/* This is full storage for dpotrf subroutine */
		//float C[NDIM][NDIM]={ 16.0f, 8.0f,  4.0f,
		float[][] C = { {16.0f, 8.0f,  4.0f},
						{8.0f, 13.0f, 17.0f},
						{4.0f, 17.0f, 62.0f}  };

		/* This is packed storage for dpptrf subroutine */
		//float C1[NDIM*(NDIM+1)/2]={ 16.0f, 8.0f, 4.0f, 13.0f, 17.0f, 62.0f };
		float[] C1={ 16.0f, 8.0f, 4.0f, 13.0f, 17.0f, 62.0f };

		/*****************************************************
		 Variance-covariance matrix for test
		 (should be symmetric,positive-definite)
		*****************************************************/
		//float a[NDIM]={ 3.0f, 5.0f, 2.0f };
		float[] a={ 3.0f, 5.0f, 2.0f };
		float[][] r = new float [N][NDIM];

		//////////////////////////////////////////////////////
		float[][] TT = new float [NDIM][NDIM]; // T*T'. Should be equal to var.-covar. matrix
		float[][] CC = new float [NDIM][NDIM]; // Copy of C
		int    i, j, k;

		/* Following variables are used in Cholesky factorization subroutine */
		char    uplo;
		int n;
		float[] T;
		int lda;
		int info;

		/* Sample characteristics */
		float dbMeanX, dbMeanY, dbMeanZ;
		float dbVarX, dbVarY, dbVarZ;
		float dbCovXY, dbCovXZ, dbCovYZ;

		float dbSX, dbSY, dbSZ, dbS2X, dbS2Y, dbS2Z, dbSXY, dbSXZ, dbSYZ;

		float dn = (float)N;
		float SX, SY, SZ;
		float D2X, D2Y, D2Z;
		float QX, QY, QZ;
		float DeltaMX,DeltaMY,DeltaMZ;
		float DeltaDX,DeltaDY,DeltaDZ;

		System.out.println("");
		System.out.println("Sample of vsRngGaussianMV.");
		System.out.println("----------------------");

		/*****************************************************
		 Printing variance-covariance matrix and mean vector
		 Also making the copy CC from C
		*****************************************************/
		System.out.println("Variance-covariance matrix C:");
		for(i=0;i<NDIM;i++) {
			for(j=0;j<NDIM;j++) {
				CC[i][j] = C[i][j];
				System.out.print("" + C[i][j] + " ");
			}
			System.out.println("");
		}
		System.out.println("");
		System.out.println("Mean vector a:");
		for(i=0;i<NDIM;i++) {
			System.out.print("" + a[i] + " ");
		}
		System.out.println("");
		System.out.println("");

		/*****************************************************
		 Cholesky factorization -
		 transform source symmetric matrix
		 to lower triangular one
		*****************************************************/
		System.out.println("VSL_MATRIX_STORAGE_FULL");
		System.out.println("-----------------------");
		System.out.println("");

		/* Full maxtrix storage (SPOTRF subroutine) */
		uplo = 'U'; // Due to FORTRAN stores matrices by columns while C by rows, ->
					// -> 'U' defines lower triangular matrix
		n=NDIM;
		lda=NDIM;
//		T=(float*)C;
		T = new float[NDIM*NDIM];
		for(i=0;i<NDIM;i++) {
			for(j=0;j<NDIM;j++) {
				T[i*NDIM+j] = C[i][j];
			}
		}

		/* MKL Choelsky factorization routine call */
//		dpotrf( &uplo, &n, T, &lda, &info );

		/*****************************************************
		 Stream initialization
		*****************************************************/
		VSLStreamStatePtr stream = new VSLStreamStatePtr();
		int errcode = VSL.vslNewStream(stream,BRNG, SEED);

		/*****************************************************
		 Generating random numbers
		 from multivariate normal distribution
		*****************************************************/
		{
		float[] r1d = new float[N*NDIM];
		errcode = VSL.vsRngGaussianMV( METHOD, stream, N, r1d, NDIM, STORAGE_FULL, a, T );
		ErrCheck.CheckVslError( errcode );
		for(i=0;i<N;i++) {
			for(j=0;j<NDIM;j++) {
				r[i][j] = r1d[i*NDIM+j];
			}
		}
		}

		/*****************************************************
		 Printing random numbers
		*****************************************************/
		System.out.println("Results (first " + NN + " of " + N + "):");
		System.out.println("---------------------------");
		for(i=0;i<NN;i++) {
			System.out.print("r[" + i + "]=(");
			for(j=0;j<NDIM;j++) {
				System.out.print("" + r[i][j] + " ");
			}
			System.out.println(")");
		}
		System.out.println("");

		/*****************************************************
		 Testing sample characteristics
		*****************************************************/
		dbSX=dbSY=dbSZ=dbS2X=dbS2Y=dbS2Z=dbSXY=dbSXZ=dbSYZ=0.0f;
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
		dbMeanX = dbSX/(float)N;
		dbMeanY = dbSY/(float)N;
		dbMeanZ = dbSZ/(float)N;
		dbVarX = dbS2X/(float)N-dbMeanX*dbMeanX;
		dbVarY = dbS2Y/(float)N-dbMeanY*dbMeanY;
		dbVarZ = dbS2Z/(float)N-dbMeanZ*dbMeanZ;
		dbCovXY = dbSXY/(float)N-dbMeanX*dbMeanY;
		dbCovXZ = dbSXZ/(float)N-dbMeanX*dbMeanZ;
		dbCovYZ = dbSYZ/(float)N-dbMeanY*dbMeanZ;

		/* Printing */
		System.out.println("Sample characteristics:");
		System.out.println("-----------------------");
		System.out.println("       Sample           Theory");
		System.out.println("Mean :(" + dbMeanX + "," + dbMeanY + "," + dbMeanZ + ") (" + a[0] + "," + a[1] + "," + a[2] + ")");
		System.out.println("Var. :(" + dbVarX + "," + dbVarY + "," + dbVarZ + ") (" + CC[0][0] + "," + CC[1][1] + "," + CC[2][2] + ")");
		System.out.println("CovXY: " + dbCovXY + "               " + CC[0][1]);
		System.out.println("CovXZ: " + dbCovXZ + "               " + CC[0][2]);
		System.out.println("CovYZ: " + dbCovYZ + "               " + CC[1][2]);
		System.out.println("");

		/* Checking results */
		D2X = CC[0][0]*CC[0][0];
		D2Y = CC[1][1]*CC[1][1];
		D2Z = CC[2][2]*CC[2][2];

		QX  = 3.0f*D2X;
		QY  = 3.0f*D2Y;
		QZ  = 3.0f*D2Z;

		SX  = ((QX-D2X)/dn)-(2*(QX-2*D2X)/(dn*dn))+((QX-3*D2X)/(dn*dn*dn));
		SY  = ((QY-D2Y)/dn)-(2*(QY-2*D2Y)/(dn*dn))+((QY-3*D2Y)/(dn*dn*dn));
		SZ  = ((QZ-D2Z)/dn)-(2*(QZ-2*D2Z)/(dn*dn))+((QZ-3*D2Z)/(dn*dn*dn));

		DeltaMX = (a[0]-dbMeanX)/(float)Math.sqrt(CC[0][0]/dn);
		DeltaMY = (a[1]-dbMeanY)/(float)Math.sqrt(CC[1][1]/dn);
		DeltaMZ = (a[2]-dbMeanZ)/(float)Math.sqrt(CC[2][2]/dn);

		DeltaDX = (CC[0][0]-dbVarX)/(float)Math.sqrt(SX);
		DeltaDY = (CC[1][1]-dbVarY)/(float)Math.sqrt(SY);
		DeltaDZ = (CC[2][2]-dbVarZ)/(float)Math.sqrt(SZ);

		/* All deltas must be less than 3.0f */
		if(DeltaMX>3.0f || DeltaDX>3.0f || DeltaMY>3.0f || DeltaDY>3.0f || DeltaMZ>3.0f || DeltaDZ>3.0f)
		{
			System.out.println ("Error: sample moments");
			System.out.println ("are disagreed with theory");
			System.out.println ("     DeltaM: " + DeltaMX + " " + DeltaMY + " " + DeltaMZ);
			System.out.println ("     DeltaD: " + DeltaDX + " " + DeltaDY + " " + DeltaDZ);
			System.out.println ("     ( at least one of the Deltas > 3.0)");
		} else {
			System.out.println ("Sample moments");
			System.out.println ("are agreed with theory");
			System.out.println ("     DeltaM: " + DeltaMX + " " + DeltaMY + " " + DeltaMZ);
			System.out.println ("     DeltaD: " + DeltaDX + " " + DeltaDY + " " + DeltaDZ);
			System.out.println ("     ( All Deltas < 3.0)");
		}
		System.out.println("");

		/*****************************************************
		 Stream finalization
		*****************************************************/
		errcode = VSL.vslDeleteStream( stream );
		ErrCheck.CheckVslError( errcode );

		/*****************************************************
		 Cholesky factorization
		*****************************************************/
		/* Packed maxtrix storage (SPPTRF subroutine) */
		uplo='L';
		n=NDIM;
//		T=(float*)C1;
		T = new float[NDIM*(NDIM+1)/2];
		for(i=0;i<NDIM*(NDIM+1)/2;i++) {
			T[i] = C1[i];
		}
		k=0;

//		dpptrf( &uplo, &n, T, &info );

		System.out.println("VSL_MATRIX_STORAGE_PACKED");
		System.out.println("-------------------------");
		System.out.println("");

		/*****************************************************
		 Stream initialization
		*****************************************************/
		errcode = VSL.vslNewStream(stream, BRNG, SEED);
		ErrCheck.CheckVslError( errcode );

		/*****************************************************
		 Generating random numbers
		 from multivariate normal distribution
		*****************************************************/
		{
		float[] r1d = new float[N*NDIM];
		errcode = VSL.vsRngGaussianMV( METHOD, stream, N, r1d, NDIM, STORAGE_PACK, a, T );
		ErrCheck.CheckVslError( errcode );
		for(i=0;i<N;i++) {
			for(j=0;j<NDIM;j++) {
				r[i][j] = r1d[i*NDIM+j];
			}
		}
		}

		/*****************************************************
		 Printing random numbers
		*****************************************************/
		System.out.println("Results (first " + NN + " of " + N + "):");
		System.out.println("---------------------------");
		for(i=0;i<NN;i++) {
			System.out.print("r[" + i + "]=(");
			for(j=0;j<NDIM;j++) {
				System.out.print("" + r[i][j] + " ");
			}
			System.out.println(")");
		}
		System.out.println("");

		/*****************************************************
		 Testing sample characteristics
		*****************************************************/
		dbSX=dbSY=dbSZ=dbS2X=dbS2Y=dbS2Z=dbSXY=dbSXZ=dbSYZ=0.0f;
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
		dbMeanX = dbSX/(float)N;
		dbMeanY = dbSY/(float)N;
		dbMeanZ = dbSZ/(float)N;
		dbVarX = dbS2X/(float)N-dbMeanX*dbMeanX;
		dbVarY = dbS2Y/(float)N-dbMeanY*dbMeanY;
		dbVarZ = dbS2Z/(float)N-dbMeanZ*dbMeanZ;
		dbCovXY = dbSXY/(float)N-dbMeanX*dbMeanY;
		dbCovXZ = dbSXZ/(float)N-dbMeanX*dbMeanZ;
		dbCovYZ = dbSYZ/(float)N-dbMeanY*dbMeanZ;

		/* Printing results */
		System.out.println("Sample characteristics:");
		System.out.println("-----------------------");
		System.out.println("       Sample           Theory");
		System.out.println("Mean :(" + dbMeanX + "," + dbMeanY + "," + dbMeanZ + ") (" + a[0] + "," + a[1] + "," + a[2] + ")");
		System.out.println("Var. :(" + dbVarX + "," + dbVarY + "," + dbVarZ + ") (" + CC[0][0] + "," + CC[1][1] + "," + CC[2][2] + ")");
		System.out.println("CovXY: " + dbCovXY + "               " + CC[0][1]);
		System.out.println("CovXZ: " + dbCovXZ + "               " + CC[0][2]);
		System.out.println("CovYZ: " + dbCovYZ + "               " + CC[1][2]);

		/* Checking results */
		DeltaMX = (a[0]-dbMeanX)/(float)Math.sqrt(CC[0][0]/dn);
		DeltaMY = (a[1]-dbMeanY)/(float)Math.sqrt(CC[1][1]/dn);
		DeltaMZ = (a[2]-dbMeanZ)/(float)Math.sqrt(CC[2][2]/dn);

		DeltaDX = (CC[0][0]-dbVarX)/(float)Math.sqrt(SX);
		DeltaDY = (CC[1][1]-dbVarY)/(float)Math.sqrt(SY);
		DeltaDZ = (CC[2][2]-dbVarZ)/(float)Math.sqrt(SZ);


		System.out.println("");
		/* All deltas must be less than 3.0 */
		if(DeltaMX>3.0f || DeltaDX>3.0f || DeltaMY>3.0f || DeltaDY>3.0f || DeltaMZ>3.0f || DeltaDZ>3.0f)
		{
			System.out.println ("Error: sample moments");
			System.out.println ("are disagreed with theory");
			System.out.println ("     DeltaM: " + DeltaMX + " " + DeltaMY + " " + DeltaMZ);
			System.out.println ("     DeltaD: " + DeltaDX + " " + DeltaDY + " " + DeltaDZ);
			System.out.println ("     ( at least one of the Deltas > 3.0)");
		} else {
			System.out.println ("Sample moments");
			System.out.println ("are agreed with theory");
			System.out.println ("     DeltaM: " + DeltaMX + " " + DeltaMY + " " + DeltaMZ);
			System.out.println ("     DeltaD: " + DeltaDX + " " + DeltaDY + " " + DeltaDZ);
			System.out.println ("     ( All Deltas < 3.0)");
		}

		/*****************************************************
		 Stream finalization
		*****************************************************/
		errcode = VSL.vslDeleteStream( stream );
		ErrCheck.CheckVslError( errcode );

	}
}
