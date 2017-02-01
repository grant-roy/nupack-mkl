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
*   Content : TR Solver Fortran-77 example
*
********************************************************************************

C** NONLINEAR LEAST SQUARE PROBLEM WITHOUT BOUNDARY CONSTRAINTS
      PROGRAM EXAMPLE_DTRNLSP_POWELL
        IMPLICIT NONE
C** HEADER-FILE WITH DEFINITIONS (CONSTANTS, EXTERNALS)
        INCLUDE "mkl_rci.fi"
C** USER’S OBJECTIVE FUNCTION
        EXTERNAL            EXTENDET_POWELL
C** N - NUMBER OF FUNCTION VARIABLES
        INTEGER             N
        PARAMETER           (N = 40)
C** M - DIMENSION OF FUNCTION VALUE
        INTEGER             M
        PARAMETER           (M = 40)
C** SOLUTION VECTOR. CONTAINS VALUES X FOR F(X)
        DOUBLE PRECISION    X (N)
C** PRECISIONS FOR STOP-CRITERIA (SEE MANUAL FOR MORE DETAILES)
        DOUBLE PRECISION    EPS (6)
C** JACOBI CALCULATION PRECISION
        DOUBLE PRECISION    JAC_EPS
C** REVERSE COMMUNICATION INTERFACE PARAMETER
        INTEGER             RCI_REQUEST
C** FUNCTION (F(X)) VALUE VECTOR
        DOUBLE PRECISION    FVEC (M)
C** JACOBI MATRIX
        DOUBLE PRECISION    FJAC (M, N)
C** NUMBER OF ITERATIONS
        INTEGER             ITER
C** NUMBER OF STOP-CRITERION
        INTEGER             ST_CR
C** CONTROLS OF RCI CYCLE
        INTEGER             SUCCESSFUL
C** MAXIMUM NUMBER OF ITERATIONS
        INTEGER             ITER1
C** MAXIMUM NUMBER OF ITERATIONS OF CALCULATION OF TRIAL-STEP
        INTEGER             ITER2
C** INITIAL STEP BOUND
        DOUBLE PRECISION    RS
C** INITIAL AND FINAL RESIDAULS
        DOUBLE PRECISION    R1, R2
C** TR SOLVER HANDLE
        INTEGER*8            HANDLE
C** CYCLE’S COUNTERS
        INTEGER             I, J

C** SET PRECISIONS FOR STOP-CRITERIA
	DO I = 1, 6
        EPS (I) = 1.D-5
	ENDDO
C** SET MAXIMUM NUMBER OF ITERATIONS
        ITER1 = 1000
C** SET MAXIMUM NUMBER OF ITERATIONS OF CALCULATION OF TRIAL-STEP
        ITER2 = 100
C** SET INITIAL STEP BOUND
        RS = 100.D0
C** PRECISIONS FOR JACOBI CALCULATION
        JAC_EPS = 1.D-8
C** SET THE INITIAL GUESS
        DO I = 1, N/4
            X (4*I - 3) =  3.D0
            X (4*I - 2) = -1.D0
            X (4*I - 1) =  0.D0
            X (4*I)     =  1.D0
        ENDDO
C** SET INITIAL VALUES
        DO I = 1, M
            FVEC (I) = 0.D0
            DO J = 1, N
                FJAC (I, J) = 0.D0
            ENDDO
        ENDDO
C** INITIALIZE SOLVER (ALLOCATE MAMORY, SET INITIAL VALUES)
C**   HANDLE    IN/OUT: TR SOLVER HANDLE
C**   N         IN:     NUMBER OF FUNCTION VARIABLES
C**   M         IN:     DIMENSION OF FUNCTION VALUE
C**   X         IN:     SOLUTION VECTOR. CONTAINS VALUES X FOR F(X)
C**   EPS       IN:     PRECISIONS FOR STOP-CRITERIA
C**   ITER1     IN:     MAXIMUM NUMBER OF ITERATIONS
C**   ITER2     IN:     MAXIMUM NUMBER OF ITERATIONS OF CALCULATION OF TRIAL-STEP
C**   RS        IN:     INITIAL STEP BOUND
        IF (DTRNLSP_INIT (HANDLE, N, M, X, EPS, ITER1, ITER2, RS)
     +  /= TR_SUCCESS) THEN
C** IF FUNCTION DOES NOT COMPLETE SUCCESSFUL THEN PRINT ERROR MESSAGE
            PRINT *, '| ERROR IN DTRNLSP_INIT'
C** AND STOP
            STOP
        ENDIF
C** SET INITIAL RCI CYCLE VARIABLES
        RCI_REQUEST = 0
        SUCCESSFUL = 0
C** RCI CYCLE
        DO WHILE (SUCCESSFUL == 0)
C** CALL TR SOLVER
C**   HANDLE        IN/OUT: TR SOLVER HANDLE
C**   FVEC          IN:     VECTOR
C**   FJAC          IN:     JACOBI MATRIX
C**   RCI_REQUEST   IN/OUT: RETURN NUMBER WHICH DENOTE NEXT STEP FOR PERFORMING
	      IF (DTRNLSP_SOLVE (HANDLE, FVEC, FJAC, RCI_REQUEST)
     +      /= TR_SUCCESS) THEN
C** IF FUNCTION DOES NOT COMPLETE SUCCESSFUL THEN PRINT ERROR MESSAGE
	          PRINT *, '| ERROR IN DTRNLSP_SOLVE'
C** AND STOP
	          STOP
	      ENDIF
C** ACCORDING WITH RCI_REQUEST VALUE WE DO NEXT STEP
            SELECT CASE (RCI_REQUEST)
            CASE (-1, -2, -3, -4, -5, -6)
C**   STOP RCI CYCLE
	          SUCCESSFUL = 1
            CASE (1)
C**   RECALCULATE FUNCTION VALUE
C**     M               IN:     DIMENSION OF FUNCTION VALUE
C**     N               IN:     NUMBER OF FUNCTION VARIABLES
C**     X               IN:     SOLUTION VECTOR
C**     FVEC            OUT:    FUNCTION VALUE F(X)
	          CALL EXTENDET_POWELL (M, N, X, FVEC)
            CASE (2)
C**   COMPUTE JACOBI MATRIX
C**     EXTENDET_POWELL IN:     EXTERNAL OBJECTIVE FUNCTION
C**     N               IN:     NUMBER OF FUNCTION VARIABLES
C**     M               IN:     DIMENSION OF FUNCTION VALUE
C**     FJAC            OUT:    JACOBI MATRIX
C**     X               IN:     SOLUTION VECTOR
C**     JAC_EPS         IN:     JACOBI CALCULATION PRECISION
                IF (DJACOBI (EXTENDET_POWELL, N, M, FJAC, X, JAC_EPS)
     +          /= TR_SUCCESS) THEN
C** IF FUNCTION DOES NOT COMPLETE SUCCESSFUL THEN PRINT ERROR MESSAGE
	              PRINT *, '| ERROR IN DJACOBI'
C** AND STOP
	              STOP
	          ENDIF
	      ENDSELECT
        ENDDO
C** GET SOLUTION STATUSES
C**   HANDLE            IN: TR SOLVER HANDLE
C**   ITER              OUT: NUMBER OF ITERATIONS
C**   ST_CR             OUT: NUMBER OF STOP CRITERION
C**   R1                OUT: INITIAL RESIDUALS
C**   R2                OUT: FINAL RESIDUALS
        IF (DTRNLSP_GET (HANDLE, ITER, ST_CR, R1, R2)
     +  /= TR_SUCCESS) THEN
C** IF FUNCTION DOES NOT COMPLETE SUCCESSFUL THEN PRINT ERROR MESSAGE
            PRINT *, '| ERROR IN DTRNLSP_GET'
C** AND STOP
            STOP
        ENDIF
C** FREE HANDLE MEMORY
        IF (DTRNLSP_DELETE (HANDLE) /= TR_SUCCESS) THEN
C** IF FUNCTION DOES NOT COMPLETE SUCCESSFUL THEN PRINT ERROR MESSAGE
            PRINT *, '| ERROR IN DTRNLSP_DELETE'
C** AND STOP
            STOP
        ENDIF

C** IF FINAL RESIDUAL LESS THEN REQUIRED PRECISION THEN PRINT PASS
        IF (R2 < 1.D-5) THEN
            PRINT *, '|         DTRNLSP POWELL............PASS'!, R1, R2
C** ELSE PRINT FAILED
        ELSE
            PRINT *, '|         DTRNLSP POWELL............FAILED'!, R1, R2
        ENDIF

      END PROGRAM EXAMPLE_DTRNLSP_POWELL

C** ROUTINE FOR EXTENDET POWELL FUNCTION CALCULATION
C**   M     IN:     DIMENSION OF FUNCTION VALUE
C**   N     IN:     NUMBER OF FUNCTION VARIABLES
C**   X     IN:     VECTOR FOR FUNCTION CALCULATING
C**   F     OUT:    FUNCTION VALUE F(X)
      SUBROUTINE EXTENDET_POWELL (M, N, X, F)
        IMPLICIT NONE
        INTEGER M, N
        DOUBLE PRECISION X (*), F (*)
        INTEGER I

        DO I = 1, N/4
            F (4*I-3) = X(4*I - 3) + 10.D0 * X(4*I - 2)
            F (4*I-2) = 2.2360679774D0*(X(4*I-1) - X(4*I))
            F (4*I-1) = (X(4*I-2) - 2.D0*X(4*I-1))**2
            F (4*I)   = 3.1622776601D0*(X(4*I-3) - X(4*I))**2
        ENDDO

      ENDSUBROUTINE EXTENDET_POWELL