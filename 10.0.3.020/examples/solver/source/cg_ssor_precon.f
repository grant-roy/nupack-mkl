!*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 2005-2008 Intel Corporation. All Rights Reserved.
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
!
!*******************************************************************************
!  Content: Intel MKL RCI (P)CG Fortran-77 example
!
!*******************************************************************************

C---------------------------------------------------------------------------
C Example program for solving symmetric positive definite system of
C equations A*x =f where A is a symmetric tridiagonal matrix of order N whose
C nonzero elements are defined as follows
C
C         a(i, i) = 1,     i=1, N;
C         a(i, i+1)= 1/2,    i=1, N-1;
C         a(i+1, i) = 1/2,   i=1, N-1;
C
C The system is solved with the help of the conjugate gradient method where
C the Symmetric Successive Over Relaxation (SSOR) stationary iterative solver
C with the given number of iterations is used as the preconditioner. The relaxation
C parameter is set to 1/2, the number of iterations for the SSOR preconditioner
C is equal to 20. For simplicity, we don't check the convergence while solving
C equations with the help of SSOR. Let us recall that the scheme of SSOR is
C the following:
C
C         (D+w*U^{T}) x_{k+1} = (-w*U+(1-w)*D)*x_{k}+w*f
C
C where w is the relaxation parameter, f is the right hand side, D is the diagonal
C of A, U its strict upper part so that
C
C                  A   =  U^{T} + D + U
C
C The compressed sparse row format is used for storing nonzeros of A and
C since D is the identity matrix, we don't need to store diagonal elements. So
C the code given below only uses the sparse representation of U. The Intel
C MKL Sparse BLAS is designed so that it allows the user to perform large
C variety of operations with one sparse representation setting appropriate
C values of the descriptor array (see MKL User Guide for the further details).
C
C Full case: full functionality of RCI (P)CG is used.
C---------------------------------------------------------------------------
      PROGRAM rci_pcg_f77_test_ssor
      IMPLICIT NONE
      INCLUDE 'mkl_solver.f77'
C---------------------------------------------------------------------------
C Define arrays for the upper triangle of the coefficient matrix and
C preconditioner as well as an array for rhs vector
C Compressed sparse row storage is used for sparse representation
C---------------------------------------------------------------------------
      INTEGER N, RCI_request, itercount, i
      PARAMETER (N=100)
      DOUBLE PRECISION  rhs(N)
      INTEGER IA(N+1)
      INTEGER JA(N-1)
      DOUBLE PRECISION A(N-1), A1(N-1)
C---------------------------------------------------------------------------
C Allocate storage for the solver ?par and the initial solution vector
C---------------------------------------------------------------------------
      INTEGER length
      PARAMETER (length=128)
      INTEGER ipar(length)
      DOUBLE PRECISION dpar(length),TMP(N,4)
C---------------------------------------------------------------------------
C Some additional variables to use with the RCI (P)CG solver
C OMEGA is the relaxation parameter, NITER_SSOR is the maximum number of
C iterations for the SSOR preconditioner
C---------------------------------------------------------------------------
      DOUBLE PRECISION  solution(N)
      DOUBLE PRECISION expected_sol(N)
      DOUBLE PRECISION OMEGA, ONE, ZERO
      DATA  OMEGA/0.5D0/, ONE/1.D0/, ZERO/0.D0/
      DOUBLE PRECISION DNRM2, Euclidean_norm, temp(N)
      INTEGER   NITER_SSOR
      DATA      NITER_SSOR/20/
      CHARACTER MATDES(3)

      EXTERNAL DNRM2
C---------------------------------------------------------------------------
C Initialize the coeffient matrix and expected solution
C---------------------------------------------------------------------------
      DO I=1, N
         expected_sol(I)=1.D0
      ENDDO
      DO I=1, N-1
         JA(I)=I+1
         IA(I)=I
         A(I)=0.5D0
         A1(I)= OMEGA*A(I)
      ENDDO
      IA(N)=N
      IA(N+1)=IA(N)
      MATDES(1)='S'
      MATDES(2)='U'
      MATDES(3)='U'
C---------------------------------------------------------------------------
C Initialize the right hand side through matrix-vector product
C---------------------------------------------------------------------------

         CALL MKL_DCSRMV('N', N, N, ONE, MATDES,
     &            A, JA, IA, IA(2), expected_sol, ZERO, rhs)

C---------------------------------------------------------------------------
C Initialize the initial guess
C---------------------------------------------------------------------------
         DO I=1, N
           solution(I)=ZERO
         ENDDO
C---------------------------------------------------------------------------
C Initialize the solver
C---------------------------------------------------------------------------
      CALL dcg_init(N, solution,rhs, RCI_request,ipar,dpar,TMP)
      IF (RCI_request .NE. 0 ) GOTO 999
C---------------------------------------------------------------------------
C Set the desired parameters:
C INTEGER parameters:
C set the maximal number of iterations to 100
C LOGICAL parameters:
C run the Preconditioned version of RCI (P)CG with preconditioner C_inverse
C DOUBLE PRECISION parameters
C -
C---------------------------------------------------------------------------
      ipar(5)=100
      ipar(11)=1
C---------------------------------------------------------------------------
C Check the correctness and consistency of the newly set parameters
C---------------------------------------------------------------------------
      CALL dcg_check(N,solution,rhs,RCI_request,ipar,dpar,TMP)
      IF (RCI_request .NE. 0 ) GOTO 999
C---------------------------------------------------------------------------
C Compute the solution by RCI (P)CG solver
C Reverse Communications starts here
C---------------------------------------------------------------------------
1     CALL dcg(N,solution,rhs,RCI_request,ipar,dpar,TMP)
C---------------------------------------------------------------------------
C If RCI_request=0, then the solution was found according to the requested
C stopping tests. In this case, this means that it was found after 100
C iterations.
C---------------------------------------------------------------------------
      IF (RCI_request .EQ. 0) THEN
          GOTO 700
C---------------------------------------------------------------------------
C If RCI_request=1, then compute the vector A*TMP(:,1)
C and put the result in vector TMP(:,2)
C---------------------------------------------------------------------------
      ELSEIF (RCI_request .EQ. 1) THEN
          MATDES(1)='S'
          CALL MKL_DCSRMV('N', N, N, ONE, MATDES,
     &            A, JA, IA, IA(2), TMP, ZERO, TMP(1,2))
        GOTO 1
C---------------------------------------------------------------------------
C If RCI_request=2, then do the user-defined stopping test: compute the
C Euclidean norm of the actual residual using MKL routines and check if
C it is less than 1.D-8
C---------------------------------------------------------------------------
      ELSEIF (RCI_request .EQ. 2) THEN
          MATDES(1)='S'
          CALL MKL_DCSRMV('N', N, N, ONE, MATDES,
     &            A, JA, IA, IA(2), SOLUTION, ZERO, TEMP)
         CALL DAXPY(N,-1.D0,rhs,1,temp,1)
         Euclidean_norm = DNRM2(N,temp,1)
         IF (Euclidean_norm .GT. 1.D-6) THEN
C---------------------------------------------------------------------------
C The solution has not been found yet according to the user-defined stopping
C test. Continue RCI (P)CG iterations.
C---------------------------------------------------------------------------
          GOTO 1
        ELSE
C---------------------------------------------------------------------------
C The solution has been found according to the user-defined stopping test
C---------------------------------------------------------------------------
          GOTO 700
        END IF
C---------------------------------------------------------------------------
C If RCI_request=3, then  apply the simplest SSOR preconditioning
C on vector TMP(:,3) and put the result in vector TMP(:,4)
C---------------------------------------------------------------------------
      ELSEIF (RCI_request .EQ. 3) THEN
         CALL DCOPY(N, TMP(1,3), 1, TMP(1, 4), 1)
         MATDES(1)='T'
         DO I=1, NITER_SSOR
            CALL DCOPY(N, TMP(1, 3), 1, TEMP, 1)
            MATDES(3)='N'
            CALL MKL_DCSRMV('N', N, N, -ONE, MATDES,
     &             A1, JA, IA, IA(2), TMP(1, 4), OMEGA, TEMP)
            CALL DAXPY(N, ONE-OMEGA, TMP(1,4), 1, TEMP, 1)
            MATDES(3)='U'
            CALL MKL_DCSRSV('T', N, ONE, MATDES,
     &             A1, JA, IA, IA(2), TEMP, TMP(1,4))
         ENDDO
        GOTO 1
      ELSE
C---------------------------------------------------------------------------
C If RCI_request=anything else, then dcg subroutine failed
C to compute the solution vector: solution(N)
C---------------------------------------------------------------------------
        GOTO 999
      ENDIF
C---------------------------------------------------------------------------
C Reverse Communication ends here
C Get the current iteration number
C---------------------------------------------------------------------------
700   CALL dcg_get(N,solution,rhs,RCI_request,ipar,dpar,TMP,
     &                    itercount)
C---------------------------------------------------------------------------
C Print solution vector: solution(N) and number of iterations: itercount
C---------------------------------------------------------------------------
      WRITE(*, *) ' The system is successfully solved '
      WRITE(*, *) ' The following solution obtained '
      WRITE(*,800) (solution(i),i =1,N)
      WRITE(*, *) ' expected_sol solution '
      WRITE(*,800)(expected_sol(i),i =1,N)
800   FORMAT(4(F10.3))
      WRITE(*,900)(itercount)
900   FORMAT(' Number of iterations: ', I5)
      GOTO 1000
999   WRITE(*,*) 'Solver returned error code ', RCI_request
      STOP
1000  CONTINUE
      STOP
      END
