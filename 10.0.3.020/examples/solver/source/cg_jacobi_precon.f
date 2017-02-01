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
C  Example program for solving symmetric positive definite system of equations.
C  Full case: full functionality of RCI (P)CG is used.
C---------------------------------------------------------------------------
      PROGRAM rci_pcg_f77_test_3
      IMPLICIT NONE
      INCLUDE 'mkl_solver.f77'
C---------------------------------------------------------------------------
C Define arrays for the upper triangle of the coefficient matrix and
C preconditioner as well as an array for rhs vector
C Compressed sparse row storage is used for sparse representation
C---------------------------------------------------------------------------
      INTEGER N, RCI_request, itercount, i
      PARAMETER (N=8)
      DOUBLE PRECISION  rhs(N)
      INTEGER IA(9)
      INTEGER JA(18)
      DOUBLE PRECISION A(18)
C Fill all arrays containing matrix data.
      DATA IA /1,5,8,10,12,15,17,18,19/
      DATA JA
     1 /1,  3,     6,7,
     2    2,3,   5,
     3      3,         8,
     4         4,    7,
     5           5,6,7,
     6             6,  8,
     7               7,
     8                 8/
      DATA A
     1 /7.D0,      1.D0,           2.D0, 7.D0,
     2       -4.D0, 8.D0,     2.D0,
     3             1.D0,                      5.D0,
     4                  7.D0,      9.D0,
     5                       5.D0, 1.D0, 5.D0,
     6                            -1.D0,      5.D0,
     7                                  11.D0,
     8                                        5.D0/

C---------------------------------------------------------------------------
C Allocate storage for the solver ?par and the initial solution vector
C---------------------------------------------------------------------------
      INTEGER length
      PARAMETER (length=128)
      INTEGER ipar(length)
      DOUBLE PRECISION dpar(length),TMP(N,4)
C---------------------------------------------------------------------------
C Some additional variables to use with the RCI (P)CG solver
C---------------------------------------------------------------------------
      CHARACTER MATDES(3)
      DOUBLE PRECISION  solution(N)
      DOUBLE PRECISION expected_sol(N)
      DATA expected_sol/1.D0, 0.D0, 1.D0, 0.D0, 1.D0, 0.D0, 1.D0, 0.D0/
      DOUBLE PRECISION  ONE
      DATA   ONE/1.D0/
      DOUBLE PRECISION DNRM2, Euclidean_norm, temp(N)
      EXTERNAL DNRM2
C---------------------------------------------------------------------------
C Initialize the right hand side through matrix-vector product
C---------------------------------------------------------------------------
       CALL MKL_DCSRSYMV('U', N, A, IA, JA, expected_sol, rhs)
C---------------------------------------------------------------------------
C Initialize the initial guess
C---------------------------------------------------------------------------
       DO I=1, N
         solution(I)=0.D0
       ENDDO
       MATDES(1)='D'
       MATDES(2)='L'
       MATDES(3)='N'
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
        CALL MKL_DCSRSYMV('U', N, A, IA, JA, TMP, TMP(1,2))
        GOTO 1
C---------------------------------------------------------------------------
C If RCI_request=2, then do the user-defined stopping test: compute the
C Euclidean norm of the actual residual using MKL routines and check if
C it is less than 1.D-8
C---------------------------------------------------------------------------
      ELSEIF (RCI_request .EQ. 2) THEN
        CALL MKL_DCSRSYMV('U', N, A, IA, JA, solution, temp)
        CALL DAXPY(N,-1.D0,rhs,1,temp,1)
        Euclidean_norm = DNRM2(N,temp,1)
        IF (Euclidean_norm .GT. 1.D-8) THEN
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
C If RCI_request=3, then compute apply the preconditioner matrix C_inverse
C on vector TMP(:,3) and put the result in vector TMP(:,4)
C---------------------------------------------------------------------------
      ELSEIF (RCI_request .EQ. 3) THEN
        CALL MKL_DCSRSV('N', N, ONE, MATDES,
     &             A, JA, IA, IA(2), TMP(1,3), TMP(1,4))
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
     &             itercount)
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
900   FORMAT(' Number of iterations: ',1(I2))
      GOTO 1000
999   WRITE(*,*) 'Solver returned error code ', RCI_request
      STOP
1000  CONTINUE
      STOP
      END
