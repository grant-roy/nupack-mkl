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
C*******************************************************************************
C  Content:
C  Intel MKL RCI (P)FGMRES ((Preconditioned) Flexible Generalized Minimal
C                                                       RESidual method) example
C*******************************************************************************

C---------------------------------------------------------------------------
C  Example program for solving non-symmetric indefinite system of equations
C  Simplest case: no preconditioning and no user-defined stopping tests
C---------------------------------------------------------------------------

      PROGRAM FGMRES_NO_PRECON_F

      INCLUDE "mkl_rci.fi"

      INTEGER N
      PARAMETER(N=5)
      INTEGER SIZE
      PARAMETER (SIZE=128)
C---------------------------------------------------------------------------
C Define arrays for the upper triangle of the coefficient matrix
C Compressed sparse row storage is used for sparse representation
C---------------------------------------------------------------------------
      INTEGER IA(6)
      DATA IA /1,3,6,9,12,14/
      INTEGER JA(13)
      DATA JA    /  1,        3,
     1              1,   2,        4,
     2						 2,   3,        5,
     3 								3,   4,   5,
     4 									  4,   5  /
      DOUBLE PRECISION A(13)
      DATA A     / 1.0,     -1.0,
     1 		  	   -1.0, 1.0,     -1.0,
     2 				      1.0,-2.0,      1.0,
     3 				          -1.0, 2.0,-1.0,
     4 				               -1.0,-3.0 /
C---------------------------------------------------------------------------
C Allocate storage for the ?par parameters and the solution/rhs vectors
C---------------------------------------------------------------------------
      INTEGER IPAR(SIZE)
      DOUBLE PRECISION DPAR(SIZE), TMP(N*(2*N+1)+(N*(N+9))/2+1)
      DOUBLE PRECISION EXPECTED_SOLUTION(N)
      DATA EXPECTED_SOLUTION /-1.0,1.0,0.0,1.0,-1.0/
      DOUBLE PRECISION RHS(N)
      DOUBLE PRECISION COMPUTED_SOLUTION(N)
C---------------------------------------------------------------------------
C Some additional variables to use with the RCI (P)FGMRES solver
C---------------------------------------------------------------------------
      INTEGER ITERCOUNT
      INTEGER RCI_REQUEST, I

      PRINT *,'--------------------------------------------------'
      PRINT *,'The SIMPLEST example of usage of RCI FGMRES solver'
      PRINT *,'to solve a non-symmetric indefinite non-degenerate'
      PRINT *,'       algebraic system of linear equations'
      PRINT *,'--------------------------------------------------'
C---------------------------------------------------------------------------
C Initialize variables and the right hand side through matrix-vector product
C---------------------------------------------------------------------------
      CALL MKL_DCSRGEMV('N', N, A, IA, JA, EXPECTED_SOLUTION, RHS)
C---------------------------------------------------------------------------
C Initialize the initial guess
C---------------------------------------------------------------------------
      DO I=1,N
         COMPUTED_SOLUTION(I)=1.0
      ENDDO
C---------------------------------------------------------------------------
C Initialize the solver
C---------------------------------------------------------------------------
      CALL DFGMRES_INIT(N, COMPUTED_SOLUTION, RHS, RCI_REQUEST, IPAR,
     1 DPAR, TMP)
      IF (RCI_REQUEST.NE.0) GOTO 999
C---------------------------------------------------------------------------
C Set the desired parameters:
C LOGICAL parameters:
C do residual stopping test
C do not request for the user defined stopping test
C do the check of the norm of the next generated vector automatically
C DOUBLE PRECISION parameters
C set the relative tolerance to 1.0D-3 instead of default value 1.0D-6
C---------------------------------------------------------------------------
      IPAR(9)=1
      IPAR(10)=0
      IPAR(12)=1
      DPAR(1)=1.0D-3
C---------------------------------------------------------------------------
C Check the correctness and consistency of the newly set parameters
C---------------------------------------------------------------------------
      CALL DFGMRES_CHECK(N, COMPUTED_SOLUTION, RHS, RCI_REQUEST,
     1 IPAR, DPAR, TMP)
      IF (RCI_REQUEST.NE.0) GOTO 999
C---------------------------------------------------------------------------
C Print the info about the RCI FGMRES method
C---------------------------------------------------------------------------
      PRINT *, ''
      PRINT *,'Some info about the current run of RCI FGMRES method:'
      PRINT *, ''
      IF (IPAR(8).NE.0) THEN
         WRITE(*,'(A,I1,A,A)') 'As IPAR(8)=',IPAR(8),', the automatic',
     1 ' test for the maximal number of iterations will be'
         PRINT *,'performed'
      ELSE
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(8)=',IPAR(8),', the automatic',
     1 ' test for the maximal number of iterations will be'
      	PRINT *,'skipped'
      ENDIF
      PRINT *,'+++'
      IF (IPAR(9).NE.0) THEN
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(9)=',IPAR(9),', the automatic',
     1 ' residual test will be performed'
      ELSE
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(9)=',IPAR(9),', the automatic',
     1 ' residual test will be skipped'
      ENDIF
      PRINT *,'+++'
      IF (IPAR(10).NE.0) THEN
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(10)=',IPAR(10),', the',
     1 ' user-defined stopping test will be requested via'
      	PRINT *,'RCI_REQUEST=2'
      ELSE
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(10)=',IPAR(10),', the',
     1 ' user-defined stopping test will not be requested, thus,'
      	PRINT *,'RCI_REQUEST will not take the value 2'
      ENDIF
      PRINT *,'+++'
      IF (IPAR(11).NE.0) THEN
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(11)=',IPAR(11),', the',
     1 ' Preconditioned FGMRES iterations will be performed, thus,'
      	WRITE(*,'(A,A)') 'the preconditioner action will be requested',
     1 ' via RCI_REQUEST=3'
      ELSE
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(11)=',IPAR(11),', the',
     1 ' Preconditioned FGMRES iterations will not be performed,'
      	WRITE(*,'(A)') 'thus, RCI_REQUEST will not take the value 3'
      ENDIF
      PRINT *,'+++'
      IF (IPAR(12).NE.0) THEN
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(12)=',IPAR(12),', the automatic',
     1 ' test for the norm of the next generated vector is'
      	WRITE(*,'(A,A)') 'not equal to zero up to rounding and',
     1 ' computational errors will be performed,'
      	PRINT *,'thus, RCI_REQUEST will not take the value 4'
      ELSE
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(12)=',IPAR(12),', the automatic',
     1 ' test for the norm of the next generated vector is'
      	WRITE(*,'(A,A)') 'not equal to zero up to rounding and',
     1 ' computational errors will be skipped,'
      	WRITE(*,'(A,A)') 'thus, the user-defined test will be requested',
     1 ' via RCI_REQUEST=4'
      ENDIF
      PRINT *,'+++'
C---------------------------------------------------------------------------
C Compute the solution by RCI (P)FGMRES solver without preconditioning
C Reverse Communication starts here
C---------------------------------------------------------------------------
1     CALL DFGMRES(N, COMPUTED_SOLUTION, RHS, RCI_REQUEST, IPAR,
     1 DPAR, TMP)
C---------------------------------------------------------------------------
C If RCI_REQUEST=0, then the solution was found with the required precision
C---------------------------------------------------------------------------
      IF (RCI_REQUEST.EQ.0) GOTO 3
C---------------------------------------------------------------------------
C If RCI_REQUEST=1, then compute the vector A*TMP(IPAR(22))
C and put the result in vector TMP(IPAR(23))
C---------------------------------------------------------------------------
      IF (RCI_REQUEST.EQ.1) THEN
      	CALL MKL_DCSRGEMV('N',N, A, IA, JA, TMP(IPAR(22)), TMP(IPAR(23)))
      	GOTO 1
C---------------------------------------------------------------------------
C If RCI_REQUEST=anything else, then DFGMRES subroutine failed
C to compute the solution vector: COMPUTED_SOLUTION(N)
C---------------------------------------------------------------------------
      ELSE
      	GOTO 999
      ENDIF
C---------------------------------------------------------------------------
C Reverse Communication ends here
C Get the current iteration number and the FGMRES solution (DO NOT FORGET to
C call DFGMRES_GET routine as COMPUTED_SOLUTION is still containing
C the initial guess!)
C---------------------------------------------------------------------------
3     CALL DFGMRES_GET(N, COMPUTED_SOLUTION, RHS, RCI_REQUEST, IPAR,
     1 DPAR, TMP, ITERCOUNT)
C---------------------------------------------------------------------------
C Print solution vector: COMPUTED_SOLUTION(N) and
C the number of iterations: ITERCOUNT
C---------------------------------------------------------------------------
      PRINT *, ''
      PRINT *,' The system has been SUCCESSFULLY solved'
      PRINT *, ''
      PRINT *,' The following solution has been obtained:'
      DO I=1,N
         WRITE(*,'(A18,I1,A2,E10.3)') 'COMPUTED_SOLUTION(',I,')=',
     1 COMPUTED_SOLUTION(I)
      ENDDO
      PRINT *, ''
      PRINT *,' The expected solution is:'
      DO I=1,N
         WRITE(*,'(A18,I1,A2,E10.3)') 'EXPECTED_SOLUTION(',I,')=',
     1 EXPECTED_SOLUTION(I)
      ENDDO
      PRINT *, ''
      PRINT *,' Number of iterations: ',ITERCOUNT
      GOTO 1000

999   PRINT *,'The solver has returned the ERROR code ', RCI_REQUEST

1000  CONTINUE
      END
