!*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 2006-2008 Intel Corporation. All Rights Reserved.
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
C  Intel MKL example of RCI Flexible Generalized Minimal RESidual method with
C  ILU0 Preconditioner
C*******************************************************************************

C---------------------------------------------------------------------------
C  Example program for solving non-degenerate system of equations.
C  Full functionality of RCI FGMRES solver is exploited. Example shows how
C  ILU0 preconditioner accelerates the solver by reducing the number of
C  iterations.
C---------------------------------------------------------------------------

      PROGRAM FGMRES_FULL_FUNCT_F

      IMPLICIT NONE

      INCLUDE "mkl_rci.fi"

      INTEGER N
      PARAMETER(N=4)
      INTEGER SIZE
      PARAMETER (SIZE=128)
C---------------------------------------------------------------------------
C Define arrays for the upper triangle of the coefficient matrix
C Compressed sparse row storage is used for sparse representation
C---------------------------------------------------------------------------
      INTEGER IA(5)
      DATA IA /1,4,7,10,13/
      INTEGER JA(12)
      DATA JA /1,2,3,1,2,4,1,3,4,2,3,4/
      DOUBLE PRECISION A(12),BILU0(12),TRVEC(N)
      DATA A / 4.,-1.,-1.,-1.,4.,-1.,-1.,4.,-1.,-1.,-1.,4./
C---------------------------------------------------------------------------
C Allocate storage for the ?par parameters and the solution/rhs/residual vectors
C---------------------------------------------------------------------------
      INTEGER IPAR(SIZE),IERR
      DOUBLE PRECISION DPAR(SIZE), TMP(N*(2*N+1)+(N*(N+9))/2+1)
      DOUBLE PRECISION EXPECTED_SOLUTION(N)
      DATA EXPECTED_SOLUTION /1.0,1.0,1.0,1.0/
      DOUBLE PRECISION RHS(N), B(N)
      DOUBLE PRECISION COMPUTED_SOLUTION(N)
      DOUBLE PRECISION RESIDUAL(N)

      INTEGER MATSIZE, INCX, REF_NIT
      DOUBLE PRECISION REF_NORM2, NRM2
      PARAMETER (MATSIZE=12, INCX=1, REF_NIT=2, REF_NORM2=7.772387d0)
C---------------------------------------------------------------------------
C Some additional variables to use with the RCI (P)FGMRES solver
C---------------------------------------------------------------------------
      INTEGER ITERCOUNT
      INTEGER RCI_REQUEST, I
      DOUBLE PRECISION DVAR
C---------------------------------------------------------------------------
C An external BLAS function is taken from MKL BLAS to use
C with the RCI (P)FGMRES solver
C---------------------------------------------------------------------------
      DOUBLE PRECISION DNRM2
      EXTERNAL DNRM2

      WRITE( *,'(A,A)') '---------------------------------------------',
     1 '----------------------'
      WRITE(*,'(A,A)') 'The FULLY ADVANCED example RCI FGMRES with',
     1 ' ILU0 preconditioner'
      WRITE(*,'(A,A)') 'to solve the non-degenerate algebraic system',
     1 ' of linear equations'
      WRITE( *,'(A,A)') '---------------------------------------------',
     1 '----------------------'
C---------------------------------------------------------------------------
C Initialize variables and the right hand side through matrix-vector product
C---------------------------------------------------------------------------
      CALL MKL_DCSRGEMV('N', N, A, IA, JA, EXPECTED_SOLUTION, RHS)
C---------------------------------------------------------------------------
C Save the right-hand side in vector B for future use
C---------------------------------------------------------------------------
	CALL DCOPY(N, RHS, 1, B, 1)
C---------------------------------------------------------------------------
C Initialize the initial guess
C---------------------------------------------------------------------------
      DO I=1,N
         COMPUTED_SOLUTION(I)=0.d0
      ENDDO
         COMPUTED_SOLUTION(1)=100.d0

C---------------------------------------------------------------------------
C Initialize the solver
C---------------------------------------------------------------------------
      CALL DFGMRES_INIT(N, COMPUTED_SOLUTION, RHS, RCI_REQUEST, IPAR,
     1 DPAR, TMP)
      IF (RCI_REQUEST.NE.0) GOTO 999

C---------------------------------------------------------------------------
C Calculate ILU0 preconditioner.
C                      !ATTENTION!
C DCSRILU0 routine uses some IPAR, DPAR set by DFGMRES_INIT routine.
C Important for DCSRILU0 default entries set by DFGMRES_INIT are
C ipar(2) = 6 - output of error messages to the screen,
C ipar(6) = 1 - allow output of error messages,
C ipar(31)= 0 - abort DCSRILU0 calculations if routine meets zero diagonal element.
C
C If ILU0 is going to be used out of MKL FGMRES context, than the values
C of ipar(2), ipar(6), ipar(31), dpar(31), and dpar(32) should be user
C provided before the DCSRILU0 routine call.
C
C In this example, specific for DCSRILU0 entries are set in turn:
C ipar(31)= 1 - change small diagonal value to that given by dpar(32),
C dpar(31)= 1.D-20 instead of the default value set by DFGMRES_INIT.
C                  It is a small value to compare a diagonal entry with it.
C dpar(32)= 1.D-16 instead of the default value set by DFGMRES_INIT.
C                  It is the target value of the diagonal value if it is
C                  small as compared to dpar(31) and the routine should change
C                  it rather than abort DCSRILU0 calculations.
C---------------------------------------------------------------------------

	IPAR(31)=1
	DPAR(31)=1.D-20
	DPAR(32)=1.D-16
        CALL DCSRILU0(N, A, IA, JA, BILU0, IPAR, DPAR, IERR)
        NRM2=DNRM2(MATSIZE, BILU0, INCX)

	IF(IERR.ne.0) THEN
	  WRITE(*,'(A,A,I1)') ' Error after calculation of the',
     1	' preconditioner DCSRILU0',IERR
	  GOTO 998
	ENDIF

C---------------------------------------------------------------------------
C Set the desired parameters:
C do the restart after 2 iterations
C LOGICAL parameters:
C do not do the stopping test for the maximal number of iterations
C do the Preconditioned iterations of FGMRES method
C Set parameter IPAR(11) for preconditioner call. For this example,
C it reduces the number of iterations.
C DOUBLE PRECISION parameters
C set the relative tolerance to 1.0D-3 instead of default value 1.0D-6
C NOTE. Preconditioner may increase the number of iterations for an
C arbitrary case of the system and initial guess and even ruin the
C convergence. It is user's responsibility to use a suitable preconditioner
C and to apply it skillfully.
C---------------------------------------------------------------------------
      IPAR(15)=2
      IPAR(8)=0
      IPAR(11)=1
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
      WRITE( *,'(A)') ' '
      WRITE( *,'(A,A)') 'Some info about the current run of RCI FGMRES',
     1 ' method:'
      WRITE( *,'(A)') ' '
      IF (IPAR(8).NE.0) THEN
         WRITE(*,'(A,I1,A,A)') 'As IPAR(8)=',IPAR(8),', the automatic',
     1 ' test for the maximal number of iterations will be performed'
      ELSE
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(8)=',IPAR(8),', the automatic',
     1   ' test for the maximal number of iterations will be skipped'
      ENDIF
      WRITE( *,'(A)') '+++'
      IF (IPAR(9).NE.0) THEN
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(9)=',IPAR(9),', the automatic',
     1 ' residual test will be performed'
      ELSE
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(9)=',IPAR(9),', the automatic',
     1 ' residual test will be skipped'
      ENDIF
      WRITE( *,'(A)') '+++'
      IF (IPAR(10).NE.0) THEN
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(10)=',IPAR(10),', the',
     1 ' user-defined stopping test will be requested via RCI_REQUEST=2'
      ELSE
      	WRITE(*,'(A,I1,A,A,A)') 'As IPAR(10)=',IPAR(10),', the',
     1 ' user-defined stopping test will not be requested, thus,',
     1 ' RCI_REQUEST will not take the value 2'
      ENDIF
      WRITE( *,'(A)') '+++'
      IF (IPAR(11).NE.0) THEN
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(11)=',IPAR(11),', the',
     1 ' Preconditioned FGMRES iterations will be performed, thus,'
      	WRITE(*,'(A,A)') 'the preconditioner action will be requested',
     1 ' via RCI_REQUEST=3'
      ELSE
      	WRITE(*,'(A,I1,A,A)') 'As IPAR(11)=',IPAR(11),', the',
     1 ' Preconditioned FGMRES iterations will not be performed,'
      	WRITE( *,'(A)') 'thus, RCI_REQUEST will not take the value 3'
      ENDIF
      WRITE( *,'(A)') '+++'
      IF (IPAR(12).NE.0) THEN
      	WRITE(*,'(A,I1,A,A)')'As IPAR(12)=',IPAR(12),', the automatic',
     1 ' test for the norm of the next generated vector is not'
      	WRITE( *,'(A,A)') ' equal to zero up to rounding and',
     1 ' computational errors will be performed,'
      	WRITE( *,'(A)') 'thus, RCI_REQUEST will not take the value 4'
      ELSE
      	WRITE(*,'(A,I1,A,A)')'As IPAR(12)=',IPAR(12),', the automatic',
     1 ' test for the norm of the next generated vector is'
      	WRITE(*,'(A,A)') 'not equal to zero up to rounding and',
     1	' computational errors will be skipped,'
      	WRITE(*,'(A,A)') 'thus, the user-defined test will be requested',
     1 ' via RCI_REQUEST=4'
      ENDIF
      WRITE( *,'(A)') '+++'
C---------------------------------------------------------------------------
C Compute the solution by RCI (P)FGMRES solver with preconditioning
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
      ENDIF
C---------------------------------------------------------------------------
C If RCI_request=2, then do the user-defined stopping test
C The residual stopping test for the computed solution is performed here
C---------------------------------------------------------------------------
C NOTE: from this point vector B(N) is no longer containing the right-hand
C side of the problem! It contains the current FGMRES approximation to the
C solution. If you need to keep the right-hand side, save it in some other
C vector before the call to DFGMRES routine. Here we saved it in vector
C RHS(N). The vector B is used instead of RHS to preserve the original
C right-hand side of the problem and guarantee the proper restart of FGMRES
C method. Vector B will be altered when computing the residual stopping
C criterion!
C---------------------------------------------------------------------------
      IF (RCI_REQUEST.EQ.2) THEN
C Request to the DFGMRES_GET routine to put the solution into B(N) via IPAR(13)
      	IPAR(13)=1
C Get the current FGMRES solution in the vector B(N)
      	CALL DFGMRES_GET(N, COMPUTED_SOLUTION, B, RCI_REQUEST, IPAR,
     1 DPAR, TMP, ITERCOUNT)
C Compute the current true residual via MKL (Sparse) BLAS routines
      	CALL MKL_DCSRGEMV('N', N, A, IA, JA, B, RESIDUAL)
      	CALL DAXPY(N, -1.0D0, RHS, 1, RESIDUAL, 1)
      	DVAR=DNRM2(N, RESIDUAL, 1)
      	IF (DVAR.LT.1.0E-3) THEN
      	   GOTO 3
      	ELSE
      	   GOTO 1
      	ENDIF
      ENDIF
C---------------------------------------------------------------------------
C If RCI_REQUEST=3, then apply the preconditioner on the vector
C TMP(IPAR(22)) and put the result in vector TMP(IPAR(23))
C Here is the recommended usage of the result produced by ILU0 routine
C via standard MKL Sparse Blas solver routine mkl_dcsrtrsv.
C---------------------------------------------------------------------------
      IF (RCI_REQUEST.EQ.3) THEN
       CALL MKL_DCSRTRSV('L','N','U',N,BILU0,IA,JA,TMP(IPAR(22)),TRVEC)
       CALL MKL_DCSRTRSV('U','N','N',N,BILU0,IA,JA,TRVEC,TMP(IPAR(23)))
       GOTO 1
      ENDIF
C---------------------------------------------------------------------------
C If RCI_REQUEST=4, then check if the norm of the next generated vector is
C not zero up to rounding and computational errors. The norm is contained
C in DPAR(7) parameter
C---------------------------------------------------------------------------
      IF (RCI_REQUEST.EQ.4) THEN
      	IF (DPAR(7).LT.1.0D-12) THEN
      	   GOTO 3
      	ELSE
      	   GOTO 1
      	ENDIF
C---------------------------------------------------------------------------
C If RCI_REQUEST=anything else, then DFGMRES subroutine failed
C to compute the solution vector: COMPUTED_SOLUTION(N)
C---------------------------------------------------------------------------
      ELSE
      	GOTO 999
      ENDIF
C---------------------------------------------------------------------------
C Reverse Communication ends here
C Get the current iteration number and the FGMRES solution. (DO NOT FORGET to
C call DFGMRES_GET routine as computed_solution is still containing
C the initial guess!). Request to DFGMRES_GET to put the solution into
C vector COMPUTED_SOLUTION(N) via IPAR(13)
C---------------------------------------------------------------------------
3     IPAR(13)=0
      CALL DFGMRES_GET(N, COMPUTED_SOLUTION, RHS, RCI_REQUEST, IPAR,
     1 DPAR, TMP, ITERCOUNT)
C---------------------------------------------------------------------------
C Print solution vector: COMPUTED_SOLUTION(N) and
C the number of iterations: ITERCOUNT
C---------------------------------------------------------------------------
      WRITE( *,'(A)') ' '
      WRITE( *,'(A)') 'The system has been solved'
      WRITE( *,'(A)') ' '
      WRITE( *,'(A)') 'The following solution has been obtained:'
      DO I=1,N
         WRITE(*,'(A18,I1,A2,E10.3)') 'COMPUTED_SOLUTION(',I,')=',
     1 COMPUTED_SOLUTION(I)
      ENDDO
      WRITE( *,'(A)') ' '
      WRITE( *,'(A)') 'The expected solution is:'
      DO I=1,N
         WRITE(*,'(A18,I1,A2,E10.3)') 'EXPECTED_SOLUTION(',I,')=',
     1 EXPECTED_SOLUTION(I)
      ENDDO
      WRITE( *,'(A)') ' '
      WRITE( *,'(A,I2)') 'Number of iterations: ',ITERCOUNT
      WRITE( *,'(A)') ' '

      IF(ITERCOUNT.EQ.REF_NIT.AND.DABS(REF_NORM2-NRM2).LT.1.D-6) THEN
      WRITE( *,'(A)') ' '
      WRITE( *,'(A)') '------------------------------------------------'
      WRITE( *,'(A,A)') 'Fortran example of FGMRES with ILU0',
     1 ' preconditioner '
      WRITE( *,'(A,A)') 'has successfully PASSED all stages of',
     1 ' computations'
      WRITE( *,'(A)') '------------------------------------------------'
      WRITE( *,'(A)') ' '
      GOTO 1000
	ELSE
      WRITE( *,'(A,A)') 'Probably, the preconditioner was computed',
     1 ' incorrectly:'
      WRITE( *,'(A,F9.6,A,F9.6)')
     1 'Either the preconditioner norm',NRM2,
     2 ' differs from the expected norm',REF_NORM2
      WRITE( *,'(A,I2,A,I2)'),
     1 'and/or the number of iterations ', ITERCOUNT,
     2 ' differs from the expected number ', REF_NIT
      WRITE( *,'(A)') ' '
      WRITE( *,'(A,A)') '---------------------------------------------',
     1 '----------------------'
      WRITE( *,'(A,A)') 'Unfortunately, FGMRES+ILU0 Fortran example',
     1 ' has FAILED'
      WRITE( *,'(A,A)') '---------------------------------------------',
     1 '----------------------'
      WRITE( *,'(A)') ' '
      GOTO 1000
      END IF
999   WRITE( *,'(A,I2)') 'The solver has returned the ERROR code ',
     1 RCI_REQUEST
998   WRITE( *,'(A)') ' '
      WRITE( *,'(A,A)') '---------------------------------------------',
     1 '----------------------'
      WRITE( *,'(A,A)') 'Unfortunately, FGMRES+ILU0 Fortran example',
     1 ' has FAILED'
      WRITE( *,'(A,A)') '---------------------------------------------',
     1 '----------------------'
      WRITE( *,'(A)') ' '

1000  CONTINUE
      END
