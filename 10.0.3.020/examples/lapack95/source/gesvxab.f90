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
!  Content:
!     S G E S V X  Example Program Text
!*******************************************************************************

      PROGRAM SGESVX_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: GESVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, NRHS, INFO
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      REAL(WP) :: RCOND, RPVGRW
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), X(:,:),BB(:,:), FERR(:), BERR(:)
      REAL(WP), ALLOCATABLE :: RR(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SGESVX Example Program Results.'
!     WRITE(*,*)'Size of matrix A, N = ?'
!     READ ( *, * ) N
      N = 2
!     WRITE(*,*)'Number of right hand sides, NRHS = ?'
!     READ ( *, * ) NRHS
      NRHS = 1
      ALLOCATE( A(N,N), AA(N,N), B(N,NRHS), X(N,NRHS),BB(N,NRHS), IPIV(N), RR(N,N), FERR(NRHS), BERR(NRHS) )

!     OPEN(UNIT=21,FILE='data/gesv.dat',STATUS='UNKNOWN')
!     DO J=1,N
!     DO I=1,N
!        READ(21,'(F2.0)') AA(I,J)
!     ENDDO
!     ENDDO
!     CLOSE(21)
      AA(1,1) = 0; AA(1,2) = 1; AA(2,1) = 0; AA(2,2) = 1

!      DO I = 1, N; READ (*, *) (RR(I, J), J = 1, N); ENDDO
!      AA=RR

      DO J = 1, NRHS; BB(:,J) = SUM( AA, DIM=2)*J; ENDDO

      WRITE(*,*) 'The matrix A:'
      DO I=1,N; WRITE(*,"(4(I3,1X),I3,1X)") INT(AA(I,:)); ENDDO

      WRITE(*,*) 'The RHS matrix B:'
      DO I=1,N; WRITE(*,"(2(I3,1X),I3,1X)") INT(BB(I,:)); ENDDO

      WRITE(*,*) 'CALL GESVX( A, B, X, FERR=FERR, BERR=BERR, ', &
                 'RCOND=RCOND, RPVGRW=RPVGRW, INFO =INFO )'
      A=AA; B=BB
      CALL GESVX( A, B, X, FERR=FERR, BERR=BERR, RCOND=RCOND, &
                     RPVGRW=RPVGRW, INFO=INFO )

      WRITE(*,*) 'INFO = ', INFO
      WRITE(*,*) 'FERR = ', FERR
      WRITE(*,*) 'BERR = ', BERR
      WRITE(*,*) 'RCOND = ', RCOND
      WRITE(*,*) 'RPVGRW = ', RPVGRW

      WRITE(*,*) '\noindent'
      WRITE(*,*) 'The solution of the system $ A\,X = B $ is:'
      WRITE(*,*) '$$ X = \left( \begin{array}{rrr}'
      DO I=1,N; WRITE(*,"(2(F9.5,' & '),F9.5,' \\')") X(I,:); ENDDO
      WRITE(*,*) '\end{array} \right). $$'

      WRITE(*,*) 'The matrix A on exit:'
      DO I=1,N; WRITE(*,*) AA(I,:); ENDDO

      DEALLOCATE(A, AA, B, X, BB, IPIV, RR, FERR, BERR)

      END PROGRAM SGESVX_MAIN
