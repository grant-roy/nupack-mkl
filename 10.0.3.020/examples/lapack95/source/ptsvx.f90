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
!     S P T S V X  Example Program Text
!*******************************************************************************

      PROGRAM SPTSVX_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: PTSVX
!  .. "IMPLICIT STATEMENT" ..
      IMPLICIT NONE
!  .. "Parameters" ..
      REAL(WP), PARAMETER :: ONE = 1.0_WP, THREE = 3.0_WP
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N, NRHS
      REAL(WP) ::  RCOND
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:), D(:), E(:), B(:,:), X(:,:), FERR(:), BERR(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SPTSVX Program Results.'
      N = 5; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      ALLOCATE ( A(N), D(N), E(N-1), B(N,NRHS), X(N,NRHS), FERR(NRHS), BERR(NRHS) )

      A = 0
!      OPEN(UNIT=21,FILE='data/ptsv_a.dat',STATUS='UNKNOWN')
      DO I=1,N
!         READ(21,'(F3.0)') A(I);
         READ(*,'(F3.0)') A(I);
      ENDDO;
!      CLOSE(21)

      E = THREE; D(1) = 2*E(1)+ONE; D(2:N) = 2*E+ONE
      B(1,1) = D(1)+E(1); B(N,1) = E(N-1)+D(N)
      B(2:N-1,1) = 2*E + D(2:N-1)
      DO I = 2, NRHS; B(:,I) = B(:,1)*I; ENDDO
         WRITE(*,*) '  on entry:'
         WRITE (*,'(2HD:,8(1X,F9.2))') D
         WRITE (*,'(2HE:,8(1X,F9.2))') E
         WRITE(*,*) 'The RHS matrix B:'
         DO J = 1, N; WRITE (*,'(3(F9.5))') B(J,:);
         ENDDO

      CALL PTSVX( D, E, B, X, FERR=FERR, BERR=BERR, RCOND=RCOND, INFO=INFO)

      WRITE(*,*) 'FERR on exit :'
      DO I = 1, N-1; WRITE (*,'(E13.5)') FERR(I);
      ENDDO
      WRITE(*,*)'BERR on exit :'
      DO I = 1, N-1; WRITE (*,'(E13.5)') BERR(I);
      ENDDO
      WRITE(*,*) 'RCOND =', RCOND
      WRITE(*,*) 'The matrix X on exit:'
      DO J = 1, N; WRITE (*,'(7(F9.5))') X(J,:);
      ENDDO
      WRITE(*,*)'INFO = ' ,INFO

      WRITE(*,*) '\noindent'
      WRITE(*,*) 'The solution of the system $ A\,X = B $ is:'
      WRITE(*,*) '$$ X = \left( \begin{array}{rrr}'
      DO I=1,N; WRITE(*,"(2(F9.5,' & '),F9.5,' \\')") X(I,:); ENDDO
      WRITE(*,*) '\end{array} \right). $$'

      DEALLOCATE(A, D, E, B, X, FERR, BERR)

      END PROGRAM SPTSVX_MAIN
