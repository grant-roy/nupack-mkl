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
!     S P P S V X  Example Program Text
!*******************************************************************************

      PROGRAM SPPSVX_MAIN

!  .. "Use Statements"
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: PPSVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, NN, NRHS
      REAL(WP) :: RCOND
!  .. Local Arrays ..
      REAL(WP), ALLOCATABLE :: A(:), B(:,:),X(:,:), FERR(:), BERR(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SPPSVX Example Program Results.'
      N = 5; NRHS = 1
      NN = N*(N+1)/2
      ALLOCATE ( A(NN), B(N,NRHS), X(N,NRHS),FERR(NRHS), BERR(NRHS) )

      A = 0
!      OPEN(UNIT=21,FILE='data/posv_a.dat',STATUS='UNKNOWN')
      DO I=1,NN
!         READ(21,'(F3.0)') A(I);
         READ(*,'(F3.0)') A(I);
      ENDDO;
!      CLOSE(21)

      WRITE(*,*) 'The array A :'
      DO I=1,NN
      WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(A(I));
      ENDDO

      B = 0
!      OPEN(UNIT=21,FILE='data/ppsv_b.dat',STATUS='UNKNOWN')
      DO I=1,N
      DO J=1,NRHS
!         READ(21,'(F3.0)') B(I,J);
         READ(*,'(F3.0)') B(I,J);
      ENDDO;
      ENDDO;
!      CLOSE(21)

      WRITE(*,*) 'The array B :'
      DO I=1,N
      WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(B(I,:));
      ENDDO;

      WRITE(*,*) 'CALL PPSVX( A, B, X, "L", FERR=FERR, BERR=BERR, RCOND=RCOND )'
      CALL PPSVX(  A, B, X, 'L', FERR=FERR, BERR=BERR, RCOND=RCOND )

      WRITE(*,*)'X on exit :'
      DO I=1,N; WRITE(*,"(5(E13.6))") X(I,:);
      ENDDO
      WRITE(*,*)'FERR on exit :'
      DO I=1,NRHS; WRITE(*,"(5(E13.6))") FERR(I);
      ENDDO
      WRITE(*,*)'BERR = '
      DO I=1,NRHS; WRITE(*,"(5(E13.6))") BERR(I);
      ENDDO
      WRITE(*,*)'RCOND = ', RCOND

      WRITE(*,*) '\noindent'
      WRITE(*,*) 'The solution of the system $ A\,X = B $ is:'
      WRITE(*,*) '$$ X = \left( \begin{array}{rrr}'
      DO I=1,N; WRITE(*,"(2(F9.5,' & '),F9.5,' \\')") X(I,:); ENDDO
      WRITE(*,*) '\end{array} \right). $$'

      DEALLOCATE(A, B, X, FERR, BERR)

      END PROGRAM SPPSVX_MAIN
