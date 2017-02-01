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
!     S P O S V X  Example Program Text
!*******************************************************************************

      PROGRAM SPOSVX_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: POSVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, NRHS
      CHARACTER(LEN=1) :: EQUED
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), X(:,:), S(:), &
                               FERR(:), BERR(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SPOSVX Example Program Results'
      N = 5; NRHS = 3
      ALLOCATE ( A(N,N), B(N,NRHS), X(N,NRHS), S(N), FERR(NRHS), &
                 BERR(NRHS) )

      A = 0
!      OPEN(UNIT=21,FILE='data/posv_a.dat',STATUS='UNKNOWN')
      DO I=1,N
         DO J=I,N
!         READ(21,'(F3.0)') A(I,J);
         READ(*,'(F3.0)') A(I,J);
         ENDDO
      ENDDO;
!      CLOSE(21)

      A(:,1)=1E-6*A(:,1);A(1,2:N)=1E-6*A(1,2:N)

      DO J = 1, NRHS; DO I = 1, N
      B(I,J) = (SUM(A(I,I:N)) + SUM(A(1:I-1,I)))*J
      ENDDO; ENDDO

      WRITE(*,*) 'The array B :'
      DO J=1,NRHS; DO I = 1, N
       WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(B(I,J));
      ENDDO; ENDDO

      WRITE(*,*) 'CALL POSVX( A, B, X, FACT="E", EQUED=EQUED, S=S )'
      CALL POSVX(  A, B, X, FACT='E', EQUED=EQUED, S=S )

      WRITE(*,*)'EQUED = ', EQUED
      WRITE(*,*)'S = ', S
      WRITE(*,*)'FERR = ', FERR
      WRITE(*,*)'BERR = ', BERR

      WRITE(*,*) 'The solution of the system $ A\,X = B $ is:'
      WRITE(*,*) '$$ X = \left( \begin{array}{rrr}'
      DO I=1,N; WRITE(*,"(2(F9.5,' & '),F9.5,' \\')") X(I,:); ENDDO
      WRITE(*,*) '\end{array} \right). $$'

      DEALLOCATE(A, B, X, S, FERR)

      END PROGRAM SPOSVX_MAIN
