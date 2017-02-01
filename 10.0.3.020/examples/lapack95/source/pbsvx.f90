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
!     S P B S V X  Example Program Text
!*******************************************************************************

      PROGRAM SPBSVX_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: PBSVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Parameters" ..
      CHARACTER(LEN=1) :: EQUED
!  .. "Local Scalars" ..
      INTEGER :: KD, I, J, INFO, N, NRHS
      REAL(WP) :: RCOND
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), X(:,:), S(:)
      REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:), FERR(:), BERR(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SPBSVX Example Program Results.'
      N = 7; KD = 3; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      ALLOCATE ( A(KD+1,N), AA(KD+1,N), B(N,NRHS), BB(N,NRHS) )
      ALLOCATE ( X(N,NRHS), S(N), FERR(NRHS), BERR(NRHS) )

      A = 0
!      OPEN(UNIT=21,FILE='data/pbsv_a.dat',STATUS='UNKNOWN')
      DO I=1,KD+1
         DO J=1,N
!            READ(21,'(F3.0)') A(I,J);
            READ(*,'(F3.0)') A(I,J);
         ENDDO
      ENDDO
!      CLOSE(21)


      B(:,1) = 0.0
      DO I = 1, N
         DO J = MAX(1,-N+I+KD+1), KD
            B(I,1) = A(J,I-J+KD+1) + B(I,1)
         ENDDO
         DO J = MAX(1,KD+2-I), KD+1
            B(I,1) = A(J,I) + B(I,1)
         ENDDO
      ENDDO
      DO J = 2, NRHS; B(:,J) = B(:,1)*J; ENDDO
         AA = A; BB = B

      WRITE(*,*) 'AB on entry:'
      DO I = 1, KD+1; WRITE (*,'(7(F9.5))') A(I,:); ENDDO

      WRITE(*,*) 'The RHS matrix B:'
      DO J = 1, N; WRITE (*,'(3(F10.5))') B(J,:); ENDDO

print*, "***"
      CALL PBSVX(A, B, X, FACT='E', EQUED=EQUED, S=S, FERR=FERR, BERR=BERR, RCOND=RCOND, INFO=INFO)
print*, "***"
      WRITE(*,*)'EQUED = ',EQUED
      WRITE(*,*)'S on exit :'
      DO I=1,N; WRITE(*,"(5(F8.5))") S(I);
      ENDDO
      WRITE(*,*)'X on exit :'
      DO I=1,N; WRITE(*,"(5(F8.5))") X(I,:);
      ENDDO
      WRITE(*,*)'FERR on exit :'
      DO I=1,NRHS; WRITE(*,"(5(E13.6))") FERR(I);
      ENDDO
      WRITE(*,*)'BERR = '
      DO I=1,NRHS; WRITE(*,"(5(E13.6))") BERR(I);
      ENDDO
      WRITE(*,*)'RCOND = ',RCOND

      WRITE(*,*)'INFO = ' ,INFO

      WRITE(*,*) '\noindent'
      WRITE(*,*) 'The solution of the system $ A\,X = B $ is:'
      WRITE(*,*) '$$ X = \left( \begin{array}{rrr}'
      DO I=1,N; WRITE(*,"(2(F9.5,' & '),F9.5,' \\')") X(I,:); ENDDO
      WRITE(*,*) '\end{array} \right). $$'

      DEALLOCATE(A, AA, B, BB, X, S, FERR, BERR)

      END PROGRAM SPBSVX_MAIN
