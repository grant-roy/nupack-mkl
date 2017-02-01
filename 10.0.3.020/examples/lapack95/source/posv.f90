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
!     S P O S V  Example Program Text
!*******************************************************************************

      PROGRAM SPOSV_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: POSV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, NRHS
      CHARACTER(LEN=1) :: UPLO
!  .. "Local Arrays" ..
 !    INTEGER, DIMENSION(NP) :: IPUT
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), AA(:,:),BB(:,:), S(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SPOSV Example Program Results'
      N = 5; NRHS = 3

      ALLOCATE ( A(N,N), B(N,NRHS), AA(N,N), BB(N,NRHS), S(N) )

      A = 0
!      OPEN(UNIT=21,FILE='data/posv_a.dat',STATUS='UNKNOWN')
      DO I=1,N
         DO J=I,N
!         READ(21,'(F3.0)') A(I,J);
         READ(*,'(F3.0)') A(I,J);
         ENDDO
      ENDDO;
!      CLOSE(21)

      WRITE(*,*) 'The array A :'
      DO I=1,N
      WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(A(I,1:N));
      ENDDO

      DO I = 1, N
        DO J = 1, NRHS
        B(I,J) = (SUM(A(I,I:N)) + SUM(A(1:I-1,I)))*J
        ENDDO
      ENDDO

      AA = TRANSPOSE(A)
      BB = B
      WRITE(*,*) 'The array B :'
      DO I=1,N
      WRITE(*,'(3(I3,1X,1X))') INT(B(I,1:NRHS));
      ENDDO

      WRITE(*,*) 'CALL POSV( A, B )'
      CALL POSV(  A, B )

      WRITE(*,*)'A on exit: '
      DO I=1,N; WRITE(*,"(5(E15.6))") A(I,1:N);
      ENDDO

      WRITE(*,*)'B on exit: '
      DO I=1,N; WRITE(*,"(3(F9.5))") B(I,1:NRHS);
      ENDDO

      WRITE(*,*)' * Example 2 * '

      WRITE(*,*) 'The array A :'
      DO J=1,N
      WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(AA(J,:));
      ENDDO

      WRITE(*,*) 'The array B(:,1) :'
      Do I=1,N
      WRITE(*,'(3(I3,1X,1X))') INT(BB(I,1));
      ENDDO

      WRITE(*,*) "CALL POSV( A, B(:,1), 'L' )"
      UPLO='L'
      CALL POSV(  AA, BB(:,1), 'L' )

      WRITE(*,*)'A on exit: '
      DO J=1,N; WRITE(*,"(6(E13.6))") AA(J,:); ENDDO

      WRITE(*,*)'B on exit: '
      DO I=1,N; WRITE(*,"(3(F8.5))") BB(I,1); ENDDO

      DEALLOCATE(A, B, AA, BB, S)

      END PROGRAM SPOSV_MAIN
