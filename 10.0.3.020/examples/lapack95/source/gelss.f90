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
!     S G E L S S  Example Program Text
!*******************************************************************************

      PROGRAM SGELSS_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: GELSS
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" .
      REAL(WP) ::  R1, R2, R3
      INTEGER :: RANK, I, J, INFO, M, N, NRHS
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:), S(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'GELSS Example Program Results'
      M=6; N = 4; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      ALLOCATE( A(M,N), AA(M,N), B(M,NRHS), BB(M,NRHS), S(N) )

      A = 0
      B = 0
!      OPEN(UNIT=21,FILE='data/gelss_a.dat',STATUS='UNKNOWN')
      DO I=1,M
         DO J=1,N
!            READ(21,*) A(I,J)
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

!      OPEN(UNIT=22,FILE='data/gelss_b.dat',STATUS='UNKNOWN')
      DO I=1,M
         DO J=1,NRHS
!            READ(22,*) B(I,J)
            READ(*,*) B(I,J)
         ENDDO
      ENDDO
!      CLOSE(22)

      WRITE(*,*)'Matrix A :'
      DO I=1,M;
         WRITE(*,"(24(F9.5))") A(I,:);
      ENDDO

      WRITE(*,*)'Matrix B :'
      DO I=1,M;
         WRITE(*,"(3(F9.5))") B(I,:);
      ENDDO

      WRITE(*,*) 'CALL GELSS( A, B, RANK, S, 0.00001_WP, INFO )'
      CALL GELSS( A, B, RANK, S, 0.00001_WP, INFO=INFO )

      WRITE(*,*) ' A on exit : '
      DO I=1,M;
         WRITE(*,"(4(E14.6))") A(I,:);
      ENDDO

      WRITE(*,*) ' B on exit : '
      DO I=1,M;
         WRITE(*,"(3(E14.6))") B(I,:);
      ENDDO

      WRITE(*,*) ' S on exit : '
      DO I=1,N;
         WRITE(*,"(E14.6)") S(I);
      ENDDO

      WRITE(*,*) 'RANK = ', RANK
      WRITE(*,*)'INFO = ',INFO

      DEALLOCATE(A, AA, B, BB, S)

      END PROGRAM SGELSS_MAIN
