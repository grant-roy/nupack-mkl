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
!     S S Y S V  Example Program Text
!*******************************************************************************

      PROGRAM SSYSV_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: SYSV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N, NRHS
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:)
!  .. "Intrinsic Functions" ..
      INTRINSIC  SUM
!  .. "Executable Statements" ..
      WRITE (*,*) 'SSYSV Example Program Results'
      N = 5; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      ALLOCATE ( A(N,N), AA(N,N), B(N,NRHS), BB(N,NRHS), IPIV(N) )

      A = 0
!      OPEN(UNIT=21,FILE='data/sysv_a.dat',STATUS='UNKNOWN')
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
      AA=TRANSPOSE(A)
      DO I = 1, N
        DO J = 1, NRHS
        B(I,J) = (SUM(A(I,I:N)) + SUM(A(1:I-1,I)))*J
        ENDDO
      ENDDO

      WRITE(*,*) 'The array B :'
      DO I=1,N
            WRITE(*,'(3(I3,1X,1X))') INT(B(I,1:NRHS));
      ENDDO
      BB=B
      WRITE(*,*)' CALL SYSV( A, B, IPIV=IPIV )'
      CALL SYSV( A, B, IPIV=IPIV )

      WRITE(*,*)'A on exit: '
      DO J=1,N; WRITE(*,"(6(E13.5))") A(J,:);
      ENDDO

      WRITE(*,*)'B on exit: '
      DO I=1,N; WRITE(*,"(3(F9.5))") B(I,:);
      ENDDO

      WRITE(*,*)'IPIV on exit : ', IPIV

      WRITE(*,*)' * EXAMPLE 2 * '

      WRITE(*,*) 'The array A :'
      DO I=1,N
      WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(AA(I,1:N));
      ENDDO
      WRITE(*,*) 'The array B :'
      DO I=1,N
      WRITE(*,'(3(I3,1X,1X))') INT(BB(I,1));
      ENDDO

      WRITE(*,*)"CALL SYSV( A, B(:,1), 'L', IPIV, INFO )"
      CALL SYSV( AA, BB(:,1), 'L', IPIV, INFO )

      WRITE(*,*)'A on exit: '
      DO J=1,N; WRITE(*,"(6(E13.5))") AA(J,:);
      ENDDO

      WRITE(*,*)'B(:,1) on exit: '
      DO I=1,N; WRITE(*,"(3(F9.5))") BB(I,1);
      ENDDO

      WRITE(*,*)' IPIV = ',IPIV
      WRITE(*,*)' INFO = ',INFO

      DEALLOCATE(A, AA, B, BB, IPIV)

      END PROGRAM SSYSV_MAIN
