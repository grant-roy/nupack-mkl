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
!     S G G L S E  Example Program Text
!*******************************************************************************

      PROGRAM SGGLSE_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
!      USE mkl95_LAPACK, ONLY: GGLSE, LANGE
      USE mkl95_LAPACK, ONLY: GGLSE
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" .
      REAL(WP) ::  R1
      INTEGER :: I, J, INFO, M, N, P
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), C(:), D(:), X(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'GGLSE Example Program Results'
      M=5; N = 3; P = 2
      ALLOCATE( A(M,N), B(P,N), C(M), D(P), X(N) )

      A = 0
!      OPEN(UNIT=21,FILE='data/gglse_a.dat',STATUS='UNKNOWN')
      DO I=1,M
         DO J=1,N
!            READ(21,*) A(I,J)
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      B = 0
!      OPEN(UNIT=22,FILE='data/gglse_b.dat',STATUS='UNKNOWN')
      DO I=1,P
         DO J=1,N
!            READ(22,*) B(I,J)
            READ(*,*) B(I,J)
         ENDDO
      ENDDO
!      CLOSE(22)

      C = 0
!      OPEN(UNIT=23,FILE='data/gglse_c.dat',STATUS='UNKNOWN')
      DO I=1,M
!         READ(23,*) C(I)
         READ(*,*) C(I)
      ENDDO
!      CLOSE(23)

      D = 0
!      OPEN(UNIT=24,FILE='data/gglse_d.dat',STATUS='UNKNOWN')
      DO I=1,P
!         READ(24,*) D(I)
         READ(*,*) D(I)
      ENDDO
!      CLOSE(24)

      WRITE(*,*)'Matrix A :'
      DO I=1,M;
         WRITE(*,"(3(F9.5))") A(I,:);
      ENDDO

      WRITE(*,*)'Matrix B : '
      DO I=1,P;
         WRITE(*,"(3(F9.5))") B(I,:);
      ENDDO

      WRITE(*,*)'Vector C : '
      DO I=1,M;
         WRITE(*,"(F9.5)") C(I);
      ENDDO

      WRITE(*,*) 'Vector D : '
      DO I=1,P;
         WRITE(*,"(F9.5)") D(I);
      ENDDO
      WRITE(*,*)
      WRITE(*,*) 'CALL GGLSE( A, B, C, D, X, INFO )'
      CALL GGLSE( A, B, C, D, X, INFO )

      WRITE(*,*) 'C on exit : '
      DO I=1,M;
         WRITE(*,"(E14.6)") C(I);
      ENDDO

      WRITE(*,*) 'X on exit : '
      DO I=1,N;
         WRITE(*,"(E14.5)") X(I);
      ENDDO

      WRITE(*,*)'INFO = ',INFO

      DEALLOCATE(A, B, C, D, X)

      END PROGRAM SGGLSE_MAIN
