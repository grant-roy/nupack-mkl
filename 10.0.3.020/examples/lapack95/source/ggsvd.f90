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
!     C G G S V D  Example Program Text
!*******************************************************************************

      PROGRAM CGGSVD_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: GGSVD
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, M, N, P, K, L
      REAL(WP), ALLOCATABLE :: ALPHA(:), BETA(:)
!  .. "Local Arrays" ..
      COMPLEX(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:), U(:,:), V(:,:), Q(:,:)
!  .. "Intrinsic Functions" ..
      INTRINSIC REAL, AIMAG, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'GGSVD Example Program Results'
      N = 3; M=5; P=2
      ALLOCATE( A(M,N), AA(M,N), B(P,N), BB(P,N), ALPHA(N), BETA(N), U(M,M), V(P,P), Q(N,N) )

      A = 0
!      OPEN(UNIT=21,FILE='data/ggsvd_a.dat',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,M
!            READ(21,*) A(I,J)
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      AA=A

      WRITE(*,*)'Matrix A : '
      DO I=1,M
         WRITE(*,"(3('('(I3,1X,',',I3)')',1X,1X))") INT(A(I,1)),INT(AIMAG(A(I,1))), &
              INT(A(I,2)),INT(AIMAG(A(I,2))), &
              INT(A(I,3)),INT(AIMAG(A(I,3)))
      ENDDO

      B = 0
!      OPEN(UNIT=21,FILE='data/ggsvd_b.dat',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,P
!            READ(21,*) B(I,J)
            READ(*,*) B(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      BB=B
      WRITE(*,*)
      WRITE(*,*)'Matrix B : '
      DO I=1,P
         WRITE(*,"(3('('(I3,1X,',',I3)')',1X,1X))") INT(B(I,1)), INT(AIMAG(B(I,1))), &
              INT(B(I,2)),INT(AIMAG(B(I,2))), &
              INT(B(I,3)),INT(AIMAG(B(I,3)))
      ENDDO

      WRITE(*,*)
      WRITE(*,*) "CALL GGSVD( A, B, ALPHA, BETA, K, L ) "
      CALL GGSVD( A, B, ALPHA, BETA, K, L )

      WRITE(*,*)'A on exit : '
      DO I=1,M;
         WRITE(*,"(3('('(E13.6,1X,',',E13.6)')',1X))") REAL(A(I,1)),AIMAG(A(I,1)), &
              REAL(A(I,2)),AIMAG(A(I,2)), &
              REAL(A(I,3)),AIMAG(A(I,3))
      ENDDO
      WRITE(*,*)
      WRITE(*,*)'ALPHA on exit : '
      DO I=1,N
         WRITE(*,"((E14.6,1X))") ALPHA(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*)'BETA on exit : '
      DO I=1,N
         WRITE(*,"((E14.6,1X))") BETA(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*)' K = ', K
      WRITE(*,*)' L = ', L

      WRITE(*,*)
      WRITE(*,*)' * EXAMPLE 2 * '
      WRITE(*,*)
      WRITE(*,*) "CALL GGSVD( A, B, ALPHA, BETA, K, L, U, V, Q, INFO=INFO )"
      CALL GGSVD(  AA, BB, ALPHA, BETA, K, L, U, V, Q , INFO=INFO )

      WRITE(*,*)'U on exit : '
      DO I=1,M;
         WRITE(*,"(5('('(E13.6,1X,',',1X,E13.6)')',1X))") REAL(U(I,1)), AIMAG(U(I,1)), &
              REAL(U(I,2)), AIMAG(U(I,2)), &
              REAL(U(I,3)), AIMAG(U(I,3)), &
              REAL(U(I,4)), AIMAG(U(I,4)), &
              REAL(U(I,5)), AIMAG(U(I,5))
      ENDDO
      WRITE(*,*)
      WRITE(*,*)'V on exit : '
      DO I=1,P;
         WRITE(*,"(2('('(E13.6,1X,',',E13.6)')',1X,1X))") REAL(V(I,1)), AIMAG(V(I,1)), &
              REAL(V(I,2)), AIMAG(V(I,2))
      ENDDO
      WRITE(*,*)
      WRITE(*,*)'Q on exit : '
      DO I=1,N;
         WRITE(*,"(3('('(E13.6,1X,',',E13.6)')',1X))") REAL(Q(I,1)), AIMAG(Q(I,1)), &
              REAL(Q(I,2)), AIMAG(Q(I,2)), &
              REAL(Q(I,3)), AIMAG(Q(I,3))
      ENDDO
      WRITE(*,*)
      WRITE(*,*) ' INFO = ', INFO

      DEALLOCATE(A, AA, B, BB, ALPHA, BETA, U, V, Q)

      END PROGRAM CGGSVD_MAIN
