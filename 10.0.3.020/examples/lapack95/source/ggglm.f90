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
!     S G G G L M  Example Program Text
!*******************************************************************************

      PROGRAM SGGGLM_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: GGGLM
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" .
      INTEGER :: I, J, INFO, M, N, P
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), D(:), X(:), Y(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'GGGLM Example Program Results'
      M=5; N = 4; P = 2
      ALLOCATE( A(M,N), B(M,P), D(M), X(N), Y(P) )

      A = 0
!      OPEN(UNIT=21,FILE='data/ggglm_a.dat',STATUS='UNKNOWN')
      DO I=1,M
         DO J=1,N
!            READ(21,*) A(I,J)
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      B = 0
!      OPEN(UNIT=22,FILE='data/ggglm_b.dat',STATUS='UNKNOWN')
      DO I=1,M
         DO J=1,P
!            READ(22,*) B(I,J)
            READ(*,*) B(I,J)
         ENDDO
      ENDDO
!      CLOSE(22)

!      OPEN(UNIT=24,FILE='data/ggglm.dat',STATUS='UNKNOWN')
      DO I=1,M
!         READ(24,*) D(I)
         READ(*,*) D(I)
      ENDDO
!      CLOSE(24)

      WRITE(*,*)'Matrix A :'
      DO I=1,M;
         WRITE(*,"(4(F9.5))") A(I,:);
      ENDDO
      WRITE(*,*)'Matrix B :'
      DO I=1,M;
         WRITE(*,"(2(F9.5))") B(I,:);
      ENDDO
      WRITE(*,*)'Vector D :'
      DO I=1,M;
         WRITE(*,"(F9.5)") D(I);
      ENDDO

      WRITE(*,*) 'CALL GGGLM( A, B, D, X, Y, INFO )'
      CALL GGGLM( A, B, D, X, Y, INFO )
      WRITE(*,*) 'X on exit:'
      DO I=1,N; WRITE(*,"(E14.6)") X(I); ENDDO

      WRITE(*,*) 'Y on exit:'
      DO I=1,P; WRITE(*,"(E14.6)") Y(I); ENDDO

      WRITE(*,*)'INFO = ',INFO

      DEALLOCATE(A, B, D, X, Y)

      END PROGRAM SGGGLM_MAIN
