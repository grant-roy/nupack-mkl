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
!     C H P G V  Example Program Text
!*******************************************************************************

      PROGRAM CHPGV_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: HPGV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N
      REAL(WP), ALLOCATABLE :: W(:)
!  .. "Local Arrays" ..
      COMPLEX(WP), ALLOCATABLE :: A(:), AA(:), B(:), BB(:), Z(:,:)
!  .. "Intrinsic Functions" ..
      INTRINSIC REAL, AIMAG, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'HPGV Example Program Results'
      N = 5
      ALLOCATE( A(N*(N+1)/2), AA(N*(N+1)/2), B(N*(N+1)/2), BB(N*(N+1)/2), W(N), Z(N,N) )

      A = 0
!      OPEN(UNIT=21,FILE='data/hpgvu_a.dat',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
!         READ(21,*) A(J)
         READ(*,*) A(J)
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix AP : '
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(I3,',',I3)')')") INT(A(I)), INT(AIMAG(A(I)))
      ENDDO

      B = 0
!      OPEN(UNIT=21,FILE='data/hpgvu_b.dat',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
!         READ(21,*) B(J)
         READ(*,*) B(J)
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix B : '
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(I3,',',I3)')')") INT(B(I)), INT(AIMAG(B(I)))
      ENDDO

      WRITE(*,*) "CALL HPGV( AP, BP, W) "
      CALL HPGV( A, B, W )

      WRITE(*,*)'BP on exit : '
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(E14.6,1X,','E14.6)')')") REAL(B(I)), AIMAG(B(I))
      ENDDO

      WRITE(*,*) 'W on exit : '
      DO I=1,N
         WRITE(*,"(5(E14.6))") W(I)
      ENDDO

      WRITE(*,*)
      WRITE(*,*)' * EXAMPLE 2 * '

      AA = 0
!      OPEN(UNIT=21,FILE='data/hpgvl_a.dat',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
!         READ(21,*) AA(J)
         READ(*,*) AA(J)
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix AP : '
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(I3,',',I3)')')") INT(AA(I)), INT(AIMAG(AA(I)))
      ENDDO

      BB = 0
!      OPEN(UNIT=21,FILE='data/hpgvl_b.dat',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
!         READ(21,*) BB(J)
         READ(*,*) BB(J)
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix BP : '
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(I3,',',I3)')')") INT(BB(I)), INT(AIMAG(BB(I)))
      ENDDO

      WRITE(*,*)
!      WRITE(*,*) "CALL HPGV( A, B, W, JOBZ, UPLO, INFO )"
!      CALL HPGV( AA, BB, W, 3, 'L', Z, INFO )
       WRITE(*,*) "CALL HPGV( A, B, W, 3, 'L', Z, INFO)"
       CALL HPGV( AA, BB, W, 3, 'L', Z, INFO )

      WRITE(*,*)'Z on exit : '
      DO I=1,N
         DO J=1,N-1
            WRITE(*,"('('(E14.6,1X,',',1X,E14.6)')')") REAL(Z(I,J)), AIMAG(Z(I,J))
         ENDDO
         WRITE(*,"('('(E14.6,1X,',',1X,E14.6)')')") REAL(Z(I,N)), AIMAG(Z(I,N))
      ENDDO
      WRITE(*,*)'BP on exit : '
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(E14.6,1X,',',1X,E14.6)')')") REAL(BB(I)), AIMAG(BB(I))
      ENDDO

      WRITE(*,*) 'W on exit : '
      DO I=1,N
      WRITE(*,"(5(E14.6,1X))") W(I)
      ENDDO
      WRITE(*,*) ' INFO = ', INFO

      DEALLOCATE(A, AA, B, BB, W, Z)

      END PROGRAM CHPGV_MAIN
