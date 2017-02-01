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
!     S S B E V  Example Program Text
!*******************************************************************************

      PROGRAM SSBEV_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: SBEV, SBEVD
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N, KD
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), W(:), Z(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SYEV Example Program Results'
      N = 5; KD = 2;
      ALLOCATE( A(KD+1,N), W(N), Z(N,N) )

      A = 0
!      OPEN(UNIT=21,FILE='data/sbevu_a.dat',STATUS='UNKNOWN')
      DO I=1,KD+1
         DO J=1,N
!            READ(21,*) A(I,J)
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix A:'
      DO I=1,KD+1;
         WRITE(*,"(5(I3,1X))") INT(A(I,:));
      ENDDO

      WRITE(*,*) 'CALL SBEV( A, W )'

      CALL SBEV(  A, W)

      WRITE(*,*) 'W on exit : '
      DO I=1,N
         WRITE(*,"(5(F9.5))") W(I)
      ENDDO

      A = 0
!      OPEN(UNIT=21,FILE='data/sbevl_a.dat',STATUS='UNKNOWN')
      DO I=1,KD+1
         DO J=1,N
!            READ(21,*) A(I,J)
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      WRITE(*,*)
      WRITE(*,*) ' * EXAMPLE 2 * '

      WRITE(*,*)'Matrix A : '
      DO I=1,KD+1;
         WRITE(*,"(5(I3,1X))") INT(A(I,:));
      ENDDO

      WRITE(*,*) "CALL SBEVD( A, W, 'L', Z, INFO )"
      CALL SBEVD( A, W, 'L', Z, INFO )

      WRITE(*,*) 'Z on exit : '
      DO I=1,N;
         WRITE(*,"(5(E14.6,1X))") Z(I,:);
      ENDDO

      WRITE(*,*) 'W on exit : '
      DO I=1,N
         WRITE(*,"(5(F9.5))") W(I)
      ENDDO

      WRITE(*,*) ' INFO = ', INFO

      DEALLOCATE(A, W, Z)

      END PROGRAM SSBEV_MAIN
