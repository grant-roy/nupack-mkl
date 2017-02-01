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
!     C G E E V  Example Program Text
!*******************************************************************************

      PROGRAM CGEEV_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: GEEV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N
      REAL(WP), ALLOCATABLE ::  WR(:), WI(:)
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:)
      COMPLEX(WP), ALLOCATABLE :: AA(:,:), W(:), VL(:,:), VR(:,:)
!  .. "Intrinsic Functions" ..
      INTRINSIC  REAL, AIMAG, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'GEEV Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), W(N), WR(N), WI(N), VL(N,N), VR(N,N) )

      A = 0
      AA = 0
!      OPEN(UNIT=21,FILE='data/geev1.dat',STATUS='UNKNOWN')
      DO I=1,N
         DO J=1,N
!            READ(21,*) A(I,J)
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix A:'
      DO I=1,N
         WRITE(*,"(5(I3,1X))") INT(A(I,:))
      ENDDO

      WRITE(*,*) "CALL GEEV( A, WR, WI ) "
      CALL GEEV( A, WR, WI )

      WRITE(*,*) 'WR on exit : '
      DO I=1,N
         WRITE(*,"((F9.6,1X))") REAL(WR(I))
      ENDDO

      WRITE(*,*) 'WI on exit : '
      DO I=1,N
         WRITE(*,"((F9.5,1X))") REAL(WI(I))
      ENDDO

      WRITE(*,*)
      WRITE(*,*) ' * EXAMPLE 2 * '

!      OPEN(UNIT=21,FILE='data/geev.dat',STATUS='UNKNOWN')
      DO I=1,N
         DO J=1,N
!            READ(21,*) AA(I,J)
            READ(*,*) AA(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix A : '
      DO J=1,N
         WRITE(*,"(5(I3,1X,'+',I3,'i',1X,1X,1X))") INT(AA(J,1)),INT(AIMAG(AA(J,1))), &
              INT(AA(J,2)),INT(AIMAG(AA(J,2))), &
              INT(AA(J,3)),INT(AIMAG(AA(J,3))), &
              INT(AA(J,4)),INT(AIMAG(AA(J,4))), &
              INT(AA(J,5)),INT(AIMAG(AA(J,5)))
      ENDDO

      WRITE(*,*) "CALL GEEV( A, W, VL, VR, INFO )"
      CALL GEEV( AA, W, VL, VR, INFO )

      WRITE(*,*) ' W on exit : '
      DO I=1,N;
         WRITE(*,"('('E14.7,1X,',',1X,E14.7,')',1X)")  W(I)
      ENDDO

      WRITE(*,*)' VL on exit : '
      DO I=1,N;
         WRITE(*,"(5('('E14.7,1X,',',1X,E14.7,')',1X))")  VL(I,:)
      ENDDO;

      WRITE(*,*) ' VR on exit : '
      DO I=1,N;
         WRITE(*,"(5('('E14.7,1X,',',1X,E14.7,')',1X))")  VR(I,:)
      ENDDO;

      WRITE(*,*) ' INFO = ', INFO

      DEALLOCATE( A, AA, W, WR, WI, VL, VR )

      END PROGRAM CGEEV_MAIN
