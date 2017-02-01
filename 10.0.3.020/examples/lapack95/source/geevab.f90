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
      INTEGER :: I, INFO, N
      REAL(WP), ALLOCATABLE ::  WR(:), WI(:), VL(:,:), VR(:,:)
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:)
      COMPLEX(WP), ALLOCATABLE :: AA(:,:), W(:), VLC(:,:), VRC(:,:)
!  .. "Intrinsic Functions" ..
!     INTRINSIC  REAL, AIMAG, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'GEEV V.A. Barker Program Results'
      N = 3
      ALLOCATE( A(N,N), AA(N,N), W(N), WR(N), WI(N), VL(N,N), VR(N,N), &
                VLC(N,N), VRC(N,N) )

      A(1,1) =  0.0; A(1,2) = 1.0; A(1,3) = 0.0
      A(2,1) = -1.0; A(2,2) = 0.0; A(2,3) = 0.0
      A(3,1) =  0.0; A(3,2) = 0.0; A(3,3) = 1.0

      WRITE(*,*)'Matrix A:'
      DO I=1,N
         WRITE(*,*) A(I,:)
      ENDDO

      WRITE(*,*) "CALL GEEV( A, WR, WI, VL, VR, INFO ) "
      CALL GEEV( A, WR, WI, VL, VR, INFO )

      WRITE(*,*) ' INFO = ', INFO

      WRITE(*,*) 'WR on exit : '
      WRITE(*,*) WR(1:N)

      WRITE(*,*) 'WI on exit : '
      WRITE(*,*) WI(1:N)

      WRITE(*,*)'Matrix VR:'
      DO I=1,N
         WRITE(*,*) VR(I,:)
      ENDDO

      WRITE(*,*)'Matrix VL:'
      DO I=1,N
         WRITE(*,*) VL(I,:)
      ENDDO

      WRITE(*,*)
      WRITE(*,*) ' * EXAMPLE 2 * '

      AA(1,1) =  0.0; AA(1,2) = 1.0; AA(1,3) = 0.0
      AA(2,1) = -1.0; AA(2,2) = 0.0; AA(2,3) = 0.0
      AA(3,1) =  0.0; AA(3,2) = 0.0; AA(3,3) = (0.0,1.0)

      WRITE(*,*)'Matrix AA:'
      DO I=1,N
         WRITE(*,*) AA(I,:)
      ENDDO

      WRITE(*,*) "CALL GEEV( AA, W, VLC, VRC, INFO ) "
      CALL GEEV( AA, W, VLC, VRC, INFO )

      WRITE(*,*) ' INFO = ', INFO

      WRITE(*,*) 'W on exit : '
      WRITE(*,*) W(1:N)

      WRITE(*,*)'Matrix VRC:'
      DO I=1,N
         WRITE(*,*) VRC(I,:)
      ENDDO

      WRITE(*,*)'Matrix VLC:'
      DO I=1,N
         WRITE(*,*) VLC(I,:)
      ENDDO

      DEALLOCATE(A, AA, W, WR, WI, VL, VR, VLC, VRC)

      END PROGRAM CGEEV_MAIN
