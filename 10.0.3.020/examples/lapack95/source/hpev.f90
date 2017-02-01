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
!      Example shows how to migrate from NETLIB LAPACK95 to
!      Intel(R) Math Kernel Library (MKL) LAPACK95
!*******************************************************************************
      PROGRAM LA_CHPEV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE MKL95_PRECISION, ONLY: WP => SP
      USE MKL95_LAPACK, ONLY: HPEV, HPEVD
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, INFO, N
      REAL(WP), ALLOCATABLE :: W(:)
!  .. "Local Arrays" ..
      COMPLEX(WP), ALLOCATABLE :: A(:), AA(:), Z(:,:)
!  .. "Intrinsic Functions" ..
      INTRINSIC  AIMAG, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_HPEV Example Program Results'
      N = 5
      ALLOCATE( A(N*(N+1)/2), AA(N*(N+1)/2), W(N), Z(N,N) )

!      OPEN(UNIT=21,FILE='hpev.ma',STATUS='UNKNOWN')
      DO I=1,N*(N+1)/2
         READ(*,*) A(I)
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix A : '
      DO I=1,N*(N+1)/2
         WRITE(*,"((I3,1X,'+',1X,I3,'i',1X))") INT(A(I)),INT(AIMAG(A(I)))
      ENDDO

      WRITE(*,*) "CALL LA_HPEV( A, W) "
      CALL HPEV( A, W )

      WRITE(*,*) 'W on exit : '
      DO I=1,N
      WRITE(*,"(5(E14.6))") W(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*) ' * EXAMPLE 2 * '

!      OPEN(UNIT=21,FILE='hpev.mat',STATUS='UNKNOWN')
      DO I=1,N*(N+1)/2
         READ(*,*) A(I)
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix A : '
      DO I=1,N*(N+1)/2
         WRITE(*,"((I3,1X,'+',1X,I3,'i',1X))") INT(A(I)),INT(AIMAG(A(I)))
      ENDDO

      WRITE(*,*) "CALL LA_HPEVD( A, W, 'L', Z, INFO )"
      CALL HPEVD( A, W, 'L', Z, INFO )

      WRITE(*,*)'Z on exit: '
      DO I=1,N; WRITE(*,*) Z(I,:);
      ENDDO

      WRITE(*,*) ' INFO = ', INFO

      END PROGRAM LA_CHPEV_EXAMPLE
