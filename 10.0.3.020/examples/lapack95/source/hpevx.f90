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
!     C H P E V X  Example Program Text
!*******************************************************************************

      PROGRAM CHPEVX_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: HPEVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N
      REAL(WP), ALLOCATABLE :: W(:)
!  .. "Local Arrays" ..
      COMPLEX(WP), ALLOCATABLE :: A(:), AA(:), Z(:,:)
!  .. "Intrinsic Functions" ..
      INTRINSIC AIMAG, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'HPEVX Example Program Results'
      N = 5
      ALLOCATE( A(N*(N+1)/2), AA(N*(N+1)/2), W(N), Z(N,N) )

      A = 0
!      OPEN(UNIT=21,FILE='data/hpev_a.dat',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
!         READ(21,*) A(J)
         READ(*,*) A(J)
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix A:'
      DO I=1,N*(N+1)/2
         WRITE(*,"((I3,1X,'+',1X,I3,'i',1X))") INT(A(I)), INT(AIMAG(A(I)))
      ENDDO


      WRITE(*,*) "CALL HPEVX( A, W, IL=2, IU=5, ABSTOL=1.0E-5_WP ) "
      W = 0
      CALL HPEVX( A, W, IL=2, IU=5, ABSTOL=1.0E-5_WP )

      WRITE(*,*) 'W on exit : '
      DO I=1,N
      WRITE(*,"(5(F9.5))") W(I)
      ENDDO

      DEALLOCATE(A, AA, W, Z)

      END PROGRAM CHPEVX_MAIN
