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
!     S S P S V X  Example Program Text
!*******************************************************************************

      PROGRAM SSPSVX_MAIN

!  .. "Use Statements"
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: SPSVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, INFO, N, NN, NRHS
!  .. Local Arrays ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      REAL(WP), ALLOCATABLE :: B(:,:), AP(:), X(:,:), AFP(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SSPSVX Example Program Results.'
      N = 5; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      NN = N*(N+1)/2
      ALLOCATE ( AP(NN), B(N,NRHS), X(N,NRHS), AFP(NN), IPIV(N) )

      AP = 0
!      OPEN(UNIT=21,FILE='data/spsv_a.dat',STATUS='UNKNOWN')
      DO I=1,NN
!            READ(21,'(F3.0)') AP(I)
            READ(*,'(F3.0)') AP(I)
         ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix AP :'
      DO I=1,NN; WRITE(*,"(15(I3,1X,1X),I3,1X)") INT(AP(I));
      ENDDO

      B = 0
!      OPEN(UNIT=21,FILE='data/spsv_b.dat',STATUS='UNKNOWN')
      DO I=1,N
!         READ(21,'(F3.0)') B(I,1)
         READ(*,'(F3.0)') B(I,1)
      ENDDO
!      CLOSE(21)

      WRITE(*,*)"CALL SPSVX( AP, B, X, 'L', AFP, IPIV, INFO=INFO)"
      CALL SPSVX( AP, B, X, 'L', AFP, IPIV, INFO=INFO)

      WRITE(*,*)'AFP = '
      DO I=1,NN; WRITE(*,"(F8.5)") AFP(I);
      ENDDO
      WRITE(*,*)'IPIV = ', IPIV

      DEALLOCATE(AP, B, X, AFP, IPIV)

      END PROGRAM SSPSVX_MAIN
