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
      PROGRAM LA_SSBEVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE MKL95_PRECISION, ONLY: WP => SP
      USE MKL95_LAPACK, ONLY: SBEVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, M, N, KD
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), W(:), Z(:,:), Q(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_SBEVX Example Program Results'
      N = 5; KD = 2;
      ALLOCATE( A(KD+1,N), W(N), Z(N,N), Q(N,N) )

 !     OPEN(UNIT=21,FILE='sbevu.ma',STATUS='UNKNOWN')
      DO I=1,KD+1
         DO J=1,N
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix A:'
      DO I=1,KD+1;
         WRITE(*,"(5(I3,1X))") INT(A(I,:));
      ENDDO

      WRITE(*,*) 'CALL LA_SBEVX( A, W, Z=Z, VL=-4.0_WP, VU=100.0_WP, M=M, Q=Q )'
      CALL SBEVX( A, W, Z=Z, VL=-4.0_WP, VU=100.0_WP, M=M, Q=Q)

      WRITE(*,*) 'W on exit : '
      DO I=1,M
      WRITE(*,"(5(F9.5))") W(I)
      ENDDO
      WRITE(*,*) 'M = ',M

      WRITE(*,*) 'The matrix Q'
      WRITE(*,'(5(F9.5))') ((Q(I,J),J=1,N),I=1,N)

      END PROGRAM LA_SSBEVX_EXAMPLE


