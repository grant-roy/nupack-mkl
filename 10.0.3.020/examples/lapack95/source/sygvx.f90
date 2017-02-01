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
      PROGRAM LA_SSYGVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE MKL95_PRECISION, ONLY: WP => SP
      USE MKL95_LAPACK, ONLY: SYGVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, M, N
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IFAIL(:)
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:), W(:), Z(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_SYEVX Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), B(N,N), BB(N,N), W(N), IFAIL(N), Z(N,N) )

!      OPEN(UNIT=21,FILE='sygv.ma',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      AA=A

      WRITE(*,*)'Matrix A:'
      DO I=1,N;
         WRITE(*,"(5(I3,1X))") INT(A(I,:));
      ENDDO

!      OPEN(UNIT=21,FILE='sygv.mb',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N
            READ(*,*) B(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      BB=B

      WRITE(*,*)'Matrix B:'
      DO I=1,N;
         WRITE(*,"(5(I3,1X))") INT(B(I,:));
      ENDDO

      WRITE(*,*) 'CALL LA_SYGVX( A, B, W, 3, Z=Z, VL=-10.0_WP, VU=10.0_WP, M=M, IFAIL=IFAIL )'
      CALL SYGVX(  A, B, W, 3, Z=Z, VL=-10.0_WP, VU=10.0_WP, M=M, IFAIL=IFAIL )

      WRITE(*,*)'Matrix B on exit:'
      DO I=1,N;
         WRITE(*,"(5(F12.7,1X))") B(I,:);
      ENDDO

      WRITE(*,*) 'W on exit : '
      DO I=1,M
      WRITE(*,"(5(F12.7,1X))") W(I)
      ENDDO

      WRITE(*,*) 'M=',M

      WRITE(*,*)'IFAIL = ', IFAIL

      WRITE(*,*) 'Z on exit : '
      DO J=1,M
      WRITE(*,"(5(F12.6,1X))") (Z(I,J), I = 1,N)
      ENDDO

      END PROGRAM LA_SSYGVX_EXAMPLE


