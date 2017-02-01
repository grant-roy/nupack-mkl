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
      PROGRAM LA_SGESV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE MKL95_PRECISION, ONLY: WP => SP
      USE MKL95_LAPACK, ONLY: GESV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N, NRHS
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SGESV Example Program Results.'
!      OPEN(UNIT=21,FILE='gesv.ma',STATUS='UNKNOWN')
      READ (*, * ) N
      READ (*, * ) NRHS
      ALLOCATE( A(N,N), AA(N,N), B(N,NRHS), BB(N,NRHS), IPIV(N) )
      DO J=1,N
      DO I=1,N
         READ(*,'(F2.0)') AA(I,J)
      ENDDO
      ENDDO
!      CLOSE(21)

      DO J = 1, NRHS; BB(:,J) = SUM( AA, DIM=2)*J; ENDDO

      WRITE(*,*) 'The matrix A:'
      DO I=1,N; WRITE(*,"(4(I3,1X),I3,1X)") INT(AA(I,:)); ENDDO

      WRITE(*,*) 'The RHS matrix B:'
      DO I=1,N; WRITE(*,"(2(I3,1X),I3,1X)") INT(BB(I,:)); ENDDO

      WRITE(*,*) 'CALL LA_GESV( A, B )'
      A=AA; B=BB

      CALL GESV(  A, B )

      WRITE(*,*) 'B - the solution vectors computed by LA_GESV'
      DO I=1,N; WRITE(*,"(2(E12.6,1X),E12.6,1X)") B(I,:); ENDDO

      WRITE(*,*) 'CALL LA_GESV( A, B(:,1), IPIV, INFO )'

      CALL GESV(  AA, BB(:,1), IPIV, INFO )

      WRITE(*,*) ' A on exit:'
      DO I=1,N; WRITE(*,"(4(E12.6,1X),E12.6,1X)") AA(I,:); ENDDO

      WRITE(*,*) 'B on exit:'
      DO I=1,N; WRITE(*,"(4(E12.6,1X),E12.6,1X)") BB(I,1); ENDDO

      WRITE(*,*)'IPIV on exit:', IPIV

      WRITE(*,*)'INFO on exit:', INFO

      END PROGRAM LA_SGESV_EXAMPLE









