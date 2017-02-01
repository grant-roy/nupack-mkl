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
!     S G E S V D  Example Program Text
!*******************************************************************************

      PROGRAM SGESVD_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: GESVD
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, M, N
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), S(:), U(:,:), V(:,:), VT(:,:), WW(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'GESVD Example Program Results'
      N = 5; M=3
      ALLOCATE( A(M,N), AA(M,N), S(MIN(M,N)), U(M,M), V(N,N), VT(N,N), WW(MIN(M,N)-1))

      A = 0
!      OPEN(UNIT=21,FILE='data/gesvd.dat',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,M
!            READ(21,*) A(I,J)
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      AA=A

      WRITE(*,*)'Matrix A:'
      DO I=1,M; WRITE(*,"(5(I3,1X))") INT(A(I,:)); ENDDO

      WRITE(*,*)
      WRITE(*,*) 'CALL GESVD( A, S )'
      CALL GESVD( A, S )

      WRITE(*,*) 'S on exit : '
      WRITE(*,"(3(F10.5,1X))") S(:)

      WRITE(*,*)
      WRITE(*,*)' * EXAMPLE 2 * '
      WRITE(*,*) "CALL GESVD( A, S, VT=VT, WW=WW, JOB='U', INFO=INFO )"
      CALL GESVD( AA, S, VT=VT, WW=WW, JOB='U', INFO=INFO )

      WRITE(*,*) 'A on exit : '
      DO I=1,M;
         WRITE(*,"(5(E14.6,1X))") AA(I,:);
      ENDDO

      WRITE(*,*) 'VT on exit : '
      DO I=1,N;
         WRITE(*,"(5(E14.6,1X))") VT(I,:);
      ENDDO

      WRITE(*,*)'WW on exit : '
      WRITE(*,"(5(E14.6,1X))") WW(:)

      WRITE(*,*) ' INFO = ', INFO

      DEALLOCATE(A, AA, S, U, V, VT, WW)

      END PROGRAM SGESVD_MAIN
