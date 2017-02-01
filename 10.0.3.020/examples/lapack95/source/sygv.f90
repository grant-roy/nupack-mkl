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
!     S S Y G V  Example Program Text
!*******************************************************************************

      PROGRAM SSYGV_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: SYGV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:), W(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SYEV Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), B(N,N), BB(N,N), W(N) )

      A = 0
!      OPEN(UNIT=21,FILE='data/sygv_a.dat',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N
!            READ(21,*) A(I,J)
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      AA=TRANSPOSE(A)

      WRITE(*,*)'Matrix A : '
      DO I=1,N;
         WRITE(*,"(5(I3,1X))") INT(A(I,:));
      ENDDO

      B = 0
!      OPEN(UNIT=21,FILE='data/sygv_b.dat',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N
!            READ(21,*) B(I,J)
            READ(*,*) B(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      BB=TRANSPOSE(B)

      WRITE(*,*)'Matrix B : '
      DO I=1,N;
         WRITE(*,"(5(I3,1X))") INT(B(I,:));
      ENDDO

      WRITE(*,*) 'CALL SYGV( A, B, W )'
      CALL SYGV( A, B, W )

      WRITE(*,*)'Matrix B on exit:'
      DO I=1,N;
         WRITE(*,"(5(E14.6,1X))") B(I,:);
      ENDDO

      WRITE(*,*) 'W on exit : '
      DO I=1,N
         WRITE(*,"(5(E14.6,1X))") W(I)
      ENDDO

      WRITE(*,*)
      WRITE(*,*) ' * EXAMPLE 2 * '

      WRITE(*,*)'Matrix A : '
      DO I=1,N;
         WRITE(*,"(5(I3,1X))") INT(AA(I,:));
      ENDDO

      WRITE(*,*)'Matrix B : '
      DO I=1,N;
         WRITE(*,"(5(I3,1X))") INT(BB(I,:));
      ENDDO

      WRITE(*,*) "CALL SYGV( A, B, W, '2', 'V', 'L', INFO )"
      CALL SYGV( AA, BB, W, 2, 'V', 'L', INFO )

      WRITE(*,*)'Matrix A on exit:'
      DO I=1,N;
         WRITE(*,"(5(E14.6,1X))") AA(I,:);
      ENDDO

      WRITE(*,*)'Matrix B on exit:'
      DO I=1,N;
         WRITE(*,"(5(E14.6,1X))") BB(I,:);
      ENDDO

      WRITE(*,*) 'W on exit : '
      DO I=1,N
         WRITE(*,"(5(F11.5,1X))") W(I)
      ENDDO

      WRITE(*,*) ' INFO = ', INFO

      DEALLOCATE(A, AA, B, BB, W)

      END PROGRAM SSYGV_MAIN
