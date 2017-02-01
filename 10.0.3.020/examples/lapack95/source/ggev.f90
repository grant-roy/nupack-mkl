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

      PROGRAM CGGEV_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: GGEV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N
!  .. "Local Arrays" ..
      COMPLEX(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:), ALPHA(:)
      COMPLEX(WP), ALLOCATABLE :: VL(:,:), VR(:,:), BETA(:)
      REAL(WP), ALLOCATABLE :: AR(:,:), BR(:,:), ALPHAR(:), ALPHAI(:), BETAR(:), &
                               VLR(:,:), VRR(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'GGEV Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), B(N,N), BB(N,N), ALPHA(N), BETA(N), VL(N,N), VR(N,N) )
      ALLOCATE( AR(N,N), BR(N,N), ALPHAR(N), ALPHAI(N), BETAR(N), VLR(N,N), VRR(N,N) )

      WRITE (*,*) 'Example 1, Real Example'
      AR = 0
!      OPEN(UNIT=21,FILE='data/gges_a.dat',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N
!            READ(21,*) AR(I,J)
            READ(*,*) AR(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)
      WRITE(*,*)'Matrix AR : '
      DO I=1,N;
         WRITE(*,"(5(I3,1X))") INT(AR(I,:));
      ENDDO

      BR = 0
!      OPEN(UNIT=21,FILE='data/gges_b.dat',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N
!            READ(21,*) BR(I,J)
            READ(*,*) BR(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)
      WRITE(*,*)'Matrix BR : '
      DO I=1,N;
         WRITE(*,"(5(I3,1X))") INT(BR(I,:));
      ENDDO
      WRITE(*,*)
      WRITE(*,*) 'CALL GGEV( A, B, ALPHAR, ALPHAI, BETAR, VLR, VRR )'
      CALL GGEV( AR, BR, ALPHAR, ALPHAI, BETAR, VLR, VRR )

      WRITE(*,*); WRITE(*,*)'ALPHAR on exit : '; WRITE(*,*) ALPHAR(1:N)
      WRITE(*,*); WRITE(*,*)'ALPHAI on exit : '; WRITE(*,*) ALPHAI(1:N)
      WRITE(*,*); WRITE(*,*)'BETA on exit : '; WRITE(*,*) BETAR(1:N)
      WRITE(*,*)'Array VL:'; DO I =1,N; WRITE(*,*)I, VLR(I,1:N); ENDDO
      WRITE(*,*)'Array VR:'; DO I =1,N; WRITE(*,*)I, VRR(I,1:N); ENDDO

      WRITE(*,*)
      WRITE(*,*)' Generalized eigenvalues : '
      DO I=1,N
         WRITE(*,*) '(',ALPHAR(I)/BETAR(I),',',ALPHAI(I)/BETAR(I),')'
      ENDDO

      WRITE (*,*) 'Example 2, Complex Example'
!      OPEN(UNIT=21,FILE='data/ggev_a.dat',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N
!            READ(21,*) A(I,J)
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      AA=A

      WRITE(*,*)'Matrix A : '
      DO I=1,N
         WRITE(*,"(5('('(I3,1X,',',I3)')',1X,1X))") INT(A(I,:)), INT(AIMAG(A(I,:)))
      ENDDO

!      OPEN(UNIT=21,FILE='data/ggev_b.dat',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N
!            READ(21,*) B(I,J)
            READ(*,*) B(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      BB=B

      WRITE(*,*)'Matrix B : '
      DO I=1,N
         WRITE(*,"(5('('(I3,1X,',',I3)')',1X,1X))") INT(B(I,:)), INT(AIMAG(B(I,:)))
      ENDDO
      WRITE(*,*)
      WRITE(*,*) 'CALL GGEV( A, B, ALPHA, BETA, INFO=INFO )'
      CALL GGEV( A, B, ALPHA, BETA,  INFO=INFO )

      WRITE(*,*)
      WRITE(*,*)'ALPHA on exit : '
      DO I=1,N
         WRITE(*,*) ALPHA(I)
      ENDDO

      WRITE(*,*)
      WRITE(*,*)'BETA on exit : '
      DO I=1,N
         WRITE(*,*) BETA(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*)'INFO = ', INFO

      WRITE(*,*)
      WRITE(*,*)' Generalized eigenvalues : '
      DO I=1,N
         WRITE(*,*)  ALPHA(I)/BETA(I)
      ENDDO

      WRITE(*,*)
!     WRITE(*,*)' * EXAMPLE 2 * '
      WRITE(*,*) 'CALL GGEV( A, B, ALPHAR, ALPHAI, BETA, VL, VR )'
      CALL GGEV( A, B, ALPHA, BETA, VL, VR )

      WRITE(*,*)
      WRITE(*,*)'Matrix VL on exit : '
      DO I=1,N;
         WRITE(*,"(5('('(F8.5,1X,',',F8.5)')'))") REAL(VL(I,:)), AIMAG(VL(I,:))
      ENDDO

      WRITE(*,*)
      WRITE(*,*)'Matrix VR on exit : '
      DO I=1,N;
         WRITE(*,"(5('('(F8.5,1X,',',F8.5)')'))") REAL(VR(I,:)), AIMAG(VR(I,:))
      ENDDO

      DEALLOCATE(A, AA, B, BB, ALPHA, BETA, VL, VR)
      DEALLOCATE(AR, BR, ALPHAR, ALPHAI, BETAR, VLR, VRR)

      END PROGRAM CGGEV_MAIN
