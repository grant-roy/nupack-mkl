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
!     S G B S V X  Example Program Text
!*******************************************************************************

      PROGRAM SGBSVX_MAIN

!  .. "Use Statements"
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: GBSVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: K, KL, KU, I, J, N, NRHS
      CHARACTER(LEN=1) :: EQUED
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: AB(:,:), B(:,:), X(:,:), R(:), C(:), &
                               FERR(:), BERR(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'GBSVX Example Program Results.'
      N = 6; KL = 2; KU = 1; NRHS = 2
      ALLOCATE ( AB(  KL+KU+1,N), B(N,NRHS), X(N,NRHS), R(N), C(N), &
                 FERR(NRHS),BERR(NRHS) )

      AB = 0
!      OPEN(UNIT=21,FILE='data/gbsv.dat',STATUS='UNKNOWN')
!     DO I=KL+1,2*KL+KU+1
      DO I=   1,  KL+KU+1
      DO J=1,N
!         READ(21,'(F2.0)') AB(I,J)
         READ(*,'(F2.0)') AB(I,J)
      ENDDO
      ENDDO
!      CLOSE(21)

      AB(:,1:1)=1e-6*AB(:,1:1)
      DO J = 1, N
        DO K = 1, 1+KU-MIN(J-1,KU)-1; AB(K,J) = HUGE(1.0_WP); END DO
        DO K = 1+ KU+MIN(N-J,KL)+KL+1, 1+KU+KL; AB(K,J) = HUGE(1.0_WP); END DO
      END DO
      WRITE(*,*) 'The array AB:'
      DO I=1,N; WRITE(*,*) AB(I,:); ENDDO

      B = 0.0_WP
      DO I = 1, NRHS;
        DO J = 1, N
          DO K = 1+KU-MIN(J-1,KU), 1+ KU+MIN(N-J,KL)
            B(J,I) = AB(K,J)+B(J,I);
          ENDDO
        ENDDO;
        B(:,I) = B(:,I)*I;
      ENDDO

      WRITE(*,*) 'The RHS matrix B:'
      DO I=1,N; WRITE(*,"(1(I3,1X),I3,1X)") INT(B(I,:)); ENDDO

      WRITE(*,*) "CALL GBSVX( AB, B, X, 2, TRANS='T', EQUED=EQUED,", &
                 " R=R, C=C, FERR=FERR, BERR=BERR )"

      CALL GBSVX( AB, B, X, 2, FACT='E',TRANS='T',EQUED=EQUED, &
                     R=R, C=C, FERR=FERR, BERR=BERR )

      WRITE(*,*)'X on exit: '
      DO I=1,N; WRITE(*,"(1(E12.6,1X),E12.6,1X)") X(I,:); ENDDO
      WRITE(*,*)'EQUED = ', EQUED
      WRITE(*,*)'R = ', R
      WRITE(*,*)'C = ', C
      WRITE(*,*)'FERR = ', FERR
      WRITE(*,*)'BERR = ', BERR

      DEALLOCATE(AB, B, X, R, C, FERR, BERR)

      END PROGRAM SGBSVX_MAIN
