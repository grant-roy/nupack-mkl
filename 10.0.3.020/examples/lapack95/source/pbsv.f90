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
!     S P B S V  Example Program Text
!*******************************************************************************

      PROGRAM SPBSV_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: PBSV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Parameters" ..
      REAL(WP), PARAMETER :: ZERO = 0.0_WP, ONE = 1.0_WP
!  .. "Local Scalars" ..
      INTEGER :: KD, I, J, INFO, N, NRHS
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: AB(:,:), B(:,:)
      REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SPBSV Example Program Results.'
      N = 7; KD = 3; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      ALLOCATE ( AB(KD+1,N), AA(KD+1,N), B(N,NRHS), BB(N,NRHS) )

      AB = 0
!      OPEN(UNIT=21,FILE='data/pbsv_a.dat',STATUS='UNKNOWN')
      DO I=1,KD+1
         DO J=1,N
!         READ(21,'(F3.0)') AB(I,J);
         READ(*,'(F3.0)') AB(I,J);
         ENDDO
      ENDDO
!      CLOSE(21)

      B(:,1) = ZERO
      DO I = 1, N
         DO J = MAX(1,-N+I+KD+1), KD
         B(I,1) = AB(J,I-J+KD+1) + B(I,1)
         ENDDO
         DO J = MAX(1,KD+2-I), KD+1
         B(I,1) = AB(J,I) + B(I,1)
         ENDDO
      ENDDO

      DO J = 2, NRHS; B(:,J) = B(:,1)*J; ENDDO
      AA = AB; BB = B

      WRITE(*,*) 'AB on entry:'
      DO I = 1,KD+1;
         WRITE (*,'(7(F8.5))') AB(I,:);
      ENDDO

      WRITE(*,*) 'The RHS matrix B:'
      DO J = 1, N; WRITE (*,*) B(J,:);
      ENDDO

      CALL PBSV(AB, B, INFO=INFO)

      WRITE(*,*)'AB on exit :'
      DO I=1,KD+1; WRITE(*,"(10(E12.6))") AB(I,:);
      ENDDO

      WRITE(*,*)'B on exit :'
      DO J=1,N; WRITE(*,"(10(F8.5))") B(J,:);
      ENDDO

      WRITE(*,*)'INFO = ' ,INFO

      DEALLOCATE(AB, AA, B, BB)

      END PROGRAM SPBSV_MAIN
