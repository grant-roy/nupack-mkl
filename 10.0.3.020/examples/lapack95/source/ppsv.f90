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
!     S P P S V  Example Program Text
!*******************************************************************************

      PROGRAM SPPSV_MAIN

!  .. "Use Statements"
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: PPSV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, NN, NRHS
!  .. Local Arrays ..
      REAL(WP), ALLOCATABLE :: AP(:) ,B(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SPPSV Example Program Results.'
      N = 5; NRHS = 1
      NN = N*(N+1)/2
      ALLOCATE ( AP(NN), B(N,NRHS) )

      AP = 0
!      OPEN(UNIT=21,FILE='data/posv_a.dat',STATUS='UNKNOWN')
      DO I=1,NN
!         READ(21,'(F3.0)') AP(I);
         READ(*,'(F3.0)') AP(I);
      ENDDO;
!      CLOSE(21)

      WRITE(*,*) 'The array AP :'
      DO I=1,NN
      WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(AP(I));
      ENDDO

      B = 0
!      OPEN(UNIT=21,FILE='data/ppsv_b.dat',STATUS='UNKNOWN')
      DO I=1,N
      DO J=1,NRHS
!         READ(21,'(F3.0)') B(I,J);
         READ(*,'(F3.0)') B(I,J);
      ENDDO;
      ENDDO;
!      CLOSE(21)

      WRITE(*,*) 'The array B :'
      DO I=1,N
       WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(B(I,:));
      ENDDO;

      WRITE(*,*) "CALL PPSV( AP, B, 'L' )"
      CALL PPSV(  AP, B, 'L' )

      WRITE(*,*)'AP on exit :'
      DO I=1,NN; WRITE(*,"(5(E13.6))") AP(I);
      ENDDO

      WRITE(*,*)'B on exit :'
      DO I=1,N; WRITE(*,"(5(E13.6))") B(I,:);
      ENDDO

      DEALLOCATE(AP, B)

      END PROGRAM SPPSV_MAIN
