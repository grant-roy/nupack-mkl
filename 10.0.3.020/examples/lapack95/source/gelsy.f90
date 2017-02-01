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
!     S G E L S Y  Example Program Text
!*******************************************************************************

      PROGRAM SGELSY_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: GELSY
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, M, N, NRHS, RANK
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: JPVT(:)
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'GELSY Example Program Results'
      M=6; N = 4; NRHS = 3
      WRITE(*,'(5H M = , I4, 5H N = , I4, 9H; NRHS = , I4)') M, N, NRHS
      ALLOCATE( A(M,N), AA(M,N), B(MAX(M,N),NRHS), BB(MAX(M,N),NRHS), JPVT(N) )

      A = 0
      B = 0
!      OPEN(UNIT=21,FILE='data/gelss_a.dat',STATUS='UNKNOWN')
      DO I=1,M
         DO J=1,N
!            READ(21,'(F2.0)') A(I,J)
            READ(*,'(F2.0)') A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix A :'
      DO I=1,M;
         WRITE(*,"(24(F9.5))") A(I,:);
      ENDDO

!      OPEN(UNIT=22,FILE='data/gelss_b.dat',STATUS='UNKNOWN')
      DO I=1,MAX(M,N)
         DO J=1,NRHS
!            READ(22,'(F3.0)') B(I,J)
            READ(*,'(F3.0)') B(I,J)
         ENDDO
      ENDDO
!      CLOSE(22)

      WRITE(*,*)'Matrix B :'
      DO I=1,M;
         WRITE(*,"(3(F9.5))") B(I,:);
      ENDDO

      AA=A
      BB=B

      JPVT(1)=0; JPVT(2)=0; JPVT(3)=1; JPVT(4)=0;

      WRITE(*,*) 'CALL GELSY( A, B, RANK, JPVT, 1.0E-5_WP )'

      CALL GELSY( A, B, RANK, JPVT, 1.0E-5_WP )

      WRITE(*,*) 'The matrix B on exit :'
      DO I=1,M;
         WRITE(*,"(3(E15.7),1X)") B(I,:);
      ENDDO


      WRITE(*,*)'RANK = ', RANK
      WRITE(*,*)'JPVT = ', JPVT

      DEALLOCATE(A, AA, B, BB, JPVT)

      END PROGRAM SGELSY_MAIN
