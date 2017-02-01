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
!     S H E S V  Example Program Text
!*******************************************************************************

      PROGRAM SHESV_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: HESV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, NRHS
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      COMPLEX(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SHESV Example Program Results'
      N = 5; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      ALLOCATE ( A(N,N), AA(N,N), B(N,NRHS), BB(N,NRHS), IPIV(N) )

      A = 0
!      OPEN(UNIT=21,FILE='data/hesv_a.dat',STATUS='UNKNOWN')
      DO I=1,N
         DO J=I,N
!         READ(21,*) A(I,J);
         READ(*,*) A(I,J);
         ENDDO
      ENDDO;
!      CLOSE(21)

      WRITE(*,*) 'The matrix A :'
      DO I=1,N
      WRITE(*,"(5(I3,1X,'+',1X,I3,'i',1X))") INT(A(I,1)),INT(AIMAG(A(I,1))), &
                                             INT(A(I,2)),INT(AIMAG(A(I,2))), &
                                             INT(A(I,3)),INT(AIMAG(A(I,3))), &
                                             INT(A(I,4)),INT(AIMAG(A(I,4))), &
                                             INT(A(I,5)),INT(AIMAG(A(I,5)))
      ENDDO

      B = 0
!      OPEN(UNIT=21,FILE='data/hesv_b.dat',STATUS='UNKNOWN')
      DO I=1,N
!         READ(21,*) B(I,1);
         READ(*,*) B(I,1);
      ENDDO;
!      CLOSE(21)

      WRITE(*,*) 'The array B :'
      DO I=1,N
      WRITE(*,"((I3,1X,'+',1X,I3,'i',1X))") INT(B(I,1)),INT(AIMAG(B(I,1)))
      ENDDO

      BB=B
      WRITE(*,*)' CALL HESV( A, B, IPIV=IPIV )'
      CALL HESV( A, B, IPIV=IPIV )

      WRITE(*,*)'A on exit: '
      DO I=1,N; WRITE(*,*) A(I,:);
      ENDDO

      WRITE(*,*)'B on exit: '
      DO I=1,N; WRITE(*,*) (B(I,1));
      ENDDO

      WRITE(*,*)'IPIV on exit : ', IPIV

      DEALLOCATE(A, AA, B, BB, IPIV)

      END PROGRAM SHESV_MAIN
