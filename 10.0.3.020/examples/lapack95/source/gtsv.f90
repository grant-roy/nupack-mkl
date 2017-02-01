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
!     S G T S V  Example Program Text
!*******************************************************************************

      PROGRAM SGTSV_MAIN

!  .. "Use Statements"
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: GTSV
!  .."Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, INFO, N, NRHS
      INTEGER, ALLOCATABLE :: IPIV(:)
!  .. Local Arrays ..
      REAL(WP), ALLOCATABLE :: DL(:), D(:), DU(:), B(:,:),X(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'GTSV Example Program Results.'
      N = 6; NRHS = 3

      ALLOCATE(DL(N-1),D(N),DU(N-1),B(N,NRHS), X(N,NRHS), IPIV(N) )

      DL = 0
      D = 0
      DU = 0
!      OPEN(UNIT=21,FILE='data/gtsv_a.dat',STATUS='UNKNOWN')
          DO I=1,N-1
!          READ(21,'(F2.0)') DL(I)
          READ(*,'(F2.0)') DL(I)
          ENDDO
          DO I=1,N
!          READ(21,'(F2.0)') D(I)
          READ(*,'(F2.0)') D(I)
          ENDDO
          DO I=1,N-1
!          READ(21,'(F2.0)') DU(I)
          READ(*,'(F2.0)') DU(I)
          ENDDO
!      CLOSE(21)

      WRITE(*,*)'DU :'
      WRITE(*,"(I3,1X)") INT(DU(:));

      WRITE(*,*)'D :'
      WRITE(*,"(I3,1X)") INT(D(:));

      WRITE(*,*)'DL :'
      WRITE(*,"(I3,1X)") INT(DL(:));

      DO I = 2, N-1; B(I,:) = DL(I-1) + D(I) + DU(I); ENDDO
      B(1,:) = D(1) + DU(1);B(N,:) = DL(N-1) + D(N)
      DO I = 1, NRHS; B(:,I) = B(:,I)*I; ENDDO

      WRITE(*,*) 'The RHS matrix B:'
      DO I=1,N; WRITE(*,"(3(I3,1X))") INT(B(I,:)); ENDDO

      WRITE(*,*) ' CALL GTSV(DL, D, DU, B, INFO)'
      CALL GTSV(DL, D, DU, B, INFO )

      WRITE(*,*)'DL on exit: '
      WRITE(*,"(F8.5)") DL(:);
      WRITE(*,*)'D on exit: '
      WRITE(*,"(F8.5)") D(:);
      WRITE(*,*)'DU on exit: '
      WRITE(*,"(F8.5)") DU(:);
      WRITE(*,*)'B on exit: '
      DO I=1,N; WRITE(*,"(6(F8.5))") B(I,:); ENDDO

      WRITE(*,*)'INFO = ',INFO

      DEALLOCATE(DL, D, DU, B, X, IPIV)

      END PROGRAM SGTSV_MAIN
