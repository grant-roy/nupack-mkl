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
!     S G T S V X  Example Program Text
!*******************************************************************************

      PROGRAM SGTSVX_MAIN

!  .. "Use Statements"
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: GTSVX
!  .."Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, N, NRHS
      INTEGER, ALLOCATABLE :: IPIV(:)
      CHARACTER(LEN=1) :: TRANS
!  .. Local Arrays ..
      REAL(WP), ALLOCATABLE :: DL(:), D(:), DU(:), DLF(:), &
                DF(:), DUF(:), DU2(:), B(:,:),X(:,:)

!  .. "Executable Statements" ..
       WRITE (*,*) 'SGTSVX Example Program Results.'
       N = 6; NRHS = 3

       ALLOCATE(DL(N-1), DLF(N-1), D(N), DF(N), DU(N-1), DUF(N-1), &
                DU2(N-2), B(N,NRHS), X(N,NRHS), IPIV(N) )

       DL = 0
       D = 0
       DU = 0
!       OPEN(UNIT=21,FILE='data/gtsv_a.dat',STATUS='UNKNOWN')
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
!       CLOSE(21)

       WRITE(*,*)'DU :'
       WRITE(*,"(8(I3,1X))") INT(DU(:));
       WRITE(*,*)'D :'
       WRITE(*,"(8(I3,1X))") INT(D(:));
       WRITE(*,*)'DL :'
       WRITE(*,"(8(I3,1X))") INT(DL(:))

       B = 0.0_WP
       DO I = 2, N-1; B(I,:) = DU(I-1) + D(I) + DL(I); ENDDO
       B(1,:) = D(1) + DL(1);B(N,:) = DU(N-1) + D(N)
       DO I = 1, NRHS; B(:,I) = B(:,I)*I; ENDDO
       WRITE(*,*)'B = '
       DO I=1,N; WRITE(*,"(8(F8.5,1X))") B(I,:)
       ENDDO

       WRITE(*,*) "CALL GTSVX(DL, D, DU, B, X, DLF, DF, DUF, DU2, TRANS='T' )"
       TRANS='T'
       CALL GTSVX(DL, D, DU, B, X, DLF, DF, DUF, DU2, IPIV, TRANS='T' )

       WRITE(*,*)'X = '
       DO I=1,N;WRITE(*,"(6(F8.5,1X))") X(I,:)
       ENDDO

       WRITE(*,*)'DLF on exit:'; WRITE(*,"(8(F8.5,1X))") DLF(:);
       WRITE(*,*)'DF on exit:';  WRITE(*,"(8(F8.5,1X))") DF(:);
       WRITE(*,*)'DUF on exit:'; WRITE(*,"(8(F8.5,1X))") DUF(:);
       WRITE(*,*)'DU2 on exit:'; WRITE(*,"(8(F8.5,1X))") DU2(:);
       WRITE(*,*)'IPIV on exit:';WRITE(*,"(8(I6,1X))") IPIV(:);

       DEALLOCATE(DL, DLF, D, DF, DU, DUF, DU2, B, X, IPIV)

       END PROGRAM SGTSVX_MAIN
