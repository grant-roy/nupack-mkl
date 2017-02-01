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
!     C G G E V X  Example Program Text
!*******************************************************************************

      PROGRAM CGGEVX_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: GGEVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N
      REAL :: ABNRM, BBNRM
!  .. "Local Arrays" ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
      CHARACTER(LEN=1) :: BALANC
      COMPLEX(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:), ALPHA(:)
      COMPLEX(WP), ALLOCATABLE :: BETA(:), LAMBDA(:)
      REAL(WP), ALLOCATABLE :: LSCALE(:), RSCALE(:), RCONDE(:), RCONDV(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'GGEVX Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), B(N,N), BB(N,N), ALPHA(N), BETA(N), LSCALE(N), &
&       RSCALE(N), RCONDE(N), RCONDV(N), LAMBDA(N) )

      A = 0
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

      B = 0
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
      WRITE(*,*) "CALL GGEVX( A, B, BALANC='B', LSCALE=LSCALE, RSCALE=RSCALE,"
      WRITE(*,*)"ABNRM=ABNRM, BBNRM=BBNRM, RCONDE=RCONDE, RCONDV=RCONDV )"
      BALANC = 'B'
      CALL GGEVX( A, B, ALPHA, BETA, BALANC=BALANC, LSCALE=LSCALE, RSCALE=RSCALE, ABNRM=ABNRM, &
           BBNRM=BBNRM, RCONDE=RCONDE, RCONDV=RCONDV )

      WRITE(*,*)
      WRITE(*,*)'LSCALE : '
      DO I=1,N
         WRITE(*,'(F9.5)') LSCALE(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*)'RSCALE : '
      DO I=1,N
         WRITE(*,'(F9.5)') RSCALE(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*)'ABNRM = ', ABNRM
      WRITE(*,*)
      WRITE(*,*)'BBNRM = ', BBNRM
      WRITE(*,*)
      WRITE(*,*)'RCONDE : '
      DO I=1,N
         WRITE(*,'(F9.5)') RCONDE(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*)'RCONDV : '
      DO I=1,N
         WRITE(*,'(F9.5)') RCONDV(I)
      ENDDO

      WRITE(*,*)'Matrix ALPHA : '
      WRITE(*,FMT) ALPHA

      WRITE(*,*)'Matrix BETA : '
      WRITE(*,FMT) BETA
      lambda = alpha/beta
      print *,'lambda = ', LAMBDA
      WRITE(*,FMT) LAMBDA

      DEALLOCATE(A, AA, ALPHA, BETA, LSCALE)
      DEALLOCATE(RSCALE, RCONDE, RCONDV, LAMBDA)

      END PROGRAM CGGEVX_MAIN
