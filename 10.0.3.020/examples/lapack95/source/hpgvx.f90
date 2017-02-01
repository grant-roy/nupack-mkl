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
!     C H P G V X  Example Program Text
!*******************************************************************************

      PROGRAM CHPGVX_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: HPGVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, ITYPE, IL, IU, M, INFO
      REAL(WP), ALLOCATABLE :: W(:)
      REAL(WP) :: ABSTOL
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IFAIL(:)
      COMPLEX(WP), ALLOCATABLE :: A(:), AA(:), B(:), BB(:), Z(:,:)
!  .. "Intrinsic Functions" ..
      INTRINSIC AIMAG, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'HPGVX Example Program Results'
      N = 5
      ALLOCATE( A(N*(N+1)/2), AA(N*(N+1)/2), B(N*(N+1)/2), BB(N*(N+1)/2), &
                W(N), Z(N,N), IFAIL(N) )

      A = 0
!      OPEN(UNIT=21,FILE='data/hpgvu_a.dat',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
!         READ(21,*) A(J)
         READ(*,*) A(J)
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix A:'
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(I3,1X,',',I3)')')") INT(A(I)), INT(AIMAG(A(I)))
      ENDDO


      B = 0
!      OPEN(UNIT=21,FILE='data/hpgvu_b.dat',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
!         READ(21,*) B(J)
         READ(*,*) B(J)
      ENDDO
!      CLOSE(21)

      WRITE(*,*)'Matrix B:'
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(I3,1X,',',I3)')')") INT(B(I)), INT(AIMAG(B(I)))
      ENDDO

      WRITE(*,*) "CALL HPGVX( A, B, W, 2, Z=Z, IL=4, IU=5, M=M, ", &
                 "IFAIL=IFAIL, ABSTOL=1.0E-3_WP, INFO =INFO ) "
      ITYPE=2; IL=4; IU=5; ABSTOL=1e-3
      CALL HPGVX( A, B, W, 2, Z=Z, IL=4, IU=5, M=M, &
                     IFAIL=IFAIL, ABSTOL=1.0E-3_WP, INFO = INFO )

      WRITE(*,*) 'W on exit:'
      DO I=1,N
         WRITE(*,"(5(E14.6,1X))") W(I)
      ENDDO

      WRITE(*,*) 'IFAIL on exit : ',IFAIL

      WRITE(*,*) 'M and INFO on exit:', M, INFO
      WRITE(*,*) 'Z on exit : '
      DO I=1,N
      WRITE(*,"(5(1H(,2(F12.10,1X),1H),1X))") (Z(I,J), J = 1,M)
      ENDDO

      DEALLOCATE(A, AA, B, BB, W, Z, IFAIL)

      END PROGRAM CHPGVX_MAIN
