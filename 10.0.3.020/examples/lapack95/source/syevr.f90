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
!     S S Y E V R  Example Program Text
!*******************************************************************************

      PROGRAM SSYEVR_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: SYEVR
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, M, N, IL, IU
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE ::  ISUPPZ(:)
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), W(:), Z(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SYEVR Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), W(N), Z(N,N), ISUPPZ(2*N)  )

      A = 0
!      OPEN(UNIT=21,FILE='data/syev_a.dat',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N
!            READ(21,*) A(I,J)
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      AA=A

      WRITE(*,*)'Matrix A:'
      DO I=1,N;
         WRITE(*,"(5(F9.5))") A(I,:);
      ENDDO

      WRITE(*,*) 'CALL SYEVR( A, W, Z=Z, ISUPPZ=ISUPPZ, IL=1, IU=2, M=M )'
      IL=1; IU=2

      CALL SYEVR( A, W, IL=1, IU=2, M=M )

      WRITE(*,*)'Matrix A on exit :'
      DO I=1,N;
         WRITE(*,"(5(F9.5))") A(I,:);
      ENDDO


      WRITE(*,*)'M = ', M
      WRITE(*,*) 'W on exit :'
      DO I=1,M
         WRITE(*,"(5(F9.5))") W(I)
      ENDDO

      WRITE(*,*)'Z on exit :'
      DO I=1,N
         WRITE(*,"(5(E14.6))") Z(I,1:M)
      ENDDO
      WRITE(*,*)'ISUPPZ on exit:'
      WRITE(*,*) ISUPPZ(1:2*max(1,m))

      DEALLOCATE(A, AA, W, Z, ISUPPZ)

      END PROGRAM SSYEVR_MAIN
