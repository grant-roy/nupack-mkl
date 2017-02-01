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
!     S S T E V  Example Program Text
!*******************************************************************************

      PROGRAM SSTEV_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: STEV, STEVD
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: D(:), E(:), Z(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'STEV Example Program Results'
      N = 5;
      ALLOCATE( D(N), E(N), Z(N,N) )

      D = 0
!      OPEN(UNIT=21,FILE='data/stevd_a.dat',STATUS='UNKNOWN')
      DO J=1,N
!         READ(21,*) D(J)
         READ(*,*) D(J)
      ENDDO
!      CLOSE(21)

      E = 0
!      OPEN(UNIT=21,FILE='data/steve_a.dat',STATUS='UNKNOWN')
      DO J=1,N
!         READ(21,*) E(J)
         READ(*,*) E(J)
      ENDDO
!      CLOSE(21)

      WRITE(*,*)' Vector D : '
      DO I=1,N
      WRITE(*,"(5(I5,1X))") INT(D(I))
      ENDDO

      WRITE(*,*)' Vector E : '
      DO I=1,N
      WRITE(*,"(5(I5,1X))") INT(E(I))
      ENDDO

      WRITE(*,*) 'CALL STEV( D, E )'
      CALL STEV( D, E )

      WRITE(*,*) ' D on exit : '
      DO I=1,N
      WRITE(*,"(5(F12.5,1X))") D(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*) ' * EXAMPLE 2 * '

      D = 0
!      OPEN(UNIT=21,FILE='data/stevd_a.dat',STATUS='UNKNOWN')
      DO J=1,N
!         READ(21,*) D(J)
         READ(*,*) D(J)
      ENDDO
!      CLOSE(21)

      E = 0
!      OPEN(UNIT=21,FILE='data/steve_a.dat',STATUS='UNKNOWN')
      DO J=1,N
!         READ(21,*) E(J)
         READ(*,*) E(J)
      ENDDO
!      CLOSE(21)

      WRITE(*,*)' Vector D : '
      DO I=1,N
      WRITE(*,"(5(I5,1X))") INT(D(I))
      ENDDO

      WRITE(*,*)' Vector E : '
      DO I=1,N
      WRITE(*,"(5(I5,1X))") INT(E(I))
      ENDDO

      WRITE(*,*) "CALL STEVD( D, E, Z, INFO )"
      CALL STEVD( D, E, Z, INFO )

      WRITE(*,*)' Z on exit : '
      DO I=1,N;
         WRITE(*,"(5(F12.6,1X))") Z(I,:);
      ENDDO

      WRITE(*,*)' D on exit : '
      DO I=1,N
      WRITE(*,"(5(F12.5,1X))") D(I)
      ENDDO

      WRITE(*,*) ' INFO = ', INFO

      DEALLOCATE(D, E, Z)

      END PROGRAM SSTEV_MAIN
