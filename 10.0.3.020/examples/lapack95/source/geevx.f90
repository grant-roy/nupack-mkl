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
!     S G E E V X  Example Program Text
!*******************************************************************************

      PROGRAM SGEEVX_MAIN

!  .. "Use Statements" ..
      USE mkl95_PRECISION, ONLY: WP => SP
      USE mkl95_LAPACK, ONLY: GEEVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, ILO, IHI
      REAL(WP) :: ABNRM
      REAL(WP), ALLOCATABLE :: WR(:), WI(:)
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), VL(:,:), VR(:,:), SCALE(:)
      REAL(WP), ALLOCATABLE :: RCONDE(:), RCONDV(:)
!  .. "Intrinsic Functions" ..
      INTRINSIC REAL, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'GEEVX Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), WR(N), WI(N), VL(N,N), VR(N,N), SCALE(N) )
      ALLOCATE( RCONDE(N), RCONDV(N) )

      A = 0
!      OPEN(UNIT=21,FILE='data/geev1.dat',STATUS='UNKNOWN')
      DO I=1,N
         DO J=1,N
!            READ(21,*) A(I,J)
            READ(*,*) A(I,J)
         ENDDO
      ENDDO
!      CLOSE(21)

      AA=A

      WRITE(*,*)'Matrix A:'
      DO I=1,N
         WRITE(*,"(5(I3,1X))") INT(A(I,:))
      ENDDO

      WRITE(*,*) "CALL GEEVX( A, WR, WI, 'B', ILO, IHI, SCALE,", &
                               " ABNRM, RCONDE, RCONDV )"
      CALL GEEVX( A, WR, WI, BALANC='B', ILO=ILO, IHI=IHI, &
           SCALE=SCALE, ABNRM=ABNRM, RCONDE=RCONDE, RCONDV=RCONDV )

      WRITE(*,*) "WR on exit : "
      DO I=1,N
         WRITE(*,"((E14.6,1X))") REAL(WR(I))
      ENDDO

      WRITE(*,*) "WI on exit : "
      DO I=1,N
         WRITE(*,"((E14.6,1X))") REAL(WI(I))
      ENDDO

      WRITE(*,*)'ILO : ',ILO
      WRITE(*,*)'IHI : ',IHI
      WRITE(*,*)'SCALE : ',SCALE
      WRITE(*,*)'ABNRM on exit :', ABNRM
      WRITE(*,*)'RCONDE on exit :', RCONDE
      WRITE(*,*)'RCONDV on exit :', RCONDV

      DEALLOCATE(A, AA, WR, WI, VL, VR, SCALE, RCONDE, RCONDV)

      END PROGRAM SGEEVX_MAIN
