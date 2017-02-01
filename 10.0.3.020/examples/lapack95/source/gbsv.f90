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
!      Example shows how to migrate from NETLIB LAPACK95 to
!      Intel(R) Math Kernel Library (MKL) LAPACK95
!*******************************************************************************
      PROGRAM LA_SGBSV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999

!  .. "Use Statements"
      USE MKL95_PRECISION, ONLY: WP => SP
      USE MKL95_LAPACK, ONLY: GBSV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: K, KL, KU, I, J, N, NRHS, INFO
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      REAL(WP), ALLOCATABLE :: AB(:,:), B(:,:)
!  .. "Executable Statements" ..
       WRITE (*,*) 'SGBSV Example Program Results.'
       N = 6; KL = 2; KU = 1; NRHS = 2
       ALLOCATE ( AB(2*KL+KU+1,N), B(N,NRHS), IPIV(N))
       AB=0.0; B=0.0; IPIV=0

!       OPEN(UNIT=21,FILE='gbsv.ma',STATUS='UNKNOWN')
       DO I=KL+1,2*KL+KU+1
       DO J=1,N
          READ(*,'(F2.0)') AB(I,J)
       ENDDO
       ENDDO
!       CLOSE(21)

       WRITE(*,*) 'The matrix AB:'
       DO I=1,N; WRITE(*,"(5(I3,1X,1X),I3,1X)") INT(AB(I,:));
       ENDDO

       DO I = 1, NRHS
       DO J = 1, N
       DO K = MAX(1,J-KL),MIN(J+KU,N); B(J,I) = AB(KL+KU+1+J-K,K)+B(J,I);
       ENDDO
       ENDDO;
       B(:,I) = B(:,I)*I;
       ENDDO

       WRITE(*,*) 'The RHS matrix B:'
       DO I=1,N; WRITE(*,"(1(I3,1X),I3,1X)") INT(B(I,:)); ENDDO
       WRITE(*,*) 'CALL LA_GBSV( AB, B, 2, IPIV, INFO)'

       CALL GBSV( AB, B, 2, IPIV, INFO )

       WRITE(*,*)'AB on exit: '
       DO I=1,2*KL+KU+1; WRITE(*,"(5(E12.6,1X),E12.6,1X)") AB(I,:); ENDDO
       WRITE(*,*)'B on exit: '
       DO I=1,N; WRITE(*,"(1(E12.6,1X),E12.6,1X)") B(I,:); ENDDO
       WRITE(*,*)'IPIV on exit: ', IPIV
       WRITE(*,*)'INFO on exit: ', INFO

       END PROGRAM LA_SGBSV_EXAMPLE





