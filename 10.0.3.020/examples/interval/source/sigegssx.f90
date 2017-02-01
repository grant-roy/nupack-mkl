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
!   Content:
!
!*******************************************************************************

PROGRAM SIGEGSS_EXAMPLE
!
!       An example code for computing an outer interval estimate (enclosure)
!       of the solution set to a square interval linear system of equations
!       by interval Gauss-Seidel iterative method
!
!==============================================================================!

    USE INTERVAL_ARITHMETIC
    IMPLICIT NONE

    !--------------------------------------------------------------------------!

    INTEGER, PARAMETER      ::  INPUT = 5, OUTPUT = 6
    INTEGER, PARAMETER      ::  NMAX = 2, LDA = NMAX, NRHMAX = NMAX, LDB = NMAX

    CHARACTER(1), PARAMETER ::  TRANS = 'N'
    INTEGER                 ::  N, NRHS, NITS, INFO,  I, J
    TYPE(S_INTERVAL)        ::  A(LDA,NMAX), B(LDB,NRHMAX), ENCL(LDB,NRHMAX)
    REAL(4), PARAMETER      ::  EPSILON = 1.E-3

    !--------------------------------------------------------------------------!

    WRITE ( OUTPUT, 100 )

!------------------------------------------------------------------------------!
!
!       Reading the input data -
!

    READ ( INPUT, * )
    READ ( INPUT, * ) N, NRHS, NITS

        READ ( INPUT, * ) (( A(I,J), J = 1, N ), I = 1, N )
        READ ( INPUT, * ) (( B(I,J), J = 1, NRHS ), I = 1, N )

    !--------------------------------------------------------------------------!
    !
    !   Displaying the matrix of the system and right-hand side vector -
    !

    WRITE ( OUTPUT, 200 )
    DO I = 1, N
        WRITE ( OUTPUT, * ) ('[', A(I,J), ']', J = 1, N )
    END DO

    WRITE ( OUTPUT, 300 )
    DO I = 1, N
        WRITE ( OUTPUT, * ) ('[', B(I,J), ']', J = 1, NRHS )
    END DO

    !--------------------------------------------------------------------------!
    !
    !   Assigning the starting enclosures -
    !

    DO I = 1, N
        DO J = 1, NRHS
            ENCL(I,J) = SINTERVAL(-20.,20.)
        END DO
    END DO

    !--------------------------------------------------------------------------!


    CALL SIGEGSS( TRANS, N, NRHS, A, LDA, B, LDB, ENCL, EPSILON, NITS, INFO )


    !--------------------------------------------------------------------------!

    IF( INFO /= 0 ) THEN
        WRITE ( OUTPUT, 400 )
    ELSE
        DO J = 1, NRHS
            WRITE ( OUTPUT, 500 ) J
            DO I = 1, N
                WRITE ( OUTPUT, 600 ) I, ') ', '[', B(I,J), ']'
            END DO
        END DO
    END IF

    !--------------------------------------------------------------------------!

  100  FORMAT (/,'  **** SOLVING  INTERVAL  LINEAR  SYSTEM ****',/,     &
                '         by interval Gauss-Seidel method',/)
  200  FORMAT (/,' The matrix of the system:',/)
  300  FORMAT (/,' The right-hand side matrix:',/)
  400  FORMAT (/,'    Sorry, the interval Gauss-Seidel method fails!  ',/)
  500  FORMAT (/,'   Enclosure of the solution column',I3,':',/)
  600  FORMAT ( I3, A2, A, F8.4, ',', F8.4, A )

    !--------------------------------------------------------------------------!

END PROGRAM SIGEGSS_EXAMPLE


!==============================================================================!
