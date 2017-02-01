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

PROGRAM DIGEPPS_EXAMPLE
!
!       An example code for computing optimal outer componentwise estimates
!       (i.e., the tightest ones) of the solution set to a square interval
!       linear system of equations by parameter partitioning method
!
!==============================================================================!

    USE INTERVAL_ARITHMETIC
    IMPLICIT NONE

    !--------------------------------------------------------------------------!

    INTEGER, PARAMETER            ::  INPUT = 5, OUTPUT = 6

    CHARACTER(1), PARAMETER       ::  TRANS = 'N'
    INTEGER                       ::  N, LDA, LDB, CMPT, NITS, INFO
    TYPE(D_INTERVAL), ALLOCATABLE ::  A(:,:), B(:)
    REAL(8)                       ::  ESTM, EPS
    CHARACTER(1)                  ::  MODE

    INTEGER                       ::  I, J, Iters

    !--------------------------------------------------------------------------!

    WRITE ( OUTPUT, 100 )

!------------------------------------------------------------------------------!
!
!       Reading and assigning the input data -
!

    READ ( INPUT, * )
    READ ( INPUT, * ) N

        LDA = N
        LDB = N

        ALLOCATE( A(N,N), B(N) )

        READ ( INPUT, * ) (( A(I,J), J = 1, N ), I = 1, N )
        READ ( INPUT, * ) ( B(I), I = 1, N )

    CMPT = 1
    MODE = 'l'
    NITS = 100
    EPS = 1.D-4

    Iters = NITS

    !--------------------------------------------------------------------------!
    !
    !   Displaying the matrix of the system and right-hand side vector -
    !

    WRITE ( OUTPUT, 200 )
    DO I = 1, N
        WRITE ( OUTPUT, 350 ) ('[', A(I,J), ']', J = 1, N )
    END DO

    WRITE ( OUTPUT, 300 )
    WRITE ( OUTPUT, 350 ) ('[', B(I), ']', I = 1, N )

    !--------------------------------------------------------------------------!

        CALL DIGEPPS( TRANS,N,A,LDA,B,LDB,CMPT,MODE,ESTM,EPS,NITS,INFO )

    !--------------------------------------------------------------------------!
    !
    !   Outputting the results -
    !

    IF( INFO > 0 ) THEN
        PRINT 400
        IF( INFO == 1 ) THEN
            PRINT 500
        END IF
    ELSE
        IF( NITS == Iters ) PRINT 600
        PRINT 700, MODE, CMPT, ESTM
        PRINT 800, NITS
        PRINT 900, EPS
    END IF

    !--------------------------------------------------------------------------!

    DEALLOCATE( A, B )

    !--------------------------------------------------------------------------!

  100  FORMAT (/,'  **** SOLVING  INTERVAL  LINEAR  SYSTEMS ****',/,      &
                 '        by parameter partitioning method      ')
  200  FORMAT (/,' The matrix of the system:',/)
  300  FORMAT (/,' The right-hand side matrix:',/)
  350  FORMAT (2(A,F12.6,',',F12.6,A))
  400  FORMAT (/,' The method fails to compute the enclosure.',/)
  500  FORMAT (/,' The interval matrix of the system ',                   &
                             'is likely not to be strongly regular.',/)
  600  FORMAT (/,' Limit on the number of iterations has been reached.')
  700  FORMAT (/,'  The ',A1,'-estimate of the component number ',I3,/,   &
                 '            of the solution set =',E15.7)
  800  FORMAT (/,'  The total number of iterations =',I7)
  900  FORMAT (  '  Actual accuracy =',E12.5 )

    !--------------------------------------------------------------------------!

END PROGRAM DIGEPPS_EXAMPLE

!==============================================================================!
