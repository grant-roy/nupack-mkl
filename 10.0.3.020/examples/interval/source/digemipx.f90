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

PROGRAM DIGEMIP_EXAMPLE
!
!       An example code for preconditioning of a square interval
!       linear system of equations by midpoint inverse matrix
!
!==============================================================================!

    USE INTERVAL_ARITHMETIC
    IMPLICIT NONE

    !--------------------------------------------------------------------------!

    INTEGER, PARAMETER            ::  INPUT = 5, OUTPUT = 6

    CHARACTER(1), PARAMETER       ::  TRANS = 'N'
    INTEGER                       ::  N, LDA, LDB, NRHS, INFO, I, J
    TYPE(D_INTERVAL), ALLOCATABLE ::  A(:,:), B(:,:)

    !--------------------------------------------------------------------------!

    WRITE ( OUTPUT, 100 )

!------------------------------------------------------------------------------!
!
!       Reading the input data -
!

    READ ( INPUT, * )
    READ ( INPUT, * ) N, NRHS

        LDA = N
        LDB = N

        ALLOCATE( A(N,N), B(N,NRHS) )

        READ ( INPUT, * ) (( A(I,J), J = 1, N ), I = 1, N )
        READ ( INPUT, * ) (( B(I,J), J = 1, NRHS ), I = 1, N )

    !--------------------------------------------------------------------------!
    !
    !   Displaying the matrix of the system and right-hand side vector -
    !

    WRITE ( OUTPUT, 200 )
    DO I = 1, N
        WRITE ( OUTPUT, 700 ) ( A(I,J), J = 1, N )
    END DO

    WRITE ( OUTPUT, 300 )
    DO I = 1, N
        WRITE ( OUTPUT, 700 ) ( B(I,J), J = 1, NRHS )
    END DO

    !--------------------------------------------------------------------------!

        CALL DIGEMIP( N, NRHS, A, LDA, B, LDB, INFO )

    !--------------------------------------------------------------------------!
    !
    !   Outputting the results -
    !

    IF( INFO /= 0 ) THEN
        WRITE ( OUTPUT, 400 )
    ELSE
        WRITE ( OUTPUT, 500 )
        DO I = 1, N
            WRITE ( OUTPUT, 700 ) ( A(I,J), J = 1, N )
        END DO
        WRITE ( OUTPUT, 600 )
        DO I = 1, N
            WRITE ( OUTPUT, 700 ) ( B(I,J), J = 1, NRHS )
        END DO
    END IF

    !--------------------------------------------------------------------------!

    DEALLOCATE( A, B )

    !--------------------------------------------------------------------------!

  100  FORMAT (/,'  **** PRECONDITIONING INTERVAL LINEAR SYSTEM ****',/,         &
                 '             by midpoint inverse matrix           ',/)
  200  FORMAT (/,' The matrix of the system:',/)
  300  FORMAT (/,' The right-hand side matrix:',/)
  400  FORMAT (/,' The inversion of the midpoint matrix failed.',/)
  500  FORMAT (/,' The matrix of the preconditioned system:',/)
  600  FORMAT (/,' The right-hand side of the preconditioned system:',/)
  700  FORMAT ( 2('[', F10.6, ',', F10.6, ']  ') )

    !--------------------------------------------------------------------------!

END PROGRAM DIGEMIP_EXAMPLE

!==============================================================================!
