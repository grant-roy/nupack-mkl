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

PROGRAM SITRTRI_EXAMPLE
!
!       An example code for computing an enclosure of the inverse
!       to a square interval matrix by interval Schulz iteration
!
!==============================================================================!

    USE INTERVAL_ARITHMETIC
    IMPLICIT NONE

    !--------------------------------------------------------------------------!

    INTEGER, PARAMETER            ::  INPUT = 5, OUTPUT = 6

    CHARACTER(1), PARAMETER       ::  UPLO = 'u', DIAG = 'N'
    INTEGER                       ::  N, LDA, INFO, I, J
    TYPE(S_INTERVAL), ALLOCATABLE ::  A(:,:)

    !--------------------------------------------------------------------------!

    WRITE ( OUTPUT, 100 )

!------------------------------------------------------------------------------!
!
!       Reading the input data -
!

    READ ( INPUT, * )
    READ ( INPUT, * ) N

        LDA = N

        ALLOCATE( A(N,N) )

        READ ( INPUT, * ) (( A(I,J), J = 1, N ), I = 1, N )

    !--------------------------------------------------------------------------!
    !
    !   Displaying the matrix of the system and right-hand side vector -
    !

    WRITE ( OUTPUT, 200 )
    DO I = 1, N
        WRITE ( OUTPUT, 300 ) ( A(I,J), J = 1, N )
    END DO

    !--------------------------------------------------------------------------!

        CALL SITRTRI( UPLO, DIAG, N, A, LDA, INFO )

    !--------------------------------------------------------------------------!
    !
    !   Outputting the results -
    !

    IF( INFO /= 0 ) THEN
        WRITE ( OUTPUT, 400 )
    ELSE
        WRITE ( OUTPUT, 600 )
        DO I = 1, N
            WRITE ( OUTPUT, 300 ) ( A(I,J), J = 1, N )
        END DO
    END IF

    !--------------------------------------------------------------------------!

    DEALLOCATE( A )

    !--------------------------------------------------------------------------!

  100  FORMAT (/,'  **** INVERTING INTERVAL TRIANGULAR MATRIX ****')
  200  FORMAT (/,' The matrix to be inverted:',/)
  300  FORMAT ( 2('[',F10.5,',',F10.5,']',2X) )
  400  FORMAT (/,' The procedure failed to invert the matrix.',/)
  600  FORMAT (/,' Enclosure of the inverse interval matrix:',/)

    !--------------------------------------------------------------------------!

END PROGRAM SITRTRI_EXAMPLE

!==============================================================================!
