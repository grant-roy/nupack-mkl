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
!      Intel(R) Math Kernel Library (MKL) F95 interface for BLAS routines
!*******************************************************************************
! This file was generated automatically!
!*******************************************************************************

PURE SUBROUTINE SROTI_MKL95(X,INDX,Y,C,S)
    ! MKL Fortran77 call:
    ! SROTI(NZ,X,INDX,Y,C,S)
    ! <<< Use statements >>>
    USE MKL77_BLAS, ONLY: MKL77_ROTI
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(IN), OPTIONAL :: C
    REAL(WP), INTENT(IN), OPTIONAL :: S
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: X(:)
    INTEGER, INTENT(IN) :: INDX(:)
    REAL(WP), INTENT(IN) :: Y(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=4), PARAMETER :: SRNAME = 'ROTI'
    ! <<< Local scalars >>>
    REAL(WP) :: O_C
    REAL(WP) :: O_S
    INTEGER :: NZ
    ! <<< Intrinsic functions >>>
    INTRINSIC PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(C)) THEN
        O_C = C
    ELSE
        O_C = 1
    ENDIF
    IF(PRESENT(S)) THEN
        O_S = S
    ELSE
        O_S = 1
    ENDIF
    NZ = SIZE(X)
    ! <<< Call blas77 routine >>>
    CALL MKL77_ROTI(NZ,X,INDX,Y,O_C,O_S)
END SUBROUTINE SROTI_MKL95
