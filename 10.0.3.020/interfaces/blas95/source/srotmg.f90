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

PURE SUBROUTINE SROTMG_MKL95(X1,Y1,PARAM,D1,D2)
    ! MKL Fortran77 call:
    ! SROTMG(D1,D2,X1,Y1,PARAM)
    ! <<< Use statements >>>
    USE MKL77_BLAS, ONLY: MKL77_ROTMG
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(INOUT) :: X1
    REAL(WP), INTENT(IN) :: Y1
    REAL(WP), INTENT(INOUT), OPTIONAL :: D1
    REAL(WP), INTENT(INOUT), OPTIONAL :: D2
    ! <<< Array arguments >>>
    REAL(WP), INTENT(OUT) :: PARAM(5)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'ROTMG'
    ! <<< Local scalars >>>
    REAL(WP) :: O_D1
    REAL(WP) :: O_D2
    ! <<< Intrinsic functions >>>
    INTRINSIC PRESENT
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(D1)) THEN
        O_D1 = D1
    ELSE
        O_D1 = 1
    ENDIF
    IF(PRESENT(D2)) THEN
        O_D2 = D2
    ELSE
        O_D2 = 1
    ENDIF
    ! <<< Call blas77 routine >>>
    CALL MKL77_ROTMG(O_D1,O_D2,X1,Y1,PARAM)
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(D1)) THEN
        D1 = O_D1
    ENDIF
    IF(PRESENT(D2)) THEN
        D2 = O_D2
    ENDIF
END SUBROUTINE SROTMG_MKL95
