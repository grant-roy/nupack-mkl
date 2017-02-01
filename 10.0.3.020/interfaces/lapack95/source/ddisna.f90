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
!      Intel(R) Math Kernel Library (MKL) F95 interface for LAPACK routines
!*******************************************************************************
! This file was generated automatically!
!*******************************************************************************

PURE SUBROUTINE DDISNA_MKL95(D,SEP,JOB,MINMN,INFO)
    ! MKL Fortran77 call:
    ! DDISNA(JOB,M,N,D,SEP,INFO)
    ! <<< Use statements >>>
    USE MKL77_LAPACK, ONLY: MKL77_DISNA, MKL77_XERBLA
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOB
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: MINMN
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(IN) :: D(:)
    REAL(WP), INTENT(OUT) :: SEP(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'DISNA'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_JOB
    CHARACTER(LEN=1) :: O_MINMN
    INTEGER :: O_INFO
    INTEGER :: M
    INTEGER :: N
    ! <<< Intrinsic functions >>>
    INTRINSIC PRESENT
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(JOB)) THEN
        O_JOB = JOB
    ELSE
        O_JOB = 'E'
    ENDIF
    IF(PRESENT(MINMN)) THEN
        O_MINMN = MINMN
    ELSE
        O_MINMN = 'M'
    ENDIF
    IF((O_MINMN.EQ.'M'.OR.O_MINMN.EQ.'m')) THEN
        M = SIZE(D)
    ELSE
        M = SIZE(D)+1
    ENDIF
    IF((O_MINMN.EQ.'M'.OR.O_MINMN.EQ.'m')) THEN
        N = SIZE(D)+1
    ELSE
        N = SIZE(D)
    ENDIF
    ! <<< Call lapack77 routine >>>
    CALL MKL77_DISNA(O_JOB,M,N,D,SEP,O_INFO)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL MKL77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE DDISNA_MKL95
