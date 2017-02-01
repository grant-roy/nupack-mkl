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

PURE SUBROUTINE CTRSYL_MKL95(A,B,C,SCALE,TRANA,TRANB,ISGN,INFO)
    ! MKL Fortran77 call:
    ! CTRSYL(TRANA,TRANB,ISGN,M,N,A,LDA,B,LDB,C,LDC,SCALE,INFO)
    ! <<< Use statements >>>
    USE MKL77_LAPACK, ONLY: MKL77_TRSYL, MKL77_XERBLA
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(OUT) :: SCALE
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANA
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANB
    INTEGER, INTENT(IN), OPTIONAL :: ISGN
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(IN) :: A(:,:)
    COMPLEX(WP), INTENT(IN) :: B(:,:)
    COMPLEX(WP), INTENT(INOUT) :: C(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'TRSYL'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_TRANA
    CHARACTER(LEN=1) :: O_TRANB
    INTEGER :: O_ISGN
    INTEGER :: O_INFO
    INTEGER :: M
    INTEGER :: N
    INTEGER :: LDA
    INTEGER :: LDB
    INTEGER :: LDC
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(ISGN)) THEN
        O_ISGN = ISGN
    ELSE
        O_ISGN = 1
    ENDIF
    IF(PRESENT(TRANA)) THEN
        O_TRANA = TRANA
    ELSE
        O_TRANA = 'N'
    ENDIF
    IF(PRESENT(TRANB)) THEN
        O_TRANB = TRANB
    ELSE
        O_TRANB = 'N'
    ENDIF
    LDA = MAX(1,SIZE(A,1))
    LDB = MAX(1,SIZE(B,1))
    LDC = MAX(1,SIZE(C,1))
    M = SIZE(A,2)
    N = SIZE(B,2)
    ! <<< Call lapack77 routine >>>
    CALL MKL77_TRSYL(O_TRANA,O_TRANB,O_ISGN,M,N,A,LDA,B,LDB,C,LDC,SCALE,&
     &                                                           O_INFO)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL MKL77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE CTRSYL_MKL95
