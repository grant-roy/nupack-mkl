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

PURE SUBROUTINE SGBBRD_MKL95(A,C,D,E,Q,PT,KL,M,INFO)
    ! MKL Fortran77 call:
    ! SGBBRD(VECT,M,N,NCC,KL,KU,A,LDA,D,E,Q,LDQ,PT,LDPT,C,LDC,WORK,INFO)
    ! <<< Use statements >>>
    USE MKL77_LAPACK, ONLY: MKL77_GBBRD, MKL77_XERBLA
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    INTEGER, INTENT(IN), OPTIONAL :: KL
    INTEGER, INTENT(IN), OPTIONAL :: M
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: A(:,:)
    REAL(WP), INTENT(INOUT), OPTIONAL, TARGET :: C(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: D(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: E(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: Q(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: PT(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GBBRD'
    ! <<< Local scalars >>>
    INTEGER :: O_KL
    INTEGER :: O_M
    INTEGER :: O_INFO
    CHARACTER(LEN=1) :: VECT
    INTEGER :: N
    INTEGER :: NCC
    INTEGER :: KU
    INTEGER :: LDA
    INTEGER :: LDQ
    INTEGER :: LDPT
    INTEGER :: LDC
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: O_C(:,:)
    REAL(WP), POINTER :: O_D(:)
    REAL(WP), POINTER :: O_E(:)
    REAL(WP), POINTER :: O_Q(:,:)
    REAL(WP), POINTER :: O_PT(:,:)
    REAL(WP), POINTER :: WORK(:)
    ! <<< Stubs to "allocate" optional arrays >>>
    REAL(WP), TARGET :: L_A2_REAL(1,1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    LDA = MAX(1,SIZE(A,1))
    IF(PRESENT(PT)) THEN
        LDPT = MAX(1,SIZE(PT,1))
    ELSE
        LDPT = 1
    ENDIF
    IF(PRESENT(Q)) THEN
        LDQ = MAX(1,SIZE(Q,1))
    ELSE
        LDQ = 1
    ENDIF
    N = SIZE(A,2)
    IF(PRESENT(C)) THEN
        NCC = SIZE(C,2)
    ELSE
        NCC = 0
    ENDIF
    IF(PRESENT(Q).AND.PRESENT(PT)) THEN
        VECT = 'B'
    ELSEIF(PRESENT(Q)) THEN
        VECT = 'Q'
    ELSEIF(PRESENT(PT)) THEN
        VECT = 'P'
    ELSE
        VECT = 'N'
    ENDIF
    IF(PRESENT(KL)) THEN
        O_KL = KL
    ELSE
        O_KL = (LDA-1)/2
    ENDIF
    IF(PRESENT(M)) THEN
        O_M = M
    ELSE
        O_M = N
    ENDIF
    KU = LDA-O_KL-1
    LDC = MAX(1,O_M)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(PRESENT(C)) THEN
        O_C => C
    ELSE
        O_C => L_A2_REAL
    ENDIF
    IF(PRESENT(D)) THEN
        O_D => D
    ELSE
        ALLOCATE(O_D(MIN(O_M,N)), STAT=L_STAT_ALLOC)
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(E)) THEN
            O_E => E
        ELSE
            ALLOCATE(O_E(MIN(O_M,N)-1), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(PRESENT(PT)) THEN
        O_PT => PT
    ELSE
        O_PT => L_A2_REAL
    ENDIF
    IF(PRESENT(Q)) THEN
        O_Q => Q
    ELSE
        O_Q => L_A2_REAL
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(2*MAX(O_M,N)), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL MKL77_GBBRD(VECT,O_M,N,NCC,O_KL,KU,A,LDA,O_D,O_E,O_Q,LDQ,  &
     &                                    O_PT,LDPT,O_C,LDC,WORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Deallocate local and work arrays >>>
    IF(.NOT. PRESENT(D)) THEN
        DEALLOCATE(O_D, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(.NOT. PRESENT(E)) THEN
        DEALLOCATE(O_E, STAT=L_STAT_DEALLOC)
    ENDIF
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL MKL77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE SGBBRD_MKL95
