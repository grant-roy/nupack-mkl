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

PURE SUBROUTINE DGEEVX_MKL95(A,WR,WI,VL,VR,BALANC,ILO,IHI,SCALE,ABNRM,  &
     &                                               RCONDE,RCONDV,INFO)
    ! MKL Fortran77 call:
    ! DGEEVX(BALANC,JOBVL,JOBVR,SENSE,N,A,LDA,WR,WI,VL,LDVL,VR,LDVR,ILO,
    !   IHI,SCALE,ABNRM,RCONDE,RCONDV,WORK,LWORK,IWORK,INFO)
    ! <<< Use statements >>>
    USE MKL77_LAPACK, ONLY: MKL77_GEEVX, MKL77_XERBLA
    ! <<< Implicit statement >>>
    IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: BALANC
    INTEGER, INTENT(OUT), OPTIONAL :: ILO
    INTEGER, INTENT(OUT), OPTIONAL :: IHI
    REAL(WP), INTENT(OUT), OPTIONAL :: ABNRM
    INTEGER, INTENT(OUT), OPTIONAL :: INFO
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: A(:,:)
    REAL(WP), INTENT(OUT) :: WR(:)
    REAL(WP), INTENT(OUT) :: WI(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: VL(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: VR(:,:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: SCALE(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: RCONDE(:)
    REAL(WP), INTENT(OUT), OPTIONAL, TARGET :: RCONDV(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GEEVX'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_BALANC
    INTEGER :: O_ILO
    INTEGER :: O_IHI
    REAL(WP) :: O_ABNRM
    INTEGER :: O_INFO
    CHARACTER(LEN=1) :: JOBVL
    CHARACTER(LEN=1) :: JOBVR
    CHARACTER(LEN=1) :: SENSE
    INTEGER :: N
    INTEGER :: LDA
    INTEGER :: LDVL
    INTEGER :: LDVR
    INTEGER :: LWORK
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    ! <<< Local arrays >>>
    REAL(WP), POINTER :: O_VL(:,:)
    REAL(WP), POINTER :: O_VR(:,:)
    REAL(WP), POINTER :: O_SCALE(:)
    REAL(WP), POINTER :: O_RCONDE(:)
    REAL(WP), POINTER :: O_RCONDV(:)
    REAL(WP), POINTER :: WORK(:)
    INTEGER, POINTER :: IWORK(:)
    ! <<< Arrays to request optimal sizes >>>
    REAL(WP) :: S_WORK(1)
    ! <<< Stubs to "allocate" optional arrays >>>
    INTEGER, TARGET :: L_A1_INTE(1)
    REAL(WP), TARGET :: L_A1_REAL(1)
    REAL(WP), TARGET :: L_A2_REAL(1,1)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(BALANC)) THEN
        O_BALANC = BALANC
    ELSE
        O_BALANC = 'N'
    ENDIF
    IF(PRESENT(VL).OR.PRESENT(RCONDE)) THEN
        JOBVL = 'V'
    ELSE
        JOBVL = 'N'
    ENDIF
    IF(PRESENT(VR).OR.PRESENT(RCONDE)) THEN
        JOBVR = 'V'
    ELSE
        JOBVR = 'N'
    ENDIF
    LDA = MAX(1,SIZE(A,1))
    N = SIZE(A,2)
    IF(PRESENT(RCONDE).AND.PRESENT(RCONDV)) THEN
        SENSE = 'B'
    ELSEIF(PRESENT(RCONDE)) THEN
        SENSE = 'E'
    ELSEIF(PRESENT(RCONDV)) THEN
        SENSE = 'V'
    ELSE
        SENSE = 'N'
    ENDIF
    IF(PRESENT(VL)) THEN
        LDVL = MAX(1,SIZE(VL,1))
    ELSEIF(PRESENT(RCONDE)) THEN
        LDVL = N
    ELSE
        LDVL = 1
    ENDIF
    IF(PRESENT(VR)) THEN
        LDVR = MAX(1,SIZE(VR,1))
    ELSEIF(PRESENT(RCONDE)) THEN
        LDVR = N
    ELSE
        LDVR = 1
    ENDIF
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Allocate local and work arrays >>>
    IF(.NOT.PRESENT(RCONDE)) THEN
        IF(PRESENT(VL)) THEN
            O_VL => VL
        ELSE
            O_VL => L_A2_REAL
        ENDIF
    ELSE
        IF(PRESENT(VL)) THEN
            O_VL => VL
        ELSE
            ALLOCATE(O_VL(N,N), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(.NOT.PRESENT(RCONDE)) THEN
        IF(PRESENT(VR)) THEN
            O_VR => VR
        ELSE
            O_VR => L_A2_REAL
        ENDIF
    ELSE
        IF(L_STAT_ALLOC==0) THEN
            IF(PRESENT(VR)) THEN
                O_VR => VR
            ELSE
                ALLOCATE(O_VR(N,N), STAT=L_STAT_ALLOC)
            ENDIF
        ENDIF
    ENDIF
    IF(PRESENT(RCONDE)) THEN
        O_RCONDE => RCONDE
    ELSE
        O_RCONDE => L_A1_REAL
    ENDIF
    IF(PRESENT(RCONDV)) THEN
        O_RCONDV => RCONDV
    ELSE
        O_RCONDV => L_A1_REAL
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        IF(PRESENT(SCALE)) THEN
            O_SCALE => SCALE
        ELSE
            ALLOCATE(O_SCALE(N), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    IF(.NOT.PRESENT(RCONDV)) THEN
        IWORK => L_A1_INTE
    ELSE
        IF(L_STAT_ALLOC==0) THEN
            ALLOCATE(IWORK(2*N-2), STAT=L_STAT_ALLOC)
        ENDIF
    ENDIF
    ! <<< Request work array(s) size >>>
    LWORK = -1
    CALL MKL77_GEEVX(O_BALANC,JOBVL,JOBVR,SENSE,N,A,LDA,WR,WI,O_VL,LDVL,&
     &  O_VR,LDVR,O_ILO,O_IHI,O_SCALE,O_ABNRM,O_RCONDE,O_RCONDV,S_WORK, &
     &                                               LWORK,IWORK,O_INFO)
    ! <<< Exit if error: bad parameters >>>
    IF(O_INFO /= 0) THEN
        GOTO 200
    ENDIF
    LWORK = S_WORK(1)
    ! <<< Allocate work arrays with requested sizes >>>
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(WORK(LWORK), STAT=L_STAT_ALLOC)
    ENDIF
    ! <<< Call lapack77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL MKL77_GEEVX(O_BALANC,JOBVL,JOBVR,SENSE,N,A,LDA,WR,WI,O_VL, &
     &LDVL,O_VR,LDVR,O_ILO,O_IHI,O_SCALE,O_ABNRM,O_RCONDE,O_RCONDV,WORK,&
     &                                               LWORK,IWORK,O_INFO)
    ELSE; O_INFO = -1000
    ENDIF
    ! <<< Set output optional scalar parameters >>>
    IF(PRESENT(ABNRM)) THEN
        ABNRM = O_ABNRM
    ENDIF
    IF(PRESENT(IHI)) THEN
        IHI = O_IHI
    ENDIF
    IF(PRESENT(ILO)) THEN
        ILO = O_ILO
    ENDIF
    ! <<< Deallocate work arrays with requested sizes >>>
    DEALLOCATE(WORK, STAT=L_STAT_DEALLOC)
200    CONTINUE
    ! <<< Deallocate local and work arrays >>>
    IF(.NOT. PRESENT(SCALE)) THEN
        DEALLOCATE(O_SCALE, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(PRESENT(RCONDE) .AND..NOT. PRESENT(VL)) THEN
        DEALLOCATE(O_VL, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(PRESENT(RCONDE) .AND..NOT. PRESENT(VR)) THEN
        DEALLOCATE(O_VR, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(PRESENT(RCONDV)) THEN
        DEALLOCATE(IWORK, STAT=L_STAT_DEALLOC)
    ENDIF
    ! <<< Error handler >>>
    IF(PRESENT(INFO)) THEN
        INFO = O_INFO
    ELSEIF(O_INFO <= -1000) THEN
        CALL MKL77_XERBLA(SRNAME,-O_INFO)
    ENDIF
END SUBROUTINE DGEEVX_MKL95
