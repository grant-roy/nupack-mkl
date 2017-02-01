!==============================================================================!
!
!                              INTEL CONFIDENTIAL
!
!  Copyright(C) 2005-2008 Intel Corporation. All Rights Reserved.
!  The source code contained  or  described herein and all documents related to
!  the source code ("Material") are owned by Intel Corporation or its suppliers
!  or licensors.  Title to the  Material remains with  Intel Corporation or its
!  suppliers and licensors. The Material contains trade secrets and proprietary
!  and  confidential  information of  Intel or its suppliers and licensors. The
!  Material  is  protected  by  worldwide  copyright  and trade secret laws and
!  treaty  provisions. No part of the Material may be used, copied, reproduced,
!  modified, published, uploaded, posted, transmitted, distributed or disclosed
!  in any way without Intel's prior express written permission.
!  No license  under any  patent, copyright, trade secret or other intellectual
!  property right is granted to or conferred upon you by disclosure or delivery
!  of the Materials,  either expressly, by implication, inducement, estoppel or
!  otherwise.  Any  license  under  such  intellectual property  rights must be
!  express and approved by Intel in writing.
!
!==============================================================================!
!
!   A module  that describes  interfaces  of  interval  arithmetic  procedures
!   implemented in C/C++ for plugging them into programs written in Fortran-95
!
!
!   Mnemonics for the types of operations arguments:
!
!   S  - single precision real
!
!   D  - double precision real
!
!   SI - single precision interval
!
!   DI - double precision interval
!
!   CR - single precision complex rectangular interval
!
!   ZR - double precision complex rectangular interval
!
!   CC - single precision complex circular interval
!
!   ZC - double precision complex circular interval
!
!==============================================================================!


MODULE INTERVAL_ARITHMETIC


    IMPLICIT NONE


    !--------------------------------------------------------------------------!


    TYPE S_INTERVAL
        REAL(4) ::  INF, SUP
    END TYPE S_INTERVAL

    TYPE D_INTERVAL
        REAL(8) ::  INF, SUP
    END TYPE D_INTERVAL


    !--------------------------------------------------------------------------!


    TYPE CR_INTERVAL
        TYPE(S_INTERVAL) ::  RE, IM
    END TYPE CR_INTERVAL

    TYPE ZR_INTERVAL
        TYPE(D_INTERVAL) ::  RE, IM
    END TYPE ZR_INTERVAL

    TYPE CC_INTERVAL
        COMPLEX(4) ::  MID
        REAL(4)    ::  RAD
    END TYPE CC_INTERVAL

    TYPE ZC_INTERVAL
        COMPLEX(8) ::  MID
        REAL(8)    ::  RAD
    END TYPE ZC_INTERVAL


    !--------------------------------------------------------------------------!


    INTERFACE ASSIGNMENT(=)

        SUBROUTINE SI_COPY(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(OUT) ::  X
            TYPE(S_INTERVAL), INTENT(IN)  ::  Y
        END SUBROUTINE SI_COPY

        SUBROUTINE DI_COPY(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(OUT) ::  X
            TYPE(D_INTERVAL), INTENT(IN)  ::  Y
        END SUBROUTINE DI_COPY

        SUBROUTINE CR_COPY(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(OUT) ::  X
            TYPE(CR_INTERVAL), INTENT(IN)  ::  Y
        END SUBROUTINE CR_COPY

        SUBROUTINE ZR_COPY(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(OUT) ::  X
            TYPE(ZR_INTERVAL), INTENT(IN)  ::  Y
        END SUBROUTINE ZR_COPY

        SUBROUTINE CC_COPY(X,Y)
            TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(OUT) ::  X
            TYPE(CC_INTERVAL), INTENT(IN)  ::  Y
        END SUBROUTINE CC_COPY

        SUBROUTINE ZC_COPY(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(OUT) ::  X
            TYPE(ZC_INTERVAL), INTENT(IN)  ::  Y
        END SUBROUTINE ZC_COPY

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE SINTERVAL

        FUNCTION I_SI_CONVERT(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            INTEGER, INTENT(IN) ::  X
            TYPE(S_INTERVAL) ::  I_SI_CONVERT
        END FUNCTION I_SI_CONVERT

        FUNCTION I_I_SI_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            INTEGER, INTENT(IN) ::  X, Y
            TYPE(S_INTERVAL) ::  I_I_SI_CONVERT
        END FUNCTION I_I_SI_CONVERT

        FUNCTION SR_SI_CONVERT(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            REAL(4), INTENT(IN) ::  X
            TYPE(S_INTERVAL) ::  SR_SI_CONVERT
        END FUNCTION SR_SI_CONVERT

        FUNCTION DR_SI_CONVERT(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            REAL(8), INTENT(IN) ::  X
            TYPE(S_INTERVAL) ::  DR_SI_CONVERT
        END FUNCTION DR_SI_CONVERT

        FUNCTION SR_SR_SI_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            REAL(4), INTENT(IN) ::  X, Y
            TYPE(S_INTERVAL) ::  SR_SR_SI_CONVERT
        END FUNCTION SR_SR_SI_CONVERT

        FUNCTION DR_SR_SI_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            REAL(8), INTENT(IN) ::  X
            REAL(4), INTENT(IN) ::  Y
            TYPE(S_INTERVAL) ::  DR_SR_SI_CONVERT
        END FUNCTION DR_SR_SI_CONVERT

        FUNCTION SR_DR_SI_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            REAL(4), INTENT(IN) ::  X
            REAL(8), INTENT(IN) ::  Y
            TYPE(S_INTERVAL) ::  SR_DR_SI_CONVERT
        END FUNCTION SR_DR_SI_CONVERT

        FUNCTION DR_DR_SI_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            REAL(8), INTENT(IN) ::  X, Y
            TYPE(S_INTERVAL) ::  DR_DR_SI_CONVERT
        END FUNCTION DR_DR_SI_CONVERT

        FUNCTION DI_SI_CONVERT(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            TYPE(S_INTERVAL) ::  DI_SI_CONVERT
        END FUNCTION DI_SI_CONVERT

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE DINTERVAL

        FUNCTION I_DI_CONVERT(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            INTEGER, INTENT(IN) ::  X
            TYPE(D_INTERVAL) ::  I_DI_CONVERT
        END FUNCTION I_DI_CONVERT

        FUNCTION I_I_DI_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            INTEGER, INTENT(IN) ::  X, Y
            TYPE(D_INTERVAL) ::  I_I_DI_CONVERT
        END FUNCTION I_I_DI_CONVERT

        FUNCTION DR_DI_CONVERT(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            REAL(8), INTENT(IN) ::  X
            TYPE(D_INTERVAL) ::  DR_DI_CONVERT
        END FUNCTION DR_DI_CONVERT

        FUNCTION SR_DI_CONVERT(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            REAL(4), INTENT(IN) ::  X
            TYPE(D_INTERVAL) ::  SR_DI_CONVERT
        END FUNCTION SR_DI_CONVERT

        FUNCTION DR_DR_DI_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            REAL(8), INTENT(IN) ::  X, Y
            TYPE(D_INTERVAL) ::  DR_DR_DI_CONVERT
        END FUNCTION DR_DR_DI_CONVERT

        FUNCTION SR_DR_DI_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            REAL(4), INTENT(IN) ::  X
            REAL(8), INTENT(IN) ::  Y
            TYPE(D_INTERVAL) ::  SR_DR_DI_CONVERT
        END FUNCTION SR_DR_DI_CONVERT

        FUNCTION DR_SR_DI_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            REAL(8), INTENT(IN) ::  X
            REAL(4), INTENT(IN) ::  Y
            TYPE(D_INTERVAL) ::  DR_SR_DI_CONVERT
        END FUNCTION DR_SR_DI_CONVERT

        FUNCTION SR_SR_DI_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            REAL(4), INTENT(IN) ::  X, Y
            TYPE(D_INTERVAL) ::  SR_SR_DI_CONVERT
        END FUNCTION SR_SR_DI_CONVERT

        FUNCTION SI_DI_CONVERT(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            TYPE(D_INTERVAL) ::  SI_DI_CONVERT
        END FUNCTION SI_DI_CONVERT

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE CRINTERVAL

        FUNCTION I_CR_CONVERT(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            INTEGER, INTENT(IN) ::  X
            TYPE(CR_INTERVAL) ::  I_CR_CONVERT
        END FUNCTION I_CR_CONVERT

        FUNCTION I_I_CR_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            INTEGER, INTENT(IN) ::  X, Y
            TYPE(CR_INTERVAL) ::  I_I_CR_CONVERT
        END FUNCTION I_I_CR_CONVERT

        FUNCTION S_CR_CONVERT(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            REAL(4), INTENT(IN) ::  X
            TYPE(CR_INTERVAL) ::  S_CR_CONVERT
        END FUNCTION S_CR_CONVERT

        FUNCTION D_CR_CONVERT(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            REAL(8), INTENT(IN) ::  X
            TYPE(CR_INTERVAL) ::  D_CR_CONVERT
        END FUNCTION D_CR_CONVERT

        FUNCTION S_S_CR_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            REAL(4), INTENT(IN) ::  X, Y
            TYPE(CR_INTERVAL) ::  S_S_CR_CONVERT
        END FUNCTION S_S_CR_CONVERT

        FUNCTION D_D_CR_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            REAL(8), INTENT(IN) ::  X, Y
            TYPE(CR_INTERVAL) ::  D_D_CR_CONVERT
        END FUNCTION D_D_CR_CONVERT

        FUNCTION S_D_CR_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            REAL(4), INTENT(IN) ::  X
            REAL(8), INTENT(IN) ::  Y
            TYPE(CR_INTERVAL) ::  S_D_CR_CONVERT
        END FUNCTION S_D_CR_CONVERT

        FUNCTION D_S_CR_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            REAL(8), INTENT(IN) ::  X
            REAL(4), INTENT(IN) ::  Y
            TYPE(CR_INTERVAL) ::  D_S_CR_CONVERT
        END FUNCTION D_S_CR_CONVERT

        FUNCTION C_CR_CONVERT(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            COMPLEX(4), INTENT(IN) ::  X
            TYPE(CR_INTERVAL) ::  C_CR_CONVERT
        END FUNCTION C_CR_CONVERT

        FUNCTION Z_CR_CONVERT(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            COMPLEX(8), INTENT(IN) ::  X
            TYPE(CR_INTERVAL) ::  Z_CR_CONVERT
        END FUNCTION Z_CR_CONVERT

        FUNCTION C_C_CR_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            COMPLEX(4), INTENT(IN) ::  X, Y
            TYPE(CR_INTERVAL) ::  C_C_CR_CONVERT
        END FUNCTION C_C_CR_CONVERT

        FUNCTION C_Z_CR_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            COMPLEX(4), INTENT(IN) ::  X
            COMPLEX(8), INTENT(IN) ::  Y
            TYPE(CR_INTERVAL) ::  C_Z_ZR_CONVERT
        END FUNCTION C_Z_CR_CONVERT

        FUNCTION Z_C_CR_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            COMPLEX(8), INTENT(IN) ::  X
            COMPLEX(4), INTENT(IN) ::  Y
            TYPE(CR_INTERVAL) ::  Z_C_CR_CONVERT
        END FUNCTION Z_C_CR_CONVERT

        FUNCTION Z_Z_CR_CONVERT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            COMPLEX(8), INTENT(IN) ::  X, Y
            TYPE(CR_INTERVAL) ::  Z_Z_CR_CONVERT
        END FUNCTION Z_Z_CR_CONVERT

        FUNCTION SI_CR_CONVERT(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            TYPE(CR_INTERVAL) ::  SI_CR_CONVERT
        END FUNCTION SI_CR_CONVERT

        FUNCTION DI_CR_CONVERT(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            TYPE(CR_INTERVAL) ::  DI_CR_CONVERT
        END FUNCTION DI_CR_CONVERT

    END INTERFACE CRINTERVAL


    !--------------------------------------------------------------------------!


    INTERFACE ZRINTERVAL

        FUNCTION I_ZR_CONVERT(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            INTEGER, INTENT(IN) ::  X
            TYPE(ZR_INTERVAL) ::  I_ZR_CONVERT
        END FUNCTION I_ZR_CONVERT

        FUNCTION I_I_ZR_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            INTEGER, INTENT(IN) ::  X, Y
            TYPE(ZR_INTERVAL) ::  I_I_ZR_CONVERT
        END FUNCTION I_I_ZR_CONVERT

        FUNCTION S_ZR_CONVERT(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            REAL(4), INTENT(IN) ::  X
            TYPE(ZR_INTERVAL) ::  S_ZR_CONVERT
        END FUNCTION S_ZR_CONVERT

        FUNCTION D_ZR_CONVERT(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            REAL(8), INTENT(IN) ::  X
            TYPE(ZR_INTERVAL) ::  D_ZR_CONVERT
        END FUNCTION D_ZR_CONVERT

        FUNCTION S_S_ZR_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            REAL(4), INTENT(IN) ::  X, Y
            TYPE(ZR_INTERVAL) ::  S_S_ZR_CONVERT
        END FUNCTION S_S_ZR_CONVERT

        FUNCTION S_D_ZR_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            REAL(4), INTENT(IN) ::  X
            REAL(8), INTENT(IN) ::  Y
            TYPE(ZR_INTERVAL) ::  S_D_ZR_CONVERT
        END FUNCTION S_D_ZR_CONVERT

        FUNCTION D_S_ZR_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            REAL(8), INTENT(IN) ::  X
            REAL(4), INTENT(IN) ::  Y
            TYPE(ZR_INTERVAL) ::  D_S_ZR_CONVERT
        END FUNCTION D_S_ZR_CONVERT

        FUNCTION D_D_ZR_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            REAL(8), INTENT(IN) ::  X, Y
            TYPE(ZR_INTERVAL) ::  D_D_ZR_CONVERT
        END FUNCTION D_D_ZR_CONVERT

        FUNCTION C_ZR_CONVERT(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            COMPLEX(4), INTENT(IN) ::  X
            TYPE(ZR_INTERVAL) ::  C_ZR_CONVERT
        END FUNCTION C_ZR_CONVERT

        FUNCTION Z_ZR_CONVERT(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            COMPLEX(8), INTENT(IN) ::  X
            TYPE(ZR_INTERVAL) ::  Z_ZR_CONVERT
        END FUNCTION Z_ZR_CONVERT

        FUNCTION C_C_ZR_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            COMPLEX(4), INTENT(IN) ::  X, Y
            TYPE(ZR_INTERVAL) ::  C_C_ZR_CONVERT
        END FUNCTION C_C_ZR_CONVERT

        FUNCTION C_Z_ZR_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            COMPLEX(4), INTENT(IN) ::  X
            COMPLEX(8), INTENT(IN) ::  Y
            TYPE(ZR_INTERVAL) ::  C_Z_ZR_CONVERT
        END FUNCTION C_Z_ZR_CONVERT

        FUNCTION Z_C_ZR_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            COMPLEX(8), INTENT(IN) ::  X
            COMPLEX(4), INTENT(IN) ::  Y
            TYPE(ZR_INTERVAL) ::  Z_C_ZR_CONVERT
        END FUNCTION Z_C_ZR_CONVERT

        FUNCTION Z_Z_ZR_CONVERT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            COMPLEX(8), INTENT(IN) ::  X, Y
            TYPE(ZR_INTERVAL) ::  Z_Z_ZR_CONVERT
        END FUNCTION Z_Z_ZR_CONVERT

        FUNCTION SI_ZR_CONVERT(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            TYPE(ZR_INTERVAL) ::  SI_ZR_CONVERT
        END FUNCTION SI_ZR_CONVERT

        FUNCTION DI_ZR_CONVERT(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            TYPE(ZR_INTERVAL) ::  DI_ZR_CONVERT
        END FUNCTION DI_ZR_CONVERT

    END INTERFACE ZRINTERVAL


    !--------------------------------------------------------------------------!


    INTERFACE CCINTERVAL

        FUNCTION I_CC_CONVERT(X)
            TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            INTEGER, INTENT(IN) ::  X
            TYPE(CC_INTERVAL) ::  I_CC_CONVERT
        END FUNCTION I_CC_CONVERT

        FUNCTION I_I_CC_CONVERT(X,Y)
            TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            INTEGER, INTENT(IN) ::  X, Y
            TYPE(CC_INTERVAL) ::  I_CC_CONVERT
        END FUNCTION I_I_CC_CONVERT

        FUNCTION S_CC_CONVERT(X)
            TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            REAL(4), INTENT(IN) ::  X
            TYPE(CC_INTERVAL) ::  S_CC_CONVERT
        END FUNCTION S_CC_CONVERT

        FUNCTION D_CC_CONVERT(X)
            TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            REAL(8), INTENT(IN) ::  X
            TYPE(CC_INTERVAL) ::  D_CC_CONVERT
        END FUNCTION D_CC_CONVERT

        FUNCTION S_S_CC_CONVERT(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            REAL(4), INTENT(IN) ::  X, Y
            TYPE(CC_INTERVAL) ::  S_S_CC_CONVERT
        END FUNCTION S_S_CC_CONVERT

        FUNCTION S_D_CC_CONVERT(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            REAL(4), INTENT(IN) ::  X
            REAL(8), INTENT(IN) ::  Y
            TYPE(CC_INTERVAL) ::  S_D_CC_CONVERT
        END FUNCTION S_D_CC_CONVERT

        FUNCTION D_S_CC_CONVERT(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            REAL(8), INTENT(IN) ::  X
            REAL(4), INTENT(IN) ::  Y
            TYPE(CC_INTERVAL) ::  D_S_CC_CONVERT
        END FUNCTION D_S_CC_CONVERT

        FUNCTION D_D_CC_CONVERT(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            REAL(8), INTENT(IN) ::  X, Y
            TYPE(CC_INTERVAL) ::  D_D_CC_CONVERT
        END FUNCTION D_D_CC_CONVERT

        FUNCTION C_CC_CONVERT(X)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            COMPLEX(4), INTENT(IN) ::  X
            TYPE(CC_INTERVAL) ::  C_CC_CONVERT
        END FUNCTION C_CC_CONVERT

        FUNCTION Z_CC_CONVERT(X)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            COMPLEX(8), INTENT(IN) ::  X
            TYPE(CC_INTERVAL) ::  Z_CC_CONVERT
        END FUNCTION Z_CC_CONVERT

        FUNCTION C_S_CC_CONVERT(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            COMPLEX(4), INTENT(IN) ::  X
            REAL(4), INTENT(IN)    ::  Y
            TYPE(CC_INTERVAL) ::  C_S_CC_CONVERT
        END FUNCTION C_S_CC_CONVERT

        FUNCTION C_D_CC_CONVERT(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            COMPLEX(4), INTENT(IN) ::  X
            REAL(8), INTENT(IN)    ::  Y
            TYPE(CC_INTERVAL) ::  C_D_CC_CONVERT
        END FUNCTION C_D_CC_CONVERT

        FUNCTION Z_S_CC_CONVERT(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            COMPLEX(8), INTENT(IN) ::  X
            REAL(4), INTENT(IN)    ::  Y
            TYPE(CC_INTERVAL) ::  Z_S_CC_CONVERT
        END FUNCTION Z_S_CC_CONVERT

        FUNCTION Z_D_CC_CONVERT(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            COMPLEX(8), INTENT(IN) ::  X
            REAL(8), INTENT(IN)    ::  Y
            TYPE(CC_INTERVAL) ::  Z_D_CC_CONVERT
        END FUNCTION Z_D_CC_CONVERT

    END INTERFACE CCINTERVAL


    !--------------------------------------------------------------------------!


    INTERFACE ZCINTERVAL

        FUNCTION I_ZC_CONVERT(X)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            INTEGER, INTENT(IN) ::  X
            TYPE(ZC_INTERVAL) ::  I_ZC_CONVERT
        END FUNCTION I_ZC_CONVERT

        FUNCTION I_I_ZC_CONVERT(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            INTEGER, INTENT(IN) ::  X, Y
            TYPE(ZC_INTERVAL) ::  I_ZC_CONVERT
        END FUNCTION I_I_ZC_CONVERT

        FUNCTION S_ZC_CONVERT(X)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            REAL(4), INTENT(IN) ::  X
            TYPE(ZC_INTERVAL) ::  S_ZC_CONVERT
        END FUNCTION S_ZC_CONVERT

        FUNCTION D_ZC_CONVERT(X)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            REAL(8), INTENT(IN) ::  X
            TYPE(ZC_INTERVAL) ::  D_ZC_CONVERT
        END FUNCTION D_ZC_CONVERT

        FUNCTION S_S_ZC_CONVERT(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            REAL(4), INTENT(IN) ::  X, Y
            TYPE(ZC_INTERVAL) ::  S_S_ZC_CONVERT
        END FUNCTION S_S_ZC_CONVERT

        FUNCTION S_D_ZC_CONVERT(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            REAL(4), INTENT(IN) ::  X
            REAL(8), INTENT(IN) ::  Y
            TYPE(ZC_INTERVAL) ::  S_D_ZC_CONVERT
        END FUNCTION S_D_ZC_CONVERT

        FUNCTION D_S_ZC_CONVERT(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            REAL(8), INTENT(IN) ::  X
            REAL(4), INTENT(IN) ::  Y
            TYPE(ZC_INTERVAL) ::  D_S_ZC_CONVERT
        END FUNCTION D_S_ZC_CONVERT

        FUNCTION D_D_ZC_CONVERT(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            REAL(8), INTENT(IN) ::  X, Y
            TYPE(ZC_INTERVAL) ::  D_D_ZC_CONVERT
        END FUNCTION D_D_ZC_CONVERT

        FUNCTION C_ZC_CONVERT(X)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            COMPLEX(4), INTENT(IN) ::  X
            TYPE(ZC_INTERVAL) ::  C_ZC_CONVERT
        END FUNCTION C_ZC_CONVERT

        FUNCTION Z_ZC_CONVERT(X)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            COMPLEX(8), INTENT(IN) ::  X
            TYPE(ZC_INTERVAL) ::  Z_ZC_CONVERT
        END FUNCTION Z_ZC_CONVERT

        FUNCTION C_S_ZC_CONVERT(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            COMPLEX(4), INTENT(IN) ::  X
            REAL(4), INTENT(IN)    ::  Y
            TYPE(ZC_INTERVAL) ::  C_S_ZC_CONVERT
        END FUNCTION C_S_ZC_CONVERT

        FUNCTION C_D_ZC_CONVERT(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            COMPLEX(4), INTENT(IN) ::  X
            REAL(8), INTENT(IN)    ::  Y
            TYPE(ZC_INTERVAL) ::  C_D_ZC_CONVERT
        END FUNCTION C_D_ZC_CONVERT

        FUNCTION Z_S_ZC_CONVERT(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            COMPLEX(8), INTENT(IN) ::  X
            REAL(4), INTENT(IN)    ::  Y
            TYPE(ZC_INTERVAL) ::  Z_S_ZC_CONVERT
        END FUNCTION Z_S_ZC_CONVERT

        FUNCTION Z_D_ZC_CONVERT(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            COMPLEX(8), INTENT(IN) ::  X
            REAL(8), INTENT(IN)    ::  Y
            TYPE(ZC_INTERVAL) ::  Z_D_ZC_CONVERT
        END FUNCTION Z_D_ZC_CONVERT

    END INTERFACE ZCINTERVAL


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR(+)

        FUNCTION SI_SI_ADDI(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(S_INTERVAL) ::  SI_SI_ADDI
        END FUNCTION SI_SI_ADDI

        FUNCTION SR_SI_ADDI(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            REAL(4), INTENT(IN) ::  X
            TYPE(S_INTERVAL), INTENT(IN) ::  Y
            TYPE(S_INTERVAL) ::  SR_SI_ADDI
        END FUNCTION SR_SI_ADDI

        FUNCTION SI_SR_ADDI(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            REAL(4), INTENT(IN) ::  Y
            TYPE(S_INTERVAL) ::  SI_SR_ADDI
        END FUNCTION SI_SR_ADDI

        FUNCTION DI_DI_ADDI(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(D_INTERVAL) ::  DI_DI_ADDI
        END FUNCTION DI_DI_ADDI

        FUNCTION DR_DI_ADDI(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            REAL(8), INTENT(IN) :: X
            TYPE(D_INTERVAL), INTENT(IN) ::  Y
            TYPE(D_INTERVAL) ::  DR_DI_ADDI
        END FUNCTION DR_DI_ADDI

        FUNCTION DI_DR_ADDI(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            REAL(8), INTENT(IN) ::  Y
            TYPE(D_INTERVAL) ::  DI_DR_ADDI
        END FUNCTION DI_DR_ADDI

        FUNCTION CR_CR_ADDI(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(CR_INTERVAL) ::  CR_CR_ADDI
        END FUNCTION CR_CR_ADDI

        FUNCTION ZR_ZR_ADDI(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(ZR_INTERVAL) ::  ZR_ZR_ADDI
        END FUNCTION ZR_ZR_ADDI

        FUNCTION CC_CC_ADDI(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(CC_INTERVAL) ::  CC_CC_ADDI
        END FUNCTION CC_CC_ADDI

        FUNCTION ZC_ZC_ADDI(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(ZC_INTERVAL) ::  ZC_ZC_ADDI
        END FUNCTION ZC_ZC_ADDI

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR(-)

        FUNCTION SI_SI_SUBT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(S_INTERVAL) ::  SI_SI_SUBT
        END FUNCTION SI_SI_SUBT

        FUNCTION SR_SI_SUBT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            REAL(4), INTENT(IN) ::  X
            TYPE(S_INTERVAL), INTENT(IN) ::  Y
            TYPE(S_INTERVAL) ::  SR_SI_SUBT
        END FUNCTION SR_SI_SUBT

        FUNCTION SI_SR_SUBT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            REAL(4), INTENT(IN) ::  Y
            TYPE(S_INTERVAL) ::  SI_SR_SUBT
        END FUNCTION SI_SR_SUBT

        FUNCTION DI_DI_SUBT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(D_INTERVAL) ::  DI_DI_SUBT
        END FUNCTION DI_DI_SUBT

        FUNCTION DR_DI_SUBT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            REAL(8), INTENT(IN) ::  X
            TYPE(D_INTERVAL), INTENT(IN) ::  Y
            TYPE(D_INTERVAL) ::  DR_DI_SUBT
        END FUNCTION DR_DI_SUBT

        FUNCTION DI_DR_SUBT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            REAL(8), INTENT(IN) ::  Y
            TYPE(D_INTERVAL) ::  DI_DR_SUBT
        END FUNCTION DI_DR_SUBT

        FUNCTION CR_CR_SUBT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(CR_INTERVAL) ::  CR_CR_SUBT
        END FUNCTION CR_CR_SUBT

        FUNCTION ZR_ZR_SUBT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(ZR_INTERVAL) ::  ZR_ZR_SUBT
        END FUNCTION ZR_ZR_SUBT

        FUNCTION CC_CC_SUBT(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(CC_INTERVAL) ::  CC_CC_SUBT
        END FUNCTION CC_CC_SUBT

        FUNCTION ZC_ZC_SUBT(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(ZC_INTERVAL) ::  ZC_ZC_SUBT
        END FUNCTION ZC_ZC_SUBT

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR(*)

        FUNCTION SI_SI_MULT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(S_INTERVAL) ::  SI_SI_MULT
        END FUNCTION SI_SI_MULT

        FUNCTION SR_SI_MULT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            REAL(4), INTENT(IN) ::  X
            TYPE(S_INTERVAL), INTENT(IN) ::  Y
            TYPE(S_INTERVAL) ::  SR_SI_MULT
        END FUNCTION SR_SI_MULT

        FUNCTION SI_SR_MULT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            REAL(4), INTENT(IN) ::  Y
            TYPE(S_INTERVAL) ::  SI_SR_MULT
        END FUNCTION SI_SR_MULT

        FUNCTION DI_DI_MULT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(D_INTERVAL) ::  DI_DI_MULT
        END FUNCTION DI_DI_MULT

        FUNCTION DR_DI_MULT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            REAL(8), INTENT(IN) ::  X
            TYPE(D_INTERVAL), INTENT(IN) ::  Y
            TYPE(D_INTERVAL) ::  DR_DI_MULT
        END FUNCTION DR_DI_MULT

        FUNCTION DI_DR_MULT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            REAL(8), INTENT(IN) ::  Y
            TYPE(D_INTERVAL) ::  DI_DR_MULT
        END FUNCTION DI_DR_MULT

        FUNCTION CR_CR_MULT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(CR_INTERVAL) ::  CR_CR_MULT
        END FUNCTION CR_CR_MULT

        FUNCTION C_CR_MULT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            COMPLEX(4), INTENT(IN)        ::  X
            TYPE(CR_INTERVAL), INTENT(IN) ::  Y
            TYPE(CR_INTERVAL) ::  C_CR_MULT
        END FUNCTION C_CR_MULT

        FUNCTION CR_C_MULT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X
            COMPLEX(4), INTENT(IN) ::         Y
            TYPE(CR_INTERVAL) ::  CR_C_MULT
        END FUNCTION CR_C_MULT

        FUNCTION ZR_ZR_MULT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(ZR_INTERVAL) ::  ZR_ZR_MULT
        END FUNCTION ZR_ZR_MULT

        FUNCTION Z_ZR_MULT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            COMPLEX(8), INTENT(IN)        ::  X
            TYPE(ZR_INTERVAL), INTENT(IN) ::  Y
            TYPE(ZR_INTERVAL) ::  Z_ZR_MULT
        END FUNCTION Z_ZR_MULT

        FUNCTION ZR_Z_MULT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X
            COMPLEX(8), INTENT(IN)        ::  Y
            TYPE(ZR_INTERVAL) ::  ZR_Z_MULT
        END FUNCTION ZR_Z_MULT

        FUNCTION CC_CC_MULT(X,Y)
            TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(CC_INTERVAL) ::  CC_CC_MULT
        END FUNCTION CC_CC_MULT

        FUNCTION C_CC_MULT(X,Y)
            TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            COMPLEX(4), INTENT(IN)        ::  X
            TYPE(CC_INTERVAL), INTENT(IN) ::  Y
            TYPE(CC_INTERVAL) ::  C_CC_MULT
        END FUNCTION C_CC_MULT

        FUNCTION CC_C_MULT(X,Y)
            TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X
            COMPLEX(4), INTENT(IN)        ::  Y
            TYPE(CC_INTERVAL) ::  CC_C_MULT
        END FUNCTION CC_C_MULT

        FUNCTION ZC_ZC_MULT(X,Y)
             TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(ZC_INTERVAL) ::  ZC_ZC_MULT
        END FUNCTION ZC_ZC_MULT

        FUNCTION Z_ZC_MULT(X,Y)
             TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            COMPLEX(8), INTENT(IN)        ::  X
            TYPE(ZC_INTERVAL), INTENT(IN) ::  Y
            TYPE(ZC_INTERVAL) ::  Z_ZC_MULT
        END FUNCTION Z_ZC_MULT

        FUNCTION ZC_Z_MULT(X,Y)
             TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X
            COMPLEX(8), INTENT(IN)        ::  Y
            TYPE(ZC_INTERVAL) ::  ZC_Z_MULT
        END FUNCTION ZC_Z_MULT

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR(/)

        FUNCTION SI_SI_DIVI(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(S_INTERVAL) ::  SI_SI_DIVI
        END FUNCTION SI_SI_DIVI

        FUNCTION SR_SI_DIVI(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            REAL(4), INTENT(IN) ::  X
            TYPE(S_INTERVAL), INTENT(IN) ::  Y
            TYPE(S_INTERVAL) ::  SR_SI_DIVI
        END FUNCTION SR_SI_DIVI

        FUNCTION SI_SR_DIVI(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            REAL(4), INTENT(IN) :: Y
            TYPE(S_INTERVAL) ::  SI_SR_DIVI
        END FUNCTION SI_SR_DIVI

        FUNCTION DI_DI_DIVI(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(D_INTERVAL) ::  DI_DI_DIVI
        END FUNCTION DI_DI_DIVI

        FUNCTION DR_DI_DIVI(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            REAL(8), INTENT(IN) ::  X
            TYPE(D_INTERVAL), INTENT(IN) ::  Y
            TYPE(D_INTERVAL) ::  DR_DI_DIVI
        END FUNCTION DR_DI_DIVI

        FUNCTION DI_DR_DIVI(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            REAL(8), INTENT(IN) ::  Y
            TYPE(D_INTERVAL) ::  DI_DR_DIVI
        END FUNCTION DI_DR_DIVI

        FUNCTION CR_CR_DIVI(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(CR_INTERVAL) ::  CR_CR_DIVI
        END FUNCTION CR_CR_DIVI

        FUNCTION ZR_ZR_DIVI(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(ZR_INTERVAL) ::  ZR_ZR_DIVI
        END FUNCTION ZR_ZR_DIVI

        FUNCTION CC_CC_DIVI(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(CC_INTERVAL) ::  CC_CC_DIVI
        END FUNCTION CC_CC_DIVI

        FUNCTION ZC_ZC_DIVI(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(ZC_INTERVAL) ::  ZC_ZC_DIVI
        END FUNCTION ZC_ZC_DIVI

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE INF

        FUNCTION S_INF(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            REAL(4) ::  S_INF
        END FUNCTION S_INF

        FUNCTION D_INF(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            REAL(8) ::  D_INF
        END FUNCTION D_INF

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE SUP

        FUNCTION S_SUP(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            REAL(4) ::  S_SUP
        END FUNCTION S_SUP

        FUNCTION D_SUP(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            REAL(8) ::  D_SUP
        END FUNCTION D_SUP

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE ICONJ

        FUNCTION CR_CONJ(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X
            TYPE(CR_INTERVAL) ::  CR_CONJ
        END FUNCTION CR_CONJ

        FUNCTION ZR_CONJ(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X
            TYPE(ZR_INTERVAL) ::  ZR_CONJ
        END FUNCTION ZR_CONJ

        FUNCTION CC_CONJ(X)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X
            TYPE(CC_INTERVAL) ::  CC_CONJ
        END FUNCTION CC_CONJ

        FUNCTION ZC_CONJ(X)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X
            TYPE(ZC_INTERVAL) ::  ZC_CONJ
        END FUNCTION ZC_CONJ

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE MID

        FUNCTION S_MID(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            REAL(4) ::  S_MID
        END FUNCTION S_MID

        FUNCTION D_MID(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            REAL(8) ::  D_MID
        END FUNCTION D_MID

        FUNCTION CR_MID(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X
            COMPLEX(4) ::  CR_MID
        END FUNCTION CR_MID

        FUNCTION ZR_MID(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X
            COMPLEX(8) ::  ZR_MID
        END FUNCTION ZR_MID

        FUNCTION CC_MID(X)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X
            COMPLEX(4) ::  CC_MID
        END FUNCTION CC_MID

        FUNCTION ZC_MID(X)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X
            COMPLEX(8) ::  ZC_MID
        END FUNCTION ZC_MID

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE RAD

        FUNCTION S_RAD(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            REAL(4) ::  S_RAD
        END FUNCTION S_RAD

        FUNCTION D_RAD(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            REAL(8) ::  D_RAD
        END FUNCTION D_RAD

        FUNCTION CR_RAD(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X
            REAL(4) ::  CR_RAD
        END FUNCTION CR_RAD

        FUNCTION ZR_RAD(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X
            REAL(8) ::  ZR_RAD
        END FUNCTION ZR_RAD

        FUNCTION CC_RAD(X)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X
            REAL(4) ::  CC_RAD
        END FUNCTION CC_RAD

        FUNCTION ZC_RAD(X)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X
            REAL(8) ::  ZC_RAD
        END FUNCTION ZC_RAD

    END INTERFACE


    !--------------------------------------------------------------------------!

    INTERFACE WID

        FUNCTION S_WID(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            REAL(4) ::  S_WID
        END FUNCTION S_WID

        FUNCTION D_WID(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            REAL(8) ::  D_WID
        END FUNCTION D_WID

    END INTERFACE

    !--------------------------------------------------------------------------!


    INTERFACE MIG

        FUNCTION S_MIG(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            REAL(4) ::  S_MIG
        END FUNCTION S_MIG

        FUNCTION D_MIG(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            REAL(8) ::  D_MIG
        END FUNCTION D_MIG

        FUNCTION CR_MIG(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X
            REAL(4) ::  CR_MIG
        END FUNCTION CR_MIG

        FUNCTION ZR_MIG(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X
            REAL(8) ::  ZR_MIG
        END FUNCTION ZR_MIG

        FUNCTION CC_MIG(X)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X
            REAL(4) ::  CC_MIG
        END FUNCTION CC_MIG

        FUNCTION ZC_MIG(X)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X
            REAL(8) ::  ZC_MIG
        END FUNCTION ZC_MIG

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE MAG

        FUNCTION S_MAG(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            REAL(4) ::  S_MAG
        END FUNCTION S_MAG

        FUNCTION D_MAG(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            REAL(8) ::  D_MAG
        END FUNCTION D_MAG

        FUNCTION CR_MAG(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X
            REAL(4) ::  CR_MAG
        END FUNCTION CR_MAG

        FUNCTION ZR_MAG(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X
            REAL(8) ::  ZR_MAG
        END FUNCTION ZR_MAG

        FUNCTION CC_MAG(X)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X
            REAL(4) ::  CC_MAG
        END FUNCTION CC_MAG

        FUNCTION ZC_MAG(X)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X
            REAL(8) ::  ZC_MAG
        END FUNCTION ZC_MAG

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE ABS

        FUNCTION SI_ABS(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            TYPE(S_INTERVAL) ::  SI_ABS
        END FUNCTION SI_ABS

        FUNCTION DI_ABS(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            TYPE(D_INTERVAL) ::  DI_ABS
        END FUNCTION DI_ABS

    END INTERFACE

    !--------------------------------------------------------------------------!


    INTERFACE SQR

        FUNCTION SI_SQR(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            TYPE(S_INTERVAL) ::  SI_SQR
        END FUNCTION SI_SQR

        FUNCTION DI_SQR(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            TYPE(D_INTERVAL) ::  DI_SQR
        END FUNCTION DI_SQR

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR(.IX.)

        FUNCTION S_IX(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(S_INTERVAL) ::  S_IX
        END FUNCTION S_IX

        FUNCTION D_IX(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(D_INTERVAL) ::  D_IX
        END FUNCTION D_IX

        FUNCTION CR_IX(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(CR_INTERVAL) ::  CR_IX
        END FUNCTION CR_IX

        FUNCTION ZR_IX(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(ZR_INTERVAL) ::  ZR_IX
        END FUNCTION ZR_IX

        FUNCTION CC_IX(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(CC_INTERVAL) ::  CC_IX
        END FUNCTION CC_IX

        FUNCTION ZC_IX(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(ZC_INTERVAL) ::  ZC_IX
        END FUNCTION ZC_IX

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR(.IH.)

        FUNCTION SI_SI_IH(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(S_INTERVAL) ::  SI_SI_IH
        END FUNCTION SI_SI_IH

        FUNCTION SR_SI_IH(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            REAL(4), INTENT(IN) ::  X
            TYPE(S_INTERVAL), INTENT(IN) ::  Y
            TYPE(S_INTERVAL) ::  SR_SI_IH
        END FUNCTION SR_SI_IH

        FUNCTION SI_SR_IH(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            REAL(4), INTENT(IN) ::  Y
            TYPE(S_INTERVAL) ::  SI_SR_IH
        END FUNCTION SI_SR_IH

        FUNCTION DI_DI_IH(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            TYPE(D_INTERVAL) ::  DI_DI_IH
        END FUNCTION DI_DI_IH

        FUNCTION DR_DI_IH(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            REAL(8), INTENT(IN) ::  X
            TYPE(D_INTERVAL), INTENT(IN) ::  Y
            TYPE(D_INTERVAL) ::  DR_DI_IH
        END FUNCTION DR_DI_IH

        FUNCTION DI_DR_IH(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            REAL(8), INTENT(IN) ::  Y
            TYPE(D_INTERVAL) ::  DI_DR_IH
        END FUNCTION DI_DR_IH

    END INTERFACE


    !--------------------------------------------------------------------------!

    INTERFACE ISEMPTY

        FUNCTION S_ISEMPTY(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X
            LOGICAL ::  S_ISEMPTY
        END FUNCTION S_ISEMPTY

        FUNCTION D_ISEMPTY(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X
            LOGICAL ::  D_ISEMPTY
        END FUNCTION D_ISEMPTY

        FUNCTION CR_ISEMPTY(X)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X
            LOGICAL ::  CR_ISEMPTY
        END FUNCTION CR_ISEMPTY

        FUNCTION ZR_ISEMPTY(X)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X
            LOGICAL ::  ZR_ISEMPTY
        END FUNCTION ZR_ISEMPTY

        FUNCTION CC_ISEMPTY(X)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X
            LOGICAL ::  CC_ISEMPTY
        END FUNCTION CC_ISEMPTY

        FUNCTION ZC_ISEMPTY(X)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X
            LOGICAL ::  ZC_ISEMPTY
        END FUNCTION ZC_ISEMPTY

    END INTERFACE

    !--------------------------------------------------------------------------!

    INTERFACE DIST

        FUNCTION S_DIST(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            REAL(4) ::  S_DIST
        END FUNCTION S_DIST

        FUNCTION D_DIST(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            REAL(8) ::  D_DIST
        END FUNCTION D_DIST

        FUNCTION CR_DIST(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X, Y
            REAL(4) ::  CR_DIST
        END FUNCTION CR_DIST

        FUNCTION ZR_DIST(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X, Y
            REAL(8) ::  ZR_DIST
        END FUNCTION ZR_DIST

        FUNCTION CC_DIST(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X, Y
            REAL(4) ::  CC_DIST
        END FUNCTION CC_DIST

        FUNCTION ZC_DIST(X,Y)
            TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X, Y
            REAL(8) ::  ZC_DIST
        END FUNCTION ZC_DIST

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.SB.)

        FUNCTION S_SB(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_SB
        END FUNCTION S_SB

        FUNCTION D_SB(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_SB
        END FUNCTION D_SB

        FUNCTION CR_SB(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  CR_SB
        END FUNCTION CR_SB

        FUNCTION ZR_SB(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  ZR_SB
        END FUNCTION ZR_SB

        FUNCTION CC_SB(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  CC_SB
        END FUNCTION CC_SB

        FUNCTION ZC_SB(X,Y)
             TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  ZC_SB
        END FUNCTION ZC_SB

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.SP.)

        FUNCTION S_SP(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_SP
        END FUNCTION S_SP

        FUNCTION D_SP(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_SP
        END FUNCTION D_SP

        FUNCTION CR_SP(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE CR_INTERVAL
                TYPE(S_INTERVAL) ::  RE, IM
            END TYPE CR_INTERVAL
            TYPE(CR_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  CR_SP
        END FUNCTION CR_SP

        FUNCTION ZR_SP(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE ZR_INTERVAL
                TYPE(D_INTERVAL) ::  RE, IM
            END TYPE ZR_INTERVAL
            TYPE(ZR_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  ZR_SP
        END FUNCTION ZR_SP

        FUNCTION CC_SP(X,Y)
             TYPE CC_INTERVAL
                COMPLEX(4) ::  MID
                REAL(4)    ::  RAD
            END TYPE CC_INTERVAL
            TYPE(CC_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  CC_SP
        END FUNCTION CC_SP

        FUNCTION ZC_SP(X,Y)
             TYPE ZC_INTERVAL
                COMPLEX(8) ::  MID
                REAL(8)    ::  RAD
            END TYPE ZC_INTERVAL
            TYPE(ZC_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  ZC_SP
        END FUNCTION ZC_SP

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.PSB.)

        FUNCTION S_PSB(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_PSB
        END FUNCTION S_PSB

        FUNCTION D_PSB(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_PSB
        END FUNCTION D_PSB

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.PSP.)

        FUNCTION S_PSP(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_PSP
        END FUNCTION S_PSP

        FUNCTION D_PSP(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_PSP
        END FUNCTION D_PSP

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.IN.)

        FUNCTION S_IN(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            REAL(4), INTENT(IN) ::  X
            TYPE(S_INTERVAL), INTENT(IN) ::  Y
            LOGICAL ::  S_IN
        END FUNCTION S_IN

        FUNCTION D_IN(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            REAL(8), INTENT(IN) ::  X
            TYPE(D_INTERVAL), INTENT(IN) ::  Y
            LOGICAL ::  D_IN
        END FUNCTION D_IN

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.EQ.)

        FUNCTION S_EQ(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_EQ
        END FUNCTION S_EQ

        FUNCTION D_EQ(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_EQ
        END FUNCTION D_EQ

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.NE.)

        FUNCTION S_NE(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_NE
        END FUNCTION S_NE

        FUNCTION D_NE(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_NE
        END FUNCTION D_NE

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.ITR.)

        FUNCTION S_ITR(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_ITR
        END FUNCTION S_ITR

        FUNCTION D_ITR(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_ITR
        END FUNCTION D_ITR

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.DJ.)

        FUNCTION S_DJ(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_DJ
        END FUNCTION S_DJ

        FUNCTION D_DJ(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_DJ
        END FUNCTION D_DJ

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.LT.)

        FUNCTION S_LT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_LT
        END FUNCTION S_LT

        FUNCTION D_LT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_LT
        END FUNCTION D_LT

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.LE.)

        FUNCTION S_LE(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_LE
        END FUNCTION S_LE

        FUNCTION D_LE(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_LE
        END FUNCTION D_LE

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.GT.)

        FUNCTION S_GT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_GT
        END FUNCTION S_GT

        FUNCTION D_GT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_GT
        END FUNCTION D_GT

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.GE.)

        FUNCTION S_GE(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_GE
        END FUNCTION S_GE

        FUNCTION D_GE(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_GE
        END FUNCTION D_GE

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.WLT.)

        FUNCTION S_WLT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_WLT
        END FUNCTION S_WLT

        FUNCTION D_WLT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_WLT
        END FUNCTION D_WLT

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.WLE.)

        FUNCTION S_WLE(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_WLE
        END FUNCTION S_WLE

        FUNCTION D_WLE(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_WLE
        END FUNCTION D_WLE

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.WGT.)

        FUNCTION S_WGT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_WGT
        END FUNCTION S_WGT

        FUNCTION D_WGT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_WGT
        END FUNCTION D_WGT

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.WGE.)

        FUNCTION S_WGE(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_WGE
        END FUNCTION S_WGE

        FUNCTION D_WGE(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_WGE
        END FUNCTION D_WGE

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.SLT.)

        FUNCTION S_SLT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_SLT
        END FUNCTION S_SLT

        FUNCTION D_SLT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_SLT
        END FUNCTION D_SLT

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.SLE.)

        FUNCTION S_SLE(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_SLE
        END FUNCTION S_SLE

        FUNCTION D_SLE(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_SLE
        END FUNCTION D_SLE

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.SGT.)

        FUNCTION S_SGT(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_SGT
        END FUNCTION S_SGT

        FUNCTION D_SGT(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_SGT
        END FUNCTION D_SGT

    END INTERFACE


    !--------------------------------------------------------------------------!


    INTERFACE OPERATOR (.SGE.)

        FUNCTION S_SGE(X,Y)
            TYPE S_INTERVAL
                REAL(4) ::  INF, SUP
            END TYPE S_INTERVAL
            TYPE(S_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  S_SGE
        END FUNCTION S_SGE

        FUNCTION D_SGE(X,Y)
            TYPE D_INTERVAL
                REAL(8) ::  INF, SUP
            END TYPE D_INTERVAL
            TYPE(D_INTERVAL), INTENT(IN) ::  X, Y
            LOGICAL ::  D_SGE
        END FUNCTION D_SGE

    END INTERFACE


    !--------------------------------------------------------------------------!


END MODULE INTERVAL_ARITHMETIC


!==============================================================================!

