********************************************************************************
*                              INTEL CONFIDENTIAL
*   Copyright(C) 2004-2008 Intel Corporation. All Rights Reserved.
*   The source code contained  or  described herein and all documents related to
*   the source code ("Material") are owned by Intel Corporation or its suppliers
*   or licensors.  Title to the  Material remains with  Intel Corporation or its
*   suppliers and licensors. The Material contains trade secrets and proprietary
*   and  confidential  information of  Intel or its suppliers and licensors. The
*   Material  is  protected  by  worldwide  copyright  and trade secret laws and
*   treaty  provisions. No part of the Material may be used, copied, reproduced,
*   modified, published, uploaded, posted, transmitted, distributed or disclosed
*   in any way without Intel's prior express written permission.
*   No license  under any  patent, copyright, trade secret or other intellectual
*   property right is granted to or conferred upon you by disclosure or delivery
*   of the Materials,  either expressly, by implication, inducement, estoppel or
*   otherwise.  Any  license  under  such  intellectual property  rights must be
*   express and approved by Intel in writing.
*
********************************************************************************
*   Content : MKL DSS Fortran-77 example
*
********************************************************************************
C---------------------------------------------------------------------------
C Example program for solving symmetric positive definite system of
C equations.
C---------------------------------------------------------------------------
      PROGRAM solver_f77_test
      IMPLICIT NONE
      INCLUDE 'mkl_dss.f77'
C---------------------------------------------------------------------------
C Define the array and rhs vectors
C---------------------------------------------------------------------------
      INTEGER nRows, nCols, nNonZeros, i, nRhs
      PARAMETER (nRows = 5,
     1 nCols = 5,
     2 nNonZeros = 9,
     3 nRhs = 1)
      INTEGER rowIndex(nRows + 1), columns(nNonZeros)
      DOUBLE PRECISION values(nNonZeros), rhs(nRows)
      DATA rowIndex / 1, 6, 7, 8, 9, 10 /
      DATA columns / 1, 2, 3, 4, 5, 2, 3, 4, 5 /
      DATA values / 9, 1.5, 6, .75, 3, 0.5, 12, .625, 16 /
      DATA rhs / 1, 2, 3, 4, 5 /
C---------------------------------------------------------------------------
C Allocate storage for the solver handle and the solution vector
C---------------------------------------------------------------------------
      DOUBLE PRECISION solution(nRows)
      INTEGER*8 handle
      INTEGER error
      CHARACTER*15 statIn
      DOUBLE PRECISION statOut(5)
      INTEGER bufLen
      PARAMETER(bufLen = 20)
      INTEGER buff(bufLen)
C---------------------------------------------------------------------------
C Initialize the solver
C---------------------------------------------------------------------------
      error = dss_create(handle, MKL_DSS_DEFAULTS)
      IF (error .NE. MKL_DSS_SUCCESS ) GOTO 999
C---------------------------------------------------------------------------
C Define the non-zero structure of the matrix
C---------------------------------------------------------------------------
      error = dss_define_structure( handle, MKL_DSS_SYMMETRIC,
     & rowIndex, nRows, nCols, columns, nNonZeros )
      IF (error .NE. MKL_DSS_SUCCESS ) GOTO 999
C---------------------------------------------------------------------------
C Reorder the matrix
C---------------------------------------------------------------------------
      error = dss_reorder( handle, MKL_DSS_DEFAULTS, 0)
      IF (error .NE. MKL_DSS_SUCCESS ) GOTO 999
C---------------------------------------------------------------------------
C Factor the matrix
C---------------------------------------------------------------------------
      error = dss_factor_real( handle,
     & MKL_DSS_DEFAULTS, VALUES)
      IF (error .NE. MKL_DSS_SUCCESS ) GOTO 999
C---------------------------------------------------------------------------
C Get the solution vector
C---------------------------------------------------------------------------
      error = dss_solve_real( handle, MKL_DSS_DEFAULTS,
     & rhs, nRhs, solution)
      IF (error .NE. MKL_DSS_SUCCESS ) GOTO 999
C---------------------------------------------------------------------------
C Print Determinant of the matrix (no statistics for a diagonal matrix)
C---------------------------------------------------------------------------
      IF( nRows .LT. nNonZeros ) THEN
         statIn = 'determinant'
         call mkl_cvt_to_null_terminated_str(buff,bufLen,statIn)
         error = dss_statistics(handle, MKL_DSS_DEFAULTS,
     &                          buff,statOut)
         WRITE(*,"(' pow of determinant is ', 5(F10.3))") statOut(1)
         WRITE(*,"(' base of determinant is ', 5(F10.3))") statOut(2)
         WRITE(*,"(' Determinant is ', 5(F10.3))")(10**statOut(1))*
     &         statOut(2)
      END IF
C---------------------------------------------------------------------------
C Deallocate solver storage
C---------------------------------------------------------------------------
      error = dss_delete( handle, MKL_DSS_DEFAULTS )
      IF (error .NE. MKL_DSS_SUCCESS ) GOTO 999
C---------------------------------------------------------------------------
C Print solution vector
C---------------------------------------------------------------------------
      WRITE(*,900) (solution(i), i = 1, nCols)
  900 FORMAT(' Solution Array: ',5(F10.3))
      GOTO 1000
  999 WRITE(*,*) "Solver returned error code ", error
 1000 END
