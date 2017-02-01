!*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 2006-2008 Intel Corporation. All Rights Reserved.
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
!          MKL DFTI implementation through FFTW interface (via wrappers) example
!          program (Fortran-interface)
!
!   Complex-to-complex 1D transform for COMPLEX*16 data inplace with strides
!   and threads
!
!   Configuration parameters for MKL DFTI:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX             (obligatory)
!           DFTI_PRECISION      = DFTI_DOUBLE/DFTI_SINGLE  (obligatory)
!           DFTI_DIMENSION      = 1                        (obligatory)
!           DFTI_LENGTHS        = N                        (obligatory)
!           DFTI_PLACEMENT      = DFTI_OUTOFPLACE          (default)
!           DFTI_FORWARD_SCALE  = 1.0                      (default)
!           DFTI_BACKWARD_SCALE = 1.0/N                    (default=1.0)
!
!  Other default configuration parameters are in the mkl_dfti.f90 interface file
!******************************************************************************/

      PROGRAM COMPLEX_1D_DOUBLE_EX7

      INCLUDE 'fftw_f77.i'
      INCLUDE 'mkl_fftw_examples.fi'

      INTEGER SIZE,N,IDIST,ISTRIDE,HOWMANY,STATUS,NTHREADS,I
      PARAMETER (N=10)
      PARAMETER (ISTRIDE=2)
      PARAMETER (IDIST=N*ISTRIDE+3)
      PARAMETER (HOWMANY=2)
      PARAMETER (SIZE=HOWMANY*IDIST)
      PARAMETER (NTHREADS=2)
      INTEGER*8 FFTW_PLAN

      COMPLEX*16 IN(SIZE),OUT(N),EXP_X(SIZE)
      REAL*8 ERR,SCALE

!
!     Initialize IN and copy to expected EXP_X
!
      PRINT*,' Initialize data array'
      CALL INIT_COMPLEX_VECTOR_Z(IN,SIZE)
      DO I=1,SIZE
       EXP_X(I)=IN(I)
      END DO

!
!     FFTW requires user to call this routine before calling any other FFTW routines
!
      CALL FFTW_F77_THREADS_INIT(STATUS)

!
!     Create FFTW plan for 1D forward transform
!
      PRINT*,' Create FFTW plan for 1D forward transform'
      CALL FFTW_F77_CREATE_PLAN(FFTW_PLAN,N,FFTW_FORWARD,
     *FFTW_ESTIMATE+FFTW_IN_PLACE)

!
!     Compute Forward
!
      PRINT*,' Compute Forward'
      CALL FFTW_F77_THREADS(NTHREADS,FFTW_PLAN,HOWMANY,IN,ISTRIDE,IDIST,
     *OUT,0,0)

!
!     Destroy FFTW plan
!
      PRINT*,' Destroy FFTW plan'
      CALL FFTW_F77_DESTROY_PLAN(FFTW_PLAN)

!
!     Create FFTW plan for 1D backward transform
!
      PRINT*,' Create FFTW plan for 1D backward transform'
      CALL FFTW_F77_CREATE_PLAN(FFTW_PLAN,N,FFTW_BACKWARD,
     *FFTW_ESTIMATE+FFTW_IN_PLACE)

!
!     Compute Backward
!
      PRINT*,' Compute Backward'
      CALL FFTW_F77_THREADS(NTHREADS,FFTW_PLAN,HOWMANY,IN,ISTRIDE,IDIST
     *,OUT,0,0)

!
!     Destroy FFTW plan
!
      PRINT*,' Destroy FFTW plan'
      CALL FFTW_F77_DESTROY_PLAN(FFTW_PLAN)

!
!     Scale result. FFTW can't do this itself.
!
      PRINT*,' Scale result by 1/N'
      SCALE=1.0D0/N
      CALL SCALE_WITH_STRIDES_Z(IN,SCALE,HOWMANY,N,ISTRIDE,IDIST)

!
!     Check results
!
      PRINT*,' Check results'
      CALL CHECK_RESULT_Z(IN,EXP_X,SIZE,ERR)
      PRINT*,' Accuracy=',ERR
      IF (ERR.GT.MAX_DOUBLE_ERR) THEN
       PRINT*,' TEST FAIL'
       GOTO 100
      END IF
      PRINT*,' TEST PASSED'

100   CONTINUE

      END PROGRAM
