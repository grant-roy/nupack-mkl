!*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 2007-2008 Intel Corporation. All Rights Reserved.
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
!          MKL TT implementation through r2r FFTW interface (via wrappers) example
!          program (Fortran-interface)
!
!   Real-to-real 1D transform of type RODFT11 (staggered2 sin transform)
!   for double precision data
!******************************************************************************/

      PROGRAM R2R_1D_RODFT11_DOUBLE_EX

      INCLUDE 'fftw3.f'
      INCLUDE 'mkl_fftw_examples.fi'

      INTEGER N,OUT_N,I,J
      PARAMETER (N=15)
      INTEGER*8 FFTW_PLAN

      REAL*8 IN(N),EXP_X(N)
      REAL*8 OUT(N)
      REAL*8 ERR,SCALE
!
!     Initialize IN and copy to expected EXP_X
!
      PRINT*,' Initialize data array'
      DO I=1,N
         IN(I)=DSIN(DFLOAT(I))+DCOS(DFLOAT(I))
         EXP_X(I)=IN(I)
      END DO

!
!     Create FFTW plan for 1D real-to-real (staggered2 sine) transform
!
      PRINT*,' Create FFTW plan for 1D r2r (staggered2 sine) transform'
      CALL DFFTW_PLAN_R2R_1D(FFTW_PLAN,N,IN,OUT,FFTW_RODFT11)

!
!     Compute 1D real-to-real (staggered2 sine) transform
!
      PRINT*,' Compute 1D r2r (staggered2 sine) transform'
      CALL DFFTW_EXECUTE(FFTW_PLAN)

!
!     Destroy FFTW plan
!
      PRINT*,' Destroy FFTW plan'
      CALL DFFTW_DESTROY_PLAN(FFTW_PLAN)

!
!     Create FFTW plan for 1D real-to-real (staggered2 sine) transform
!
      PRINT*,' Create FFTW plan for 1D r2r (staggered2 sine) transform'
      CALL DFFTW_PLAN_R2R_1D(FFTW_PLAN,N,OUT,IN,FFTW_RODFT11)

!
!     Compute 1D real-to-real (staggered2 sine) transform
!
      PRINT*,' Compute 1D r2r (staggered2 sine) transform'
      CALL DFFTW_EXECUTE(FFTW_PLAN)

!
!     Destroy FFTW plan
!
      PRINT*,' Destroy FFTW plan'
      CALL DFFTW_DESTROY_PLAN(FFTW_PLAN)

!
!     Scale result. FFTW can't do this itself.
!
      PRINT*,' Scale result by 0.5/N'
      SCALE=0.5D0/N
      DO I=1,N
         IN(I)=SCALE*IN(I)
      END DO

!
!     Check results
!
      PRINT*,' Check results'
      CALL CHECK_RESULT_D(IN,EXP_X,N,ERR)
      PRINT*,' Accuracy=',ERR
      IF (ERR.GT.MAX_DOUBLE_ERR) THEN
       PRINT*,' TEST FAILED'
      ELSE
       PRINT*,' TEST PASSED'
      END IF


      END PROGRAM
