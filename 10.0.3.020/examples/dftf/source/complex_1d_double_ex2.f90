!*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 2003-2008 Intel Corporation. All Rights Reserved.
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
!       MKL DFTI interface example program (Fortran-interface)
!       Forward-Backward 1D complex transform for double precision data
!       NOT_INPLECE mode
!
!*******************************************************************************
!   Configuration parameters:
!
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX          (obligatory)
!           DFTI_PRECISION      = DFTI_DOUBLE           (obligatory)
!           DFTI_DIMENSION      = 1                     (obligatory)
!           DFTI_LENGTHS        = n                     (obligatory)
!           DFTI_PLACEMENT      = DFTI_NOT_INPLACE      (default = DFTI_INPLACE)
!           DFTI_FORWARD_SCALE  = 1.0                   (default)
!           DFTI_BACKWARD_SCALE = 1.0/real(n, KIND=8)   (default=1.0)
!
!   Other default configuration parameters are in the mkl_dfti.f90 interface file
!*******************************************************************************

      Program COMPLEX_1D_DOUBLE_EX2

      Use MKL_DFTI
      include 'mkl_dfti_examples.fi'

      integer    n

      complex(8) :: X_IN (M_MAX)
      complex(8) :: X_OUT(M_MAX)
      complex(8) :: X_EXP(M_MAX)

      type(DFTI_DESCRIPTOR), POINTER :: Desc_Handle
      integer   status
      real(8)   Scale

      real(8)   maxerr
      real(8)   eps
      parameter (eps=DOUBLE_EPS)

!
!     Read input n - size of transform from input file
!
      read*
      read*, n

      if(LEGEND_PRINT) then
          print*, 'COMPLEX_1D_DOUBLE_EX2'
          print*, 'Forward-Backward 1D complex transform for double precision data'
          print *
          print *, 'Configuration parameters:'
          print *
          print *, 'DFTI_FORWARD_DOMAIN = DFTI_COMPLEX'
          print *, 'DFTI_PRECISION      = DFTI_DOUBLE '
          print *, 'DFTI_DIMENSION      =   1'
          print 903, n
          print *, 'DFTI_PLACEMENT      = DFTI_NOT_INPLACE'
          print *, 'DFTI_FORWARD_SCALE  = 1.0 '
          print *, 'DFTI_BACKWARD_SCALE = 1.0/double(n)'
          print *
      endif

!
!     Check input parameters
!
      if(n .gt. M_MAX) then
          print*, ' Error input parameters n > M_MAX'
          print*, ' Please see mkl_dfti_examples.fi file'
          print*, ' TEST FAIL'
          goto 101
      endif

!
!     initialize X_IN and copy to expected X_EXP
!
      call INIT_COMPLEX_VECTOR_Z(X_IN, n)
      call ZCOPY(n, X_IN, 1, X_EXP, 1)

      if(ADVANCED_DATA_PRINT) then
        print *
        print*,'INPUT vector X '
        call PRINT_VECTOR_Z(X_IN, n)
      endif

!
!     Create Dfti descriptor for 1D double precision  transform
!

      Status = DftiCreateDescriptor( Desc_Handle, DFTI_DOUBLE, &
      DFTI_COMPLEX, 1, n )

      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
          call dfti_example_status_print(Status)
          goto 101
      end if

!
!     Set placement of result DFTI_NOT_INPLACE
!
      Status = DftiSetValue( Desc_Handle, DFTI_PLACEMENT, &
                                            DFTI_NOT_INPLACE)
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
          call dfti_example_status_print(Status)
          goto 100
      end if

!     Commit Dfti descriptor
!
!
      Status = DftiCommitDescriptor( Desc_Handle )
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
          call dfti_example_status_print(Status)
          goto 100
      end if

!
!     Compute Forward transform
!
      print *
      print*,'Compute DftiComputeForward'
      Status = DftiComputeForward( Desc_Handle, X_IN, X_OUT)
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR) ) then
          call dfti_example_status_print(Status)
          goto 100
      end if

      if(ADVANCED_DATA_PRINT) then
        print *
        print*,'Forward OUTPUT vector X '
        call PRINT_VECTOR_Z( X_OUT, n)
      endif

!
!     Set Scale number for Backward transform
!
      Scale = 1.0/real(n, KIND=8)

      Status = DftiSetValue(Desc_Handle, DFTI_BACKWARD_SCALE, Scale)
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
          call dfti_example_status_print(Status)
          goto 100
      end if

!
!     Commit Dfti descriptor
!
      Status = DftiCommitDescriptor( Desc_Handle )
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
          call dfti_example_status_print(Status)
          goto 100
      end if

!
!     Compute Backward transform
!
      print *
      print*,'Compute DftiComputeBackward'
      Status = DftiComputeBackward( Desc_Handle, X_OUT, X_IN)
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR) ) then
          call dfti_example_status_print(Status)
          goto 100
      end if

      if(ADVANCED_DATA_PRINT) then
        print *
        print*,'Backward OUTPUT vector X '
        call PRINT_VECTOR_Z( X_IN, n)
      endif

!
!     Check result
!
      maxerr = CHECK_RESULT_Z(X_IN, X_EXP, n)
      if(ACCURACY_PRINT) then
        print *
        print 904,  maxerr
      endif

      if(maxerr .LT.eps) then
        print*,'TEST PASSED'
      else
        print*,'TEST FAIL '
      endif

 100  continue
      Status = DftiFreeDescriptor(Desc_Handle)
      if (.not. DftiErrorClass(Status, DFTI_NO_ERROR) ) then
          call dfti_example_status_print(Status)
      end if

 903  format(' DFTI_LENGTHS        =', I4)
 904  format(' ACCURACY = ', G15.6)
 101  continue
      print *
      end


