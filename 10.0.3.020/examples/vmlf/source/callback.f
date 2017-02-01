!*******************************************************************************
!                              INTEL CONFIDENTIAL
!  Copyright(C) 2001-2008 Intel Corporation. All Rights Reserved.
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
!*******************************************************************************
!  Content:
!    vmlSet/Get/ClearErrorCallBack  Example Program Text
!*******************************************************************************

      program MKL_VML_TEST
      include "mkl_vml.fi"

      INTEGER errcb,terrcb

!     User callback function should be external in the program
      INTEGER,EXTERNAL :: cb

!     Arguments for vdLn function
      real(kind=8) :: a(1), r(1)
      integer      :: n

      print *,"Set/Get/Clear CallBack example program"
      print *,""

!     Testing vmlGetErrorCallback
      errcb=0
      errcb=VMLGETERRORCALLBACK()
      print 10,"Get CallBack returned address:",errcb,"h"

!     Registering user error callback function
      terrcb=VMLSETERRORCALLBACK(cb)
      errcb=VMLGETERRORCALLBACK()
      print 10,"User callback function address:",errcb,"h"
      print *,""
      print *,"Calling vdLn function..."

!     Call vdLn with invalid argument to test user callback function
      a(1) = 0.0
      n = 1
      call VDLN(n,a,r)

!     Testing vmlClearErrorCallback
      terrcb=VMLCLEARERRORCALLBACK()
      errcb=VMLGETERRORCALLBACK()
      print *,""
      print 10,"Callback address after calling Clear CallBack:",        &
     &          errcb,"h"

10    format (A,Z8.8,A)

      end program MKL_VML_TEST

      integer function cb( es )
      include "mkl_vml.fi"

      type(ERROR_STRUCTURE) :: es

      print 20,"In function vdLn argument a[",es%iindex,"]=",es%dba1,   &
     &         " is wrong."
      cb = 0

20    format (A,I1,A,F3.1,A)

      end function
