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
!    vmlSetErrStatus/vmlGetErrStatus/vmlClearErrStatus  Example Program Text
!*******************************************************************************

      program MKL_VML_TEST

      include "mkl_vml.fi"
      include "func_interv.fi"

      integer errst
      integer(kind=4) terrst

      print *,"vmlSetErrStatus/vmlGetErrStatus/vmlClearErrStatus",      &
     &        " example program"
      print *,""
      print *,""

      errst=VMLGETERRSTATUS()

      print 10,"Initial value of vmlErrStatus: ",errst,"h"
      call PrintTextVmlErrStatus(errst)

      errst=VML_STATUS_BADMEM
      terrst=VMLSETERRSTATUS(errst)
      errst=VMLGETERRSTATUS()
      print 10,"Value of vmlErrStatus after using vmlSetErrStatus: ",   &
     &         errst,"h"
      call PrintTextVmlErrStatus(errst)

      terrst=VMLCLEARERRSTATUS()
      errst=VMLGETERRSTATUS()
      print 10,"Value of vmlErrStatus after using vmlClearErrStatus: ", &
     &         errst,"h"
      call PrintTextVmlErrStatus(errst)

10    format(A,Z,A)

      end

      subroutine PrintTextVmlErrStatus(errst)

      include "mkl_vml.fi"

      integer(kind=2) errst

      if(errst.eq.VML_STATUS_OK       ) print *,"VML_STATUS_OK       "
      if(errst.eq.VML_STATUS_BADSIZE  ) print *,"VML_STATUS_BADSIZE  "
      if(errst.eq.VML_STATUS_BADMEM   ) print *,"VML_STATUS_BADMEM   "
      if(errst.eq.VML_STATUS_ERRDOM   ) print *,"VML_STATUS_ERRDOM   "
      if(errst.eq.VML_STATUS_SING     ) print *,"VML_STATUS_SING     "
      if(errst.eq.VML_STATUS_OVERFLOW ) print *,"VML_STATUS_OVERFLOW "
      if(errst.eq.VML_STATUS_UNDERFLOW) print *,"VML_STATUS_UNDERFLOW"

      end
