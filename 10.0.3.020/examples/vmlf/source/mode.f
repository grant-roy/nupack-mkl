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
!    vmlSetMode/vmlGetMode  Example Program Text
!*******************************************************************************

      program MKL_VML_TEST

      include "mkl_vml.fi"
      include "func_interv.fi"

      integer mode
      integer(kind=4) tmode

      mode=0

      print *,"vmlSetMode/vmlGetMode example program"
      print *,""
      print *,""

      mode=VMLGETMODE()
      print 10,"Default value of vmlMode: ",mode,"h"
      call PrintTextVmlMode(mode)

      mode=jior(VML_LA,jior(VML_FLOAT_CONSISTENT,VML_ERRMODE_IGNORE))
      tmode=VMLSETMODE(mode)
      mode=VMLGETMODE()
      print 10,"Value of vmlMode after using vmlSetMode: ",mode,"h"
      call PrintTextVmlMode(mode)

10    format(A,Z,A)

      end


      subroutine PrintTextVmlMode(mode)

      include "mkl_vml.fi"

      integer(kind=4) mode
      integer(kind=4) mask

      mask=VML_ACCURACY_MASK
      if(jiand(mode,mask).eq.1) print *,"VML_LA"
      if(jiand(mode,mask).eq.2) print *,"VML_HA"

      mask=VML_FPUMODE_MASK
      if(jiand(mode,mask).eq.0) print *,"VML_DEFAULT_PRECISION"
      if(jiand(mode,mask).eq.16) print *,"VML_FLOAT_CONSISTENT "
      if(jiand(mode,mask).eq.32) print *,"VML_DOUBLE_CONSISTENT"
      if(jiand(mode,mask).eq.48) print *,"VML_RESTORE"

      mask=VML_ERRMODE_MASK
      if(jiand(mode,VML_ERRMODE_IGNORE).eq.VML_ERRMODE_IGNORE)          &
     &  print *,"VML_ERRMODE_IGNORE"
      if(jiand(mode,VML_ERRMODE_ERRNO).eq.VML_ERRMODE_ERRNO)            &
     &  print *,"VML_ERRMODE_ERRNO"
      if(jiand(mode,VML_ERRMODE_STDERR).eq.VML_ERRMODE_STDERR)          &
     &  print *,"VML_ERRMODE_STDERR"
      if(jiand(mode,VML_ERRMODE_EXCEPT).eq.VML_ERRMODE_EXCEPT)          &
     &  print *,"VML_ERRMODE_EXCEPT"
      if(jiand(mode,VML_ERRMODE_CALLBACK).eq.VML_ERRMODE_CALLBACK)      &
     &  print *,"VML_ERRMODE_CALLBACK"

      end
