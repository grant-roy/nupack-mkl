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
!    vsPackM  Example Program Text
!*******************************************************************************

      program MKL_VML_TEST

      include "mkl_vml.fi"
      include "func_interv.fi"

      real(kind=4) sA(VEC_LEN)
      real(kind=4) sB1(VEC_LEN)
      integer ma(VEC_LEN)
      integer i

      i=0

      do i=1,VEC_LEN
            sA(i)=i/1.0
            sB1(i)=0.0
            ma(i)=iand(i,1)
      end do

      call VSPACKM(VEC_LEN,sA,ma,sB1)

      print *,"vsPackM test/example program"
      print *,""
      print *,""

      print *,"           Before packing             After packing"
      print *,"======================================================", &
     &        "========================="
      do i=1,VEC_LEN
            print 10,sA(i),sB1(i)
      end do

10    format (F25.13,F25.13)

      end
