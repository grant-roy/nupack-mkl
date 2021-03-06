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
!    vzAsin  Example Program Text
!*******************************************************************************

      program MKL_VML_TEST

      include "mkl_vml.fi"
      include "func_interv.fi"

      complex(kind=8) zA(VEC_LEN)
      complex(kind=8) zB1(VEC_LEN)
      integer i

      do i=1,VEC_LEN
            zA(i)=CMPLX(VML_DASIN_BEG+((VML_DASIN_END-VML_DASIN_BEG)*i) &
     &      /VEC_LEN,VML_DASIN_END-((VML_DASIN_END-VML_DASIN_BEG)*i)/   &
     &      VEC_LEN)
            zB1(i)=0.0
      end do

      call VZASIN(VEC_LEN,zA,zB1)

      print *,"vzAsin test/example program"
      print *,""
      print *,""
      print *,"           Argument                     vzAsin"
      print *,"======================================================", &
     &        "========================"
      do i=1,VEC_LEN
            print 10,zA(i),"    ",zB1(i)
      end do

10    format(F15.7,F15.7,A5,F15.7,F15.7)

      end
