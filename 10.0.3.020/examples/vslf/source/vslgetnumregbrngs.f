!*******************************************************************************
!                             INTEL CONFIDENTIAL
!  Copyright(C) 2003-2008 Intel Corporation. All Rights Reserved.
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
!    vslGetNumRegBrngs  Example Program Text
!*******************************************************************************

      include 'mkl_vsl.fi'
      include "errcheck.inc"

      program MKL_VSL_TEST

      USE MKL_VSL_TYPE
      USE MKL_VSL

      TYPE (VSL_STREAM_STATE) :: stream
      integer(kind=4) brngsObt,brngsExp
      integer(kind=4) errcode

      brngsExp=12
      brngsObt=0

      brngsObt = vslgetnumregbrngs()

!     ***** Printing results *****
      print *,"Sample of vslGetNumRegBrngs"
      print *,"-------------------------------"
      print *,""
      if (brngsObt.NE.brngsExp) then
        print 13,"Error: returned value ", brngsObt," is incorrect      &
     &           (expected ",brngsExp," )!"
        stop 1
      else
        print 14,"Returned ", brngsObt," as expected."
      end if

13    format(A,I3,A,I3,A)
14    format(A,I3,A)

      end
