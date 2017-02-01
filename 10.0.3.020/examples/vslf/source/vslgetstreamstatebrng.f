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
!    vslGetStreamStateBrng  Example Program Text
!*******************************************************************************

      include 'mkl_vsl.fi'
      include "errcheck.inc"

      program MKL_VSL_TEST

      USE MKL_VSL_TYPE
      USE MKL_VSL

      TYPE (VSL_STREAM_STATE) :: stream
      integer seed,brngExp
      integer(kind=4) brngObt
      integer(kind=4) errcode

      brngExp=VSL_BRNG_WH+127
      brngObt=0

      seed=7777777

!     ***** Initialize stream *****
      errcode=vslnewstream  ( stream, brngExp, seed )
      call CheckVslError(errcode)

      brngObt = vslgetstreamstatebrng( stream )

!     ***** Printing results *****
      print *,"Sample of vslGetStreamStateBrng"
      print *,"-------------------------------"
      print *,""
      print *,"Parameters:"
      print 11,"    seed =   ",seed
      print 11,"    brng =   ",brngExp

      print *,""
      if (brngObt.NE.brngExp) then
        print 13,"Error: returned value ", brngObt," is incorrect       &
     &            (expected ",brngExp," )!"
        stop 1
      else
        print 14,"Returned ", brngObt," as expected."
      end if

!     ***** Deinitialize *****
      errcode=vsldeletestream( stream )
      call CheckVslError(errcode)

10    format(A,I1,A,Z08,A,I1,A,Z08)
11    format(A,I)
12    format(A,I7,A,I7,A,I7,A,I7,A,I7,A,I7,A)
13    format(A,I,A,I2,A)
14    format(A,I,A)

      end
