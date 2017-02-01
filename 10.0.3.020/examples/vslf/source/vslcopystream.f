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
!    vslCopyStream  Example Program Text
!*******************************************************************************

      include 'mkl_vsl.fi'
      include "errcheck.inc"

      program MKL_VSL_TEST

      USE MKL_VSL_TYPE
      USE MKL_VSL

      TYPE (VSL_STREAM_STATE) :: stream
      TYPE (VSL_STREAM_STATE) :: streamCpy
      integer(kind=4) r(1000)
      integer(kind=4) rCpy(1000)
      integer(kind=4) i,err,nn
      integer n
      integer brng,method,seed
      integer(kind=4) errcode

      err=0
      n=1000
      nn=10
      brng=VSL_BRNG_MCG31
      method=0

!     ***** Initialize seeds *****
      seed=7373737

!     ***** Initialize streams *****
      errcode=vslnewstream  ( stream,   brng,  seed )
      call CheckVslError(errcode)
      errcode=vslcopystream ( streamCpy, stream )
      call CheckVslError(errcode)

!     ***** Call RNGs *****
      errcode=virnguniformbits( method, stream,   n, r )
      call CheckVslError(errcode)
      errcode=virnguniformbits( method, streamCpy, n, rCpy )
      call CheckVslError(errcode)

!     ***** Compare results *****
      do i=1,1000
        if (r(i) .NE. rCpy(i)) then
          err=err+1
        end if
      end do

!     ***** Printing results *****
      print *,"Sample of vslCopyStream"
      print *,"------------------------"
      print *,""
      print *,"Parameters:"
      print 11,"    seed   =  ",seed

      print *,""
      print *,"Results (first 10 of 1000):"
      print *,"---------------------------"
      do i=1,nn
        print 10, "r[",i-1,"]=0x",r(i)," rCpy[",i-1,"]=0x",rCpy(i)
      end do

      print *,""
      if (err>0) then
        print 13,"Error: ", err," values are incorrect!"
        stop 1
      else
        print *,"Results of original stream and its copy are identical."
      end if

!     ***** Deinitialize *****
      errcode=vsldeletestream( stream )
      call CheckVslError(errcode)
      errcode=vsldeletestream( streamCpy )
      call CheckVslError(errcode)

10    format(A,I1,A,Z08,A,I1,A,Z08)
11    format(A,I8)
13    format(A,I8,A)

      end
