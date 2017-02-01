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
!    vslNewStreamEx  Example Program Text
!*******************************************************************************

      include 'mkl_vsl.fi'
      include "errcheck.inc"

      program MKL_VSL_TEST

      USE MKL_VSL_TYPE
      USE MKL_VSL

      TYPE (VSL_STREAM_STATE) :: stream
      TYPE (VSL_STREAM_STATE) :: streamEx
      integer(kind=4) seedEx(6)
      integer(kind=4) r(1000)
      integer(kind=4) rEx(1000)
      integer(kind=4) i,err,nn
      integer n,nnn
      integer brng,method,seed
      integer(kind=4) errcode

      err=0
      n=1000
      nnn=6
      nn=10
      brng=VSL_BRNG_MRG32K3A
      method=0

!     ***** Initialize seeds *****
      seed=7777777
      seedEx(1)=7777777
      do i=2,6
        seedEx(i)=1
      end do

!     ***** Initialize streams *****
      errcode=vslnewstream  ( stream,   brng,  seed )
      call CheckVslError(errcode)
      errcode=vslnewstreamEx( streamEx, brng,  nnn, seedEx )
      call CheckVslError(errcode)

!     ***** Call RNGs *****
      errcode=virnguniformbits( method, stream,   n, r )
      call CheckVslError(errcode)
      errcode=virnguniformbits( method, streamEx, n, rEx )
      call CheckVslError(errcode)

!     ***** Compare results *****
      do i=1,1000
        if (r(i) .NE. rEx(i)) then
          err=err+1
        end if
      end do

!     ***** Printing results *****
      print *,"Sample of vslNewStreamEx"
      print *,"------------------------"
      print *,""
      print *,"Parameters:"
      print 11,"    seed   =  ",seed
      print 12,"    seedEx = { ",seedEx(1)," ",                         &
     &         seedEx(2)," ",seedEx(3)," ",                             &
     &         seedEx(4)," ",seedEx(5)," ",seedEx(6), " }"

      print *,""
      print *,"Results (first 10 of 1000):"
      print *,"---------------------------"
      do i=1,nn
        print 10, "r[",i-1,"]=0x",r(i)," rEx[",i-1,"]=0x",rEx(i)
      end do

      print *,""
      if (err>0) then
        print 13,"Error: ", err," values are incorrect!"
        stop 1
      else
        print *,"Results of ordinary and extended NewStream functions", &
     &    " are identical."
      end if

!     ***** Deinitialize *****
      errcode=vsldeletestream( stream )
      call CheckVslError(errcode)
      errcode=vsldeletestream( streamEx )
      call CheckVslError(errcode)

10    format(A,I1,A,Z08,A,I1,A,Z08)
11    format(A,I8)
12    format(A,I7,A,I1,A,I1,A,I1,A,I1,A,I1,A)
13    format(A,I8,A)

      end
