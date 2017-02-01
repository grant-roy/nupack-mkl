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
!    vslLeapfrogStream  Example Program Text
!*******************************************************************************

      include 'mkl_vsl.fi'
      include "errcheck.inc"

      program MKL_VSL_TEST

      USE MKL_VSL_TYPE
      USE MKL_VSL

      TYPE NEW_TYPE
          TYPE (VSL_STREAM_STATE):: s1
      END TYPE NEW_TYPE

      TYPE (NEW_TYPE):: stream
      TYPE (NEW_TYPE):: streamL(10)
      integer(kind=4) r(1000)
      integer(kind=4) rL(1000)
      integer(kind=4) i,j,k,err,s
      integer n,ns
      integer brng,method,seed
      integer(kind=4) errcode

      err=0
      n=1000
      s=10
      ns=100
      brng=VSL_BRNG_MCG31
      method=0
      seed=7777777

!     ***** Create main stream *****
      errcode=vslnewstream  ( stream%s1,   brng,  seed )
      call CheckVslError(errcode)

!     ***** Create leapfrog streams as copies of the main one *****
      do i=1,10
        errcode=vslcopystream( streamL(i)%s1,stream%s1 )
        call CheckVslError(errcode)
        k=i-1
        errcode=vslleapfrogstream( streamL(i)%s1, k, s )
        call CheckVslError(errcode)
      end do

!     ***** Generate random numbers for main stream *****
      errcode=virnguniformbits( method, stream%s1, n, r )
      call CheckVslError(errcode)
      do i=1,10
        errcode=virnguniformbits( method, streamL(i)%s1, ns,            &
     &                            rL((i-1)*ns+1) )
        call CheckVslError(errcode)
      end do

!     ***** Compare results *****
      j = 1
      do i=1,100
        do k=1,10
          if (r(j) .NE. rL((k-1)*ns+i)) then
            err=err+1
          end if
          j=j+1
        end do
      end do

!     ***** Printing results *****
      print *,"Sample of vslLeapfrogStream"
      print *,"---------------------------"
      print *,""
      print *,"Parameters:"
      print 11,"    seed   =  ",seed
      print *,""
      print *,"Results (first 10 of 1000):"
      print *,"---------------------------"
      do i=1,10
        print 10, "r[",i-1,"]=0x",r(i)," rL[",(i-1)*ns,"]=0x",          &
     &             rL((i-1)*ns+1)
      end do

      print *,""
      if (err>0) then
        print 13,"Error: ", err," values are incorrect!"
        stop 1
      else
        print *,"Results of ordinary and Leapfrog streams are identical"
      end if

!     ***** Deinitialize *****
      errcode=vsldeletestream( stream%s1 )
      call CheckVslError(errcode)
      do i=1,10
        errcode=vsldeletestream( streamL(i)%s1 )
        call CheckVslError(errcode)
      end do

10    format(A,I1,A,Z08,A,I3,A,Z08)
11    format(A,I8)
12    format(A,I7,A,I1,A,I1,A,I1,A,I1,A,I1,A)
13    format(A,I8,A)

      end
