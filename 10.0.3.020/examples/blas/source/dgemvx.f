!*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 1999-2008 Intel Corporation. All Rights Reserved.
!   The source code contained  or  described herein and all documents related to
!   the source code ("Material") are owned by Intel Corporation or its suppliers
!   or licensors.  Title to the  Material remains with  Intel Corporation or its
!   suppliers and licensors. The Material contains trade secrets and proprietary
!   and  confidential  information of  Intel or its suppliers and licensors. The
!   Material  is  protected  by  worldwide  copyright  and trade secret laws and
!   treaty  provisions. No part of the Material may be used, copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way without Intel's prior express written permission.
!   No license  under any  patent, copyright, trade secret or other intellectual
!   property right is granted to or conferred upon you by disclosure or delivery
!   of the Materials,  either expressly, by implication, inducement, estoppel or
!   otherwise.  Any  license  under  such  intellectual property  rights must be
!   express and approved by Intel in writing.
!
!*******************************************************************************
!  Content:
!      D G E M V  Example Program Text
!*******************************************************************************

      program   DGEMV_MAIN
*
      character*1      trans
      integer          m, n, lda, incx, incy
      integer          mmax, nmax, xmax, ymax
      parameter        (mmax=4, nmax=5, xmax=5, ymax=5)
      parameter        (lda=mmax)
      double precision alpha, beta
      double precision a(mmax,nmax), x(xmax), y(ymax)
      integer          i, j
*       Intrinsic Functions
      intrinsic        abs
*       External Subroutines
      external         DGEMV, PrintVectorD, PrintArrayD
*
*       Executable Statements
*
      print*
      print*, '   D G E M V  EXAMPLE PROGRAM'
*
*       Read input data from input file
      read*
      read*, m, n
      read*, incx, incy
      read*, alpha, beta
      read 100, trans
      if ((trans.eq.'N').or.(trans.eq.'n')) then
        if ( ((1+(n-1)*abs(incx)).gt.xmax).or.
     $       ((1+(m-1)*abs(incy)).gt.ymax) ) then
          print*, ' 1 Insufficient memory for arrays'
          goto 99
        end if
        read*,(x(i),i=1, 1+(n-1)*abs(incx))
        read*,(y(i),i=1, 1+(m-1)*abs(incy))
      else
        if ( ((1+(m-1)*abs(incx)).gt.xmax).or.
     $       ((1+(n-1)*abs(incy)).gt.ymax) ) then
          print*, ' 2 Insufficient memory for arrays'
          goto 99
        end if
        read*,(x(i),i=1, 1+(m-1)*abs(incx))
        read*,(y(i),i=1, 1+(n-1)*abs(incy))
      end if
      if (m.gt.mmax.or.n.gt.nmax) then
        print*, ' 3 Insufficient memory for arrays'
        goto 99
      end if
      read*, ((a(i,j),j=1,n),i=1,m)
*
*       Print input data
      print*
      print*, '     INPUT DATA'
      print 101, m, n
      print 102, alpha, beta
      print 103, trans
      if ((trans.eq.'N').or.(trans.eq.'n')) then
        call PrintVectorD(0,n,x,incx,'X ')
        call PrintVectorD(0,m,y,incy,'Y ')
      else
        call PrintVectorD(0,m,x,incx,'X ')
        call PrintVectorD(0,n,y,incy,'Y ')
      end if
      call PrintArrayD(0,0,m,n,a,lda,'A')
*
*      Call DGEMV subroutine
      call DGEMV(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
*
      print*
      print*, '     OUTPUT DATA'
      if ((trans.eq.'N').or.(trans.eq.'n')) then
        call PrintVectorD(0,m,y,incy,'Y ')
      else
        call PrintVectorD(0,n,y,incy,'Y ')
      end if

  99  continue
 100  format(a1)
 101  format(7x,'M=',i1,'  N=',i1)
 102  format(7x,'ALPHA=',f5.2,'  BETA=',f5.2)
 103  format(7x,'TRANS=',a1)
      stop
      end
