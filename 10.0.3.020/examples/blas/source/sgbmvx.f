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
!      S G B M V  Example Program Text
!*******************************************************************************

      program   SGBMV_MAIN
*
      character*1      trans
      integer          m, n, kl, ku, lda, incx, incy
      integer          mmax, nmax, xmax, ymax
      parameter        (mmax=15, nmax=10, xmax=8, ymax=8)
      parameter        (lda=mmax)
      real             alpha, beta
      real             a(mmax,nmax), x(xmax), y(ymax)

      integer          i, j, ku1, kl1

*       Intrinsic Functions
      intrinsic        abs
*       External Subroutines
      external         SGBMV, PrintVectorS, PrintBandArrayS
*
*       Executable Statements
*
      print*
      print*, '   S G B M V  EXAMPLE PROGRAM'
*
*       Read input data from input file
      read*
      read*, m, n, kl, ku
      read*, incx, incy
      read*, alpha, beta
      read 100, trans
      if ((trans.eq.'N').or.(trans.eq.'n')) then
        if ( ((1+(n-1)*abs(incx)).gt.xmax).or.
     $       ((1+(m-1)*abs(incy)).gt.ymax) ) then
        print*, ' Insufficient memory for arrays'
        goto 99
      end if
        read*,(x(i),i=1, 1+(n-1)*abs(incx))
        read*,(y(i),i=1, 1+(m-1)*abs(incy))
      else
        if ( ((1+(m-1)*abs(incx)).gt.xmax).or.
     $       ((1+(n-1)*abs(incy)).gt.ymax) ) then
        print*, ' Insufficient memory for arrays'
        goto 99
      end if
        read*,(x(i),i=1, 1+(m-1)*abs(incx))
        read*,(y(i),i=1, 1+(n-1)*abs(incy))
      end if
      if ( (kl+ku+1).gt.mmax.or.n.gt.nmax ) then
        print*, ' Insufficient memory for arrays'
        goto 99
      end if
      if (ku.ge.n) then
         ku1 = n-1
      else
         ku1 = ku
      end if
      if (kl.ge.m) then
         kl1 = m-1
      else
         kl1 = kl
      end if
      do i=1, ku1+1
         if ((ku1-i+m+1).ge.n) then
            k = n
         else
            k = ku1-i+m+1
         end if
         read*, (a(ku-ku1+i,j),j=ku1-i+2,k)
      end do
      do i=ku1+2, ku1+kl1+1
         if ((m+ku1-i+1).ge.n) then
            k = n
         else
            k = m+ku1-i+1
         end if
         read*, (a(ku-ku1+i,j),j=1,k)
      end do
*
*       Print input data
      print*
      print*, '     INPUT DATA'
      print 101, m, n
      print 102, alpha, beta
      print 103, trans
      if ((trans.eq.'N').or.(trans.eq.'n')) then
        call PrintVectorS(0,n,x,incx,'X ')
        call PrintVectorS(0,m,y,incy,'Y ')
      else
        call PrintVectorS(0,m,x,incx,'X ')
        call PrintVectorS(0,n,y,incy,'Y ')
      end if
      call PrintBandArrayS(0,kl,ku,m,n,a,lda,'A')
*
*      Call SGBMV subroutine
      call SGBMV(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
*
      print*
      print*, '     OUTPUT DATA'
      if ((trans.eq.'N').or.(trans.eq.'n')) then
        call PrintVectorS(0,m,y,incy,'Y ')
      else
        call PrintVectorS(0,n,y,incy,'Y ')
      end if

  99  continue
 100  format(a1)
 101  format(7x,'M=',i1,'  N=',i1)
 102  format(7x,'ALPHA=',f4.1,'  BETA=',f4.1)
 103  format(7x,'TRANS=',a1)
      stop
      end
