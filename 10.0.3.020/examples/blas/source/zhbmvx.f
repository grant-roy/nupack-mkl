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
!      Z H B M V  Example Program Text
!*******************************************************************************

      program   ZHBMV_MAIN
*
      character*1      uplo
      integer          n, k, lda, incx, incy
      integer          mmax, nmax, xmax, ymax
      parameter        (mmax=5, nmax=6, xmax=5, ymax=5)
      parameter        (lda=mmax)
      complex*16       alpha, beta
      complex*16       a(mmax,nmax), x(xmax), y(ymax)
      integer          i, j, k1
*       Intrinsic Functions
      intrinsic        abs
*       External Subroutines
      external         ZHBMV, PrintVectorZ, PrintBandArrayZ
*
*       Executable Statements
*
      print*
      print*, '   Z H B M V  EXAMPLE PROGRAM'
*
*       Read input data from input file
      read*
      read*, n, k
      read*, incx, incy
      read*, alpha, beta
      read 100, uplo
      if ( ((1+(n-1)*abs(incx)).gt.xmax).or.
     $     ((1+(n-1)*abs(incy)).gt.ymax) ) then
        print*, ' Insufficient memory for arrays'
        goto 99
      end if
      read*, (x(i),i=1, 1+(n-1)*abs(incx))
      read*, (y(i),i=1, 1+(n-1)*abs(incy))
      if ( (k+1).gt.mmax.or.n.gt.nmax ) then
        print*, ' Insufficient memory for arrays'
        goto 99
      end if
      if (k.ge.n) then
         k1 = n-1
      else
         k1 = k
      end if
      if ((uplo.eq.'U').or.(uplo.eq.'u')) then
        read*, ((a(k-k1+i,j),j=k1+2-i,n),i=1,k1+1)
      else
        do i = 1, k1+1
           read*, (a(i,j),j=1,n+1-i)
        end do
      end if
*
*       Print input data
      print*
      print*, '     INPUT DATA'
      print 101, n, k
      print 102, alpha, beta
      print 103, uplo
      call PrintVectorZ(0,n,x,incx,'X ')
      call PrintVectorZ(0,n,y,incy,'Y ')
      if ((uplo.eq.'U').or.(uplo.eq.'u')) then
        call PrintBandArrayZ(1,0,k,n,n,a,lda,'A')
      else
        call PrintBandArrayZ(1,k,0,n,n,a,lda,'A')
      end if
*
*      Call ZHBMV subroutine
      call ZHBMV( uplo, n, k, alpha, a, lda, x, incx,
     $            beta, y, incy )
*
      print*
      print*, '     OUTPUT DATA'
      call PrintVectorZ(1,n,y,incy,'Y ')

  99  continue
 100  format(a1)
 101  format(7x,'N=',i1,'  K=',i1)
 102  format(7x,'ALPHA=(',f4.1,',',f4.1,')  BETA=(',f4.1,',',f4.1,')')
 103  format(7x,'UPLO=',a1)
      stop
      end
