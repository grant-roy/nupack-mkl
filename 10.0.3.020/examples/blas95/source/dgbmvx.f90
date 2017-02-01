!*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 2005-2008 Intel Corporation. All Rights Reserved.
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
!      D G B M V  Example Program Text
!*******************************************************************************

      program   DGBMV_MAIN
      use mkl95_precision, only: wp => dp
      use mkl95_blas, only: gbmv

      implicit none

      character(len = 1) :: trans
      integer :: m, n, k, kl, ku, incx, incy
      integer :: nx, ny, nx1, nx2, ny1, ny2
      integer :: lda
      real(wp) :: alpha, beta
      real(wp), allocatable :: a(:,:), x(:), y(:)
      integer :: i, j, ku1, kl1

!     Intrinsic Functions
      intrinsic abs
!     External Subroutines
      external PrintVectorD, PrintBandArrayD

!     Executable Statements

      print*
      print*, '   D G B M V  EXAMPLE PROGRAM'

!     Read input data from input file
      read*
      read*, m, n, kl, ku
      read*, incx, incy
      read*, alpha, beta
      read 100, trans

      if ((trans.eq.'N').or.(trans.eq.'n')) then
        nx = 1+(n-1)*abs(incx)
        ny = 1+(m-1)*abs(incy)
      else
        nx = 1+(m-1)*abs(incx)
        ny = 1+(n-1)*abs(incy)
      end if
      allocate(x(nx))
      allocate(y(ny))
      read*,(x(i),i=1, nx)
      read*,(y(i),i=1, ny)
      lda = kl+ku+1
      allocate(a(lda, n))
      a = 0
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

!     Print input data
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
      call PrintBandArrayD(0,kl,ku,m,n,a,lda,'A')

!     Call DGBMV subroutine
      if (incx > 0) then
          nx1 = 1
          nx2 = nx
      else
          nx1 = nx
          nx2 = 1
      end if
      if (incy > 0) then
          ny1 = 1
          ny2 = ny
      else
          ny1 = ny
          ny2 = 1
      end if
      call GBMV(a, x(nx1:nx2:incx), y(ny1:ny2:incy), kl, m, alpha, beta, trans)

      print*
      print*, '     OUTPUT DATA'
      if ((trans.eq.'N').or.(trans.eq.'n')) then
        call PrintVectorD(0,m,y,incy,'Y ')
      else
        call PrintVectorD(0,n,y,incy,'Y ')
      end if

      deallocate(x)
      deallocate(y)
      deallocate(a)

 100  format(a1)
 101  format(7x,'M=',i1,'  N=',i1)
 102  format(7x,'ALPHA=',f4.1,'  BETA=',f4.1)
 103  format(7x,'TRANS=',a1)
      stop
      end
