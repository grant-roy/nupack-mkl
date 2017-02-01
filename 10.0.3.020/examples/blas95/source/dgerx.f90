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
!      D G E R  Example Program Text
!*******************************************************************************

      program   DGER_MAIN
      use mkl95_precision, only: wp => dp
      use mkl95_blas, only: ger

      implicit none

      integer :: m, n, lda, incx, incy
      integer :: nx, ny, nx1, nx2, ny1, ny2
      real(wp) :: alpha
      real(wp), allocatable :: a(:,:), x(:), y(:)
      integer :: i, j

!     Intrinsic Functions
      intrinsic abs
!     External Subroutines
      external PrintVectorD, PrintArrayD

!     Executable Statements

      print*
      print*, '   D G E R  EXAMPLE PROGRAM'

!     Read input data from input file
      read*
      read*, m, n
      read*, incx, incy
      read*, alpha
      nx = 1+(m-1)*abs(incx)
      ny = 1+(n-1)*abs(incy)
      allocate(x(nx))
      allocate(y(ny))
      read*, (x(i),i=1, nx)
      read*, (y(i),i=1, ny)
      lda = m
      allocate(a(m, n))
      read*, ((a(i,j),j=1,n),i=1,m)

!     Print input data
      print*
      print*, '     INPUT DATA'
      print 101, m, n
      print 102, alpha
      call PrintVectorD(0,m,x,incx,'X ')
      call PrintVectorD(0,n,y,incy,'Y ')
      call PrintArrayD(0,0,m,n,a,lda,'A')

!     Call DGER subroutine
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
      call GER(a, x(nx1:nx2:incx), y(ny1:ny2:incy), alpha)

      print*
      print*, '     OUTPUT DATA'
      call PrintArrayD(0,0,m,n,a,lda,'A')

      deallocate(x)
      deallocate(y)
      deallocate(a)

 101  format(7x,'M=',i1,'  N=',i1)
 102  format(7x,'ALPHA=',f4.1)
      stop
      end
