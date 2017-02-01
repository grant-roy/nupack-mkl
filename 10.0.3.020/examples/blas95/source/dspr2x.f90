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
!      D S P R 2  Example Program Text
!*******************************************************************************

      program   DSPR2_MAIN

      use mkl95_precision, only: wp => dp
      use mkl95_blas, only: spr2

      implicit none

      character(len = 1) :: uplo
      integer :: n, incx, incy
      integer :: nx, ny, nap, nx1, nx2, ny1, ny2
      real(wp) :: alpha
      real(wp), allocatable :: ap(:), x(:), y(:)
      integer :: i

!     Intrinsic Functions
      intrinsic abs
!     External Subroutines
      external PrintVectorD

!     Executable Statements

      print*
      print*, '   D S P R 2  EXAMPLE PROGRAM'

!     Read input data from input file
      read*
      read*, n
      read*, incx, incy
      read*, alpha
      read 100, uplo

      nx = 1+(n-1)*abs(incx)
      ny = 1+(n-1)*abs(incy)
      nap = (n*(n+1))/2

      allocate(x(nx))
      allocate(y(ny))
      allocate(ap(nap))
      read*, (x(i),i=1, nx)
      read*, (y(i),i=1, ny)
      read*, (ap(i),i=1,nap)

!     Print input data
      print*
      print*, '     INPUT DATA'
      print 101, n
      print 102, alpha
      print 103, uplo
      call PrintVectorD(0,n,x,incx,'X ')
      call PrintVectorD(0,n,y,incy,'Y ')
      call PrintVectorD(1,nap,ap,1,'AP')

!     Call DSPR2 subroutine
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
      call SPR2(ap, x(nx1:nx2:incx), y(ny1:ny2:incy), uplo, alpha)

      print*
      print*, '     OUTPUT DATA'
      call PrintVectorD(1,nap,ap,1,'AP')

      deallocate(x)
      deallocate(y)
      deallocate(ap)

 100  format(a1)
 101  format(7x,'N=',i1)
 102  format(7x,'ALPHA=',f4.1)
 103  format(7x,'UPLO=',a1)
      stop
      end
