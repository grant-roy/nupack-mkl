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
!      D R O T M   Example Program Text
!*******************************************************************************

      program   DROTM_MAIN

      use mkl95_precision, only: wp => dp
      use mkl95_blas, only: rotm

      implicit none

      integer :: n, incx, incy
      integer :: nx, ny, nx1, nx2, ny1, ny2
      real(wp) :: param(5)
      real(wp), allocatable :: x(:), y(:)
      integer :: i
!     Intrinsic Function
      intrinsic abs
!     External Subroutines
      external PrintVectorD

!     Executable Statements

      print*
      print*,'   D R O T M  EXAMPLE PROGRAM'

!     Read input data from input file
      read*
      read*, n
      read*, incx, incy

      nx = 1+(n-1)*abs(incx)
      ny = 1+(n-1)*abs(incy)

      read*, (param(i),i=1,5)
      allocate(x(nx))
      allocate(y(ny))
      read*, (x(i),i=1,nx)
      read*, (y(i),i=1,ny)

!     Print input data
      print*
      print*, '     INPUT DATA'
      print 100, n
      print 101, (param(i),i=1,5)
      call PrintVectorD(0, n, x, incx, 'X ')
      call PrintVectorD(0, n, y, incy, 'Y ')

!     Call DROTM subroutine
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
      call rotm(x(nx1:nx2:incx), y(ny1:ny2:incy), param)

      print*
      print*, '     OUTPUT DATA'
      call PrintVectorD(1, n, x, incx, 'X ')
      call PrintVectorD(1, n, y, incy, 'Y ')

      deallocate(x)
      deallocate(y)

 100  format(7x,'N=',i2)
 101  format(7x,'VECTOR PARAM',/9x,5(f6.3,1x))
      stop
      end
