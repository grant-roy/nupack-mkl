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
!      D D O T  Example Program Text
!*******************************************************************************

      program  DDOT_MAIN
      use mkl95_precision, only: wp => dp
      use mkl95_blas, only: dot

      implicit none

      integer :: n, incx, incy
      integer :: nx, ny, nx1, nx2, ny1, ny2
      real(wp), allocatable :: sx(:), sy(:)
      real(wp) :: res
      integer :: i

!     Intrinsic Functions
      intrinsic abs
!     External Subroutines
      external PrintVectorD

!     Executable Statements

      print*
      print*,'   D D O T  EXAMPLE PROGRAM'

!     Read input data from input file
      read*
      read*, n
      read*, incx, incy

      nx = 1+(n-1)*abs(incx)
      ny = 1+(n-1)*abs(incy)

      allocate(sx(nx))
      allocate(sy(ny))
      read*, (sx(i),i=1,nx)
      read*, (sy(i),i=1,ny)

!     Print input data
      print*
      print*, '     INPUT DATA'
      print 100, n
      call PrintVectorD(0,n,sx,incx,'X ')
      call PrintVectorD(0,n,sy,incy,'Y ')

!     Call DDOT subroutine
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
      res = DOT(sx(nx1:nx2:incx),sy(ny1:ny2:incy))

      print*
      print*, '     OUTPUT DATA'
      print 101, res

      deallocate(sx)
      deallocate(sy)

 100  format(7x,'N=',i2)
 101  format(7x,'DDOT = ',f8.3)
      stop
      end
