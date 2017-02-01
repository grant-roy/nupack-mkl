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
!      D D O T I  Example Program Text
!*******************************************************************************

      program   DDOTI_MAIN
      use mkl95_precision, only: wp => dp
      use mkl95_blas, only: doti

      implicit none

      integer :: n
      integer, allocatable :: indx(:)
      real(wp), allocatable :: x(:), y(:)
      real(wp) :: res
      integer :: i, indmax

!     External Function
      integer :: MaxValue
      external MaxValue, PrintVectorD

!     Executable Statements

      print*
      print*,'   D D O T I  EXAMPLE PROGRAM'

!     Read input data from input file
      read*
      read*, n
      allocate(x(n))
      allocate(indx(n))
      read*, (x(i),i=1,n)
      read*, (indx(i),i=1,n)
      indmax = MaxValue(n,indx)
      allocate(y(indmax))
      read*, (y(i),i=1,indmax)

!     Print input data
      print*
      print*, '     INPUT DATA'
      print 100, n
      call PrintVectorD(1,n,x,1,'X ')
      print 101, (indx(i),i=1,n)
      call PrintVectorD(1,indmax,y,1,'Y ')

!     Call DDOTI function
      res = DOTI(x, indx, y)

      print*
      print*, '     OUTPUT DATA'
      print 102, res

      deallocate(x)
      deallocate(indx)
      deallocate(y)

 100  format(7x,'N=',i2)
 101  format(7x,'VECTOR INDX'/10x,10(i2,1x))
 102  format(11x,'DDOTI = ',f8.3)
      stop
      end
