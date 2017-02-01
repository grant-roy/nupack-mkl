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
!      D S Y R  Example Program Text
!*******************************************************************************

      program   DSYR_MAIN

      use mkl95_precision, only: wp => dp
      use mkl95_blas, only: syr

      implicit none

      character(len = 1) :: uplo
      integer :: n, lda, incx
      integer :: nx, nx1, nx2
      real(wp) :: alpha
      real(wp), allocatable :: a(:,:), x(:)
      integer :: i, j

!     Intrinsic Functions
      intrinsic abs

!     External Subroutines
      external PrintVectorD, PrintArrayD

!     Executable Statements

      print*
      print*, '   D S Y R  EXAMPLE PROGRAM'

!     Read input data from input file
      read*
      read*, n
      read*, incx
      read*, alpha
      read 100, uplo

      nx = 1+(n-1)*abs(incx)

      allocate(x(nx))
      lda = n
      allocate(a(n, n))
      read*, (x(i),i=1, nx)
      if ((uplo.eq.'U').or.(uplo.eq.'u')) then
        read*, ((a(i,j),j=i,n),i=1,n)
      else
        read*, ((a(i,j),j=1,i),i=1,n)
      end if

!     Print input data
      print*
      print*, '     INPUT DATA'
      print 101, n
      print 102, alpha
      print 103, uplo
      call PrintVectorD(0,n,x,incx,'X ')
      if ((uplo.eq.'U').or.(uplo.eq.'u')) then
        call PrintArrayD(0,1,n,n,a,lda,'A')
      else
        call PrintArrayD(0,-1,n,n,a,lda,'A')
      end if

!     Call DSYR subroutine
      if (incx > 0) then
          nx1 = 1
          nx2 = nx
      else
          nx1 = nx
          nx2 = 1
      end if
      call SYR(a, x(nx1:nx2:incx), uplo, alpha)

      print*
      print*, '     OUTPUT DATA'
      if ((uplo.eq.'U').or.(uplo.eq.'u')) then
        call PrintArrayD(1,1,n,n,a,lda,'A')
      else
        call PrintArrayD(1,-1,n,n,a,lda,'A')
      end if

      deallocate(x)
      deallocate(a)

 100  format(a1)
 101  format(7x,'N=',i1)
 102  format(7x,'ALPHA=',f4.1)
 103  format(7x,'UPLO=',a1)
      stop
      end
