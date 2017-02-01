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
!      D T B S V  Example Program Text
!*******************************************************************************

      program   DTBSV_MAIN
      use mkl95_precision, only: wp => dp
      use mkl95_blas, only: tbsv

      implicit none

      character(len = 1) :: uplo, trans, diag
      integer :: n, k, lda, incx
      integer :: nx, nx1, nx2
      real(wp), allocatable :: a(:,:), x(:)
      integer :: i, j, k1

!     Intrinsic Functions
      intrinsic abs
!     External Subroutines
      external PrintVectorD, PrintBandArrayD

!     Executable Statements

      print*
      print*, '   D T B S V  EXAMPLE PROGRAM'

!     Read input data from input file
      read*
      read*, n, k
      read*, incx
      read 100, uplo, trans, diag
      nx = 1+(n-1)*abs(incx)
      allocate(x(nx))
      read*, (x(i),i=1, nx)
      if (k.ge.n) then
         k1 = n-1
      else
         k1 = k
      end if
      lda = k+1
      allocate(a(lda, n))
      if ((uplo.eq.'U').or.(uplo.eq.'u')) then
        read*, ((a(k-k1+i,j),j=k1+2-i,n),i=1,k1+1)
      else
        do i = 1, k1+1
           read*, (a(i,j),j=1,n+1-i)
        end do
      end if

!     Print input data
      print*
      print*, '     INPUT DATA'
      print 101, n, k
      print 102, uplo, trans, diag
      call PrintVectorD(0,n,x,incx,'X ')
      if ((uplo.eq.'U').or.(uplo.eq.'u')) then
        call PrintBandArrayD(1,0,k,n,n,a,lda,'A')
      else
        call PrintBandArrayD(1,k,0,n,n,a,lda,'A')
      end if

!     Call DTBSV subroutine
      if (incx > 0) then
          nx1 = 1
          nx2 = nx
      else
          nx1 = nx
          nx2 = 1
      end if
      call TBSV(a, x(nx1:nx2:incx), uplo, trans, diag)

      print*
      print*, '     OUTPUT DATA'
      call PrintVectorD(1,n,x,incx,'X ')

      deallocate(x)
      deallocate(a)

 100  format(3(a1,1x))
 101  format(7x,'N=',i1,'  K=',i1)
 102  format(7x,'UPLO=',a1,'  TRANS=',a1,'  DIAG=',a1)
      stop
      end
