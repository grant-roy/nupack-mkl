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
!      D S Y R 2 K  Example Program Text
!*******************************************************************************

      program   DSYR2K_MAIN

      use mkl95_precision, only: wp => dp
      use mkl95_blas, only: syr2k

      implicit none

      character(len = 1) :: uplo, trans
      integer :: n, k, lda, ldb, ldc
      real(wp) :: alpha, beta
      real(wp), allocatable :: a(:,:), b(:,:), c(:,:)
      integer :: i, j

!     External Subroutines
      external PrintArrayD

!     Executable Statements

      print*
      print*,'   D S Y R 2 K  EXAMPLE PROGRAM'

!     Read input data
      read*
      read*, n, k
      read*, alpha, beta
      read 100, uplo, trans

      if ((trans.eq.'N').or.(trans.eq.'n')) then
        lda = n
        ldb = n
        allocate(a(n, k))
        allocate(b(n, k))
        read*, ((a(i,j),j=1,k),i=1,n)
        read*, ((b(i,j),j=1,k),i=1,n)
      else
        lda = k
        ldb = k
        allocate(a(k, n))
        allocate(b(k, n))
        read*, ((a(i,j),j=1,n),i=1,k)
        read*, ((b(i,j),j=1,n),i=1,k)
      end if
      ldc = n
      allocate(c(n, n))
      if ((uplo.eq.'U').or.(uplo.eq.'u')) then
        read*, ((c(i,j),j=i,n),i=1,n)
      else
        read*, ((c(i,j),j=1,i),i=1,n)
      end if

!     Print input data
      print*
      print*, '     INPUT DATA'
      print 101, n, k
      print 102, alpha, beta
      print 103, uplo, trans
      if ((trans.eq.'N').or.(trans.eq.'n')) then
        call PrintArrayD(0,0,n,k,a,lda,'A')
        call PrintArrayD(0,0,n,k,b,ldb,'B')
      else
        call PrintArrayD(0,0,k,n,a,lda,'A')
        call PrintArrayD(0,0,k,n,b,ldb,'B')
      end if
      if ((uplo.eq.'U').or.(uplo.eq.'u')) then
        call PrintArrayD(0,1,n,n,c,ldc,'C')
      else
        call PrintArrayD(0,-1,n,n,c,ldc,'C')
      end if

!     Call DSYR2K subroutine
      call SYR2K(a, b, c, uplo, trans, alpha, beta)

      print*
      print*, '     OUTPUT DATA'
      if ((uplo.eq.'U').or.(uplo.eq.'u')) then
        call PrintArrayD(1,1,n,n,c,ldc,'C')
      else
        call PrintArrayD(1,-1,n,n,c,ldc,'C')
      end if

      deallocate(a)
      deallocate(b)
      deallocate(c)

 100  format(2(a1,1x))
 101  format(7x,'N=',i1,'  K=',i1)
 102  format(7x,'ALPHA=',f3.1,'  BETA=',f3.1)
 103  format(7x,'UPLO=',a1, '  TRANS=',a1)
      stop
      end
