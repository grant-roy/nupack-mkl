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
!      D G E M M  Example Program Text
!*******************************************************************************

      program   DGEMM_MAIN
      use mkl95_precision, only: wp => dp
      use mkl95_blas, only: gemm

      implicit none

      character(len = 1) :: transa, transb
      integer :: m, n, k
      integer :: i, j
      integer :: lda, ldb, ldc
      real(wp) :: alpha, beta
      real(wp), allocatable :: a(:,:), b(:,:), c(:,:)

!     External Subroutines
      external PrintArrayD

!     Executable Statements

      print*
      print*,'   D G E M M  EXAMPLE PROGRAM'

!     Read input data
      read*
      read*, m, n, k
      read*, alpha, beta
      read 100, transa, transb
      if ((transa.eq.'N').or.(transa.eq.'n')) then
        lda = m
        allocate(a(m, k))
        read*, ((a(i,j),j=1,k),i=1,m)
      else
        lda = k
        allocate(a(k, m))
        read*, ((a(i,j),j=1,m),i=1,k)
      end if
      if ((transb.eq.'N').or.(transb.eq.'n')) then
        ldb = k
        allocate(b(k, n))
        read*, ((b(i,j),j=1,n),i=1,k)
      else
        ldb = n
        allocate(b(n, k))
        read*, ((b(i,j),j=1,k),i=1,n)
      end if
      ldc = m
      allocate(c(m, n))
      read*, ((c(i,j),j=1,n),i=1,m)

!     Print input data
      print*
      print*, '     INPUT DATA'
      print 101, m, n, k
      print 102, alpha, beta
      print 103, transa, transb
      if ((transa.eq.'N').or.(transa.eq.'n')) then
        call PrintArrayD(0,0,m,k,a,lda,'A')
      else
        call PrintArrayD(0,0,k,m,a,lda,'A')
      end if
      if ((transb.eq.'N').or.(transb.eq.'n')) then
        call PrintArrayD(0,0,k,n,b,ldb,'B')
      else
        call PrintArrayD(0,0,n,k,b,ldb,'B')
      end if
      call PrintArrayD(0,0,m,n,c,ldc,'C')

!     Call DGEMM subroutine
      call GEMM(a, b, c, transa, transb, alpha, beta)

      print*
      print*, '     OUTPUT DATA'
      call PrintArrayD(1,0,m,n,c,ldc,'C')

      deallocate(a)
      deallocate(b)
      deallocate(c)

 100  format(2(a1,1x))
 101  format(7x,'M=',i1,'  N=',i1,'  K=',i1)
 102  format(7x,'ALPHA=',f5.2,'  BETA=',f5.2)
 103  format(7x,'TRANSA=',a1, '  TRANSB=',a1)
      stop
      end
