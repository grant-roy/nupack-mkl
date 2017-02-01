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
!      D T R S M  Example Program Text
!*******************************************************************************

      program   DTRSM_MAIN
      use mkl95_precision, only: wp => dp
      use mkl95_blas, only: trsm

      implicit none

      character(len = 1) :: side, uplo, transa, diag
      integer :: m, n, lda, ldb
      real(wp) :: alpha
      real(wp), allocatable :: a(:,:), b(:,:)
      integer :: i, j

!     External Subroutines
      external PrintArrayD

!     Executable Statements

      print*
      print*,'   D T R S M  EXAMPLE PROGRAM'
!     Read input data from input file
      read*
      read*, m, n
      read*, alpha
      read 100, side, uplo, transa, diag
      if ((side.eq.'L').or.(side.eq.'l')) then
        lda = m
        allocate(a(m, m))
        if ((uplo.eq.'U').or.(uplo.eq.'u')) then
          read*, ((a(i,j),j=i,m),i=1,m)
        else
          read*, ((a(i,j),j=1,i),i=1,m)
        end if
      else
        lda = n
        allocate(a(n, n))
        if ((uplo.eq.'U').or.(uplo.eq.'u')) then
          read*, ((a(i,j),j=i,n),i=1,n)
        else
          read*, ((a(i,j),j=1,i),i=1,n)
        end if
      end if
      ldb = m
      allocate(b(m, n))
      read*, ((b(i,j),j=1,n),i=1,m)

!     Print input data
      print*
      print*, '     INPUT DATA'
      print 101, m, n
      print 102, alpha
      print 103, side, uplo, transa, diag
      if ((side.eq.'L').or.(side.eq.'l')) then
        if ((uplo.eq.'U').or.(uplo.eq.'u')) then
          call PrintArrayD(0, 1, m, m, a, lda, 'A')
        else
          call PrintArrayD(0, -1, m, m, a, lda, 'A')
        end if
      else
        if ((uplo.eq.'U').or.(uplo.eq.'u')) then
          call PrintArrayD(0, 1, n, n, a, lda, 'A')
        else
          call PrintArrayD(0, -1, n, n, a, lda, 'A')
        end if
      end if
      call PrintArrayD(0, 0, m, n, b, ldb, 'B')

!     Call DTRSM subroutine
      call TRSM(a, b, side, uplo, transa, diag, alpha)

!     Print output data
      print*
      print*, '     OUTPUT DATA'
      call PrintArrayD(1, 0, m, n, b, ldb, 'B')

      deallocate(a)
      deallocate(b)

 100  format(4(a1,1x))
 101  format(7x,'M=',i1,'  N=',i1)
 102  format(7x,'ALPHA=',f3.1)
 103  format(7x,'SIDE=',a1, '  UPLO=',a1,'  TRANSA=',a1,'  DIAG=',a1)
      stop
      end
