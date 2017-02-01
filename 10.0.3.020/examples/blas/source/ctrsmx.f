!*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 1999-2008 Intel Corporation. All Rights Reserved.
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
!      C T R S M  Example Program Text
!*******************************************************************************

      program   CTRSM_MAIN
*
      character*1        side, uplo, transa, diag
      integer            m, n, lda, ldb
      complex            alpha
      integer            rmaxa, cmaxa, rmaxb, cmaxb
      parameter          (rmaxa=5, cmaxa=5, rmaxb=5, cmaxb=5)
      parameter          (lda=rmaxa, ldb=rmaxb)
      complex            a(rmaxa, cmaxa), b(rmaxb, cmaxb)
      integer            i, j
*      External Subroutines
      external           CTRSM, PrintArrayC
*
*      Executable Statements
*
      print*
      print*,'   C T R S M  EXAMPLE PROGRAM'
*       Read input data from input file
      read*
      read*, m, n
      read*, alpha
      read 100, side, uplo, transa, diag
      if ((side.eq.'L').or.(side.eq.'l')) then
        if ( m.gt.rmaxa.or.m.gt.cmaxa.or.
     $       m.gt.rmaxb.or.n.gt.cmaxb ) then
          print*, ' Insufficient memory for arrays'
          goto 99
        end if
        if ((uplo.eq.'U').or.(uplo.eq.'u')) then
          read*, ((a(i,j),j=i,m),i=1,m)
        else
          read*, ((a(i,j),j=1,i),i=1,m)
        end if
      else
        if ( n.gt.rmaxa.or.n.gt.cmaxa.or.
     $       m.gt.rmaxb.or.n.gt.cmaxb ) then
          print*, ' Insufficient memory for arrays'
          goto 99
        end if
        if ((uplo.eq.'U').or.(uplo.eq.'u')) then
          read*, ((a(i,j),j=i,n),i=1,n)
        else
          read*, ((a(i,j),j=1,i),i=1,n)
        end if
      end if
      read*, ((b(i,j),j=1,n),i=1,m)
*
*       Print input data
      print*
      print*, '     INPUT DATA'
      print 101, m, n
      print 102, alpha
      print 103, side, uplo, transa, diag
      if ((side.eq.'L').or.(side.eq.'l')) then
        if ((uplo.eq.'U').or.(uplo.eq.'u')) then
          call PrintArrayC(0, 1, m, m, a, lda, 'A')
        else
          call PrintArrayC(0, -1, m, m, a, lda, 'A')
        end if
      else
        if ((uplo.eq.'U').or.(uplo.eq.'u')) then
          call PrintArrayC(0, 1, n, n, a, lda, 'A')
        else
          call PrintArrayC(0, -1, n, n, a, lda, 'A')
        end if
      end if
      call PrintArrayC(0, 0, m, n, b, ldb, 'B')
*
*      Call CTRSM subroutine
      call CTRSM(side, uplo, transa, diag, m, n, alpha, a, lda,
     $           b, ldb)
*
*       Print output data
      print*
      print*, '     OUTPUT DATA'
      call PrintArrayC(1, 0, m, n, b, ldb, 'B')

  99  continue
 100  format(4(a1,1x))
 101  format(7x,'M=',i1,'  N=',i1)
 102  format(7x,'ALPHA=(',f4.1,',',f4.1,' )')
 103  format(7x,'SIDE=',a1, '  UPLO=',a1,'  TRANSA=',a1,'  DIAG=',a1)
      stop
      end
