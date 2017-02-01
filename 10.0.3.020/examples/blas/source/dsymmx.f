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
!      D S Y M M  Example Program Text
!*******************************************************************************

      program DSYMM_MAIN
*
      character*1        side, uplo
      integer            m, n, lda, ldb, ldc
      double precision   alpha, beta
      integer            rmaxa, cmaxa, rmaxb, cmaxb, rmaxc, cmaxc
      parameter          (rmaxa=5, cmaxa=5, rmaxb=5, cmaxb=5,
     $                    rmaxc=5, cmaxc=5)
      parameter          (lda=rmaxa, ldb=rmaxb, ldc=rmaxc)
      double precision   a(rmaxa, cmaxa), b(rmaxb, cmaxb),
     $                   c(rmaxc, cmaxc)
      integer            i, j
*      External Subroutines
      external           DSYMM, PrintArrayS
*
*      Executable Statements
*
      print*
      print*,'   D S Y M M  EXAMPLE PROGRAM'
*       Read input data from input file
      read*
      read*, m, n
      read*, alpha, beta
      read 100, side, uplo
      if ((side.eq.'L').or.(side.eq.'l')) then
        if ( (m.gt.rmaxa).or.(m.gt.cmaxa) ) then
          print*, ' Insufficient memory for arrays'
          goto 99
        end if
        if ((uplo.eq.'U').or.(uplo.eq.'u')) then
          read*, ((a(i,j),j=i,m),i=1,m)
        else
          read*, ((a(i,j),j=1,i),i=1,m)
        end if
      else
        if ( (n.gt.rmaxa).or.(n.gt.cmaxa) ) then
          print*, ' Insufficient memory for arrays'
          goto 99
        end if
        if ((uplo.eq.'U').or.(uplo.eq.'u')) then
          read*, ((a(i,j),j=i,n),i=1,n)
        else
          read*, ((a(i,j),j=1,i),i=1,n)
        end if
      end if
      if ( (m.gt.rmaxb).or.(n.gt.cmaxb).or.
     $     (m.gt.rmaxc).or.(n.gt.cmaxc) ) then
          print*, ' Insufficient memory for arrays'
          goto 99
      end if
      read*, ((b(i,j),j=1,n),i=1,m)
      read*, ((c(i,j),j=1,n),i=1,m)
*
*       Print input data
      print*
      print*, '     INPUT DATA'
      print 101, m, n
      print 102, alpha, beta
      print 103, side, uplo
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
      call PrintArrayD(0,0,m,n,b,ldb,'B')
      call PrintArrayD(0,0,m,n,c,ldc,'C')
*
*      Call DSYMM subroutine
      call DSYMM(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
*
*       Print output data
      print*
      print*, '     OUTPUT DATA'
      call PrintArrayD(1, 0, m, n, c, ldc, 'C')

  99  continue
 100  format(2(a1,1x))
 101  format(7x,'M=',i1,'  N=',i1)
 102  format(7x,'ALPHA=',f6.3,'  BETA=',f6.3)
 103  format(7x,'SIDE=',a1, '  UPLO=',a1)
      stop
      end
