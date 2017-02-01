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
!      Z H E R 2 K  Example Program Text
!*******************************************************************************

      program   ZHER2K_MAIN
*
      character*1      uplo, trans
      integer          n, k
      integer          lda, ldb, ldc
      complex*16       alpha
      double precision beta
      integer          rmaxa, cmaxa, rmaxb, cmaxb, rmaxc, cmaxc
      parameter        (rmaxa=4, cmaxa=4, rmaxb=5, cmaxb=5,
     $                 rmaxc=3, cmaxc=5)
      parameter        (lda=rmaxa, ldb=rmaxb, ldc=rmaxc)
      complex*16       a(rmaxa,cmaxa), b(rmaxb,cmaxb), c(rmaxc,cmaxc)
*       External Subroutines
      external         ZHER2K, PrintArrayZ
*
*      Executable Statements
*
      print*
      print*,'   Z H E R 2 K  EXAMPLE PROGRAM'
*
*      Read input data
      read*
      read*, n, k
      read*, alpha, beta
      read 100, uplo, trans
      if ((trans.eq.'N').or.(trans.eq.'n')) then
        if ( (n.gt.rmaxa.or.k.gt.cmaxa).or.
     $       (n.gt.rmaxb.or.k.gt.cmaxb) ) then
          print*, ' Insufficient memory for arrays'
          goto 99
        end if
        read*, ((a(i,j),j=1,k),i=1,n)
        read*, ((b(i,j),j=1,k),i=1,n)
      else
        if ( (k.gt.rmaxa.or.n.gt.cmaxa).or.
     $       (k.gt.rmaxb.or.n.gt.cmaxb) ) then
          print*, ' Insufficient memory for arrays'
          goto 99
        end if
        read*, ((a(i,j),j=1,n),i=1,k)
        read*, ((b(i,j),j=1,n),i=1,k)
      end if
      if ( n.gt.rmaxc.or.n.gt.cmaxc ) then
        print*, ' Insufficient memory for arrays'
        goto 99
      end if
      if ((uplo.eq.'U').or.(uplo.eq.'u')) then
        read*, ((c(i,j),j=i,n),i=1,n)
      else
        read*, ((c(i,j),j=1,i),i=1,n)
      end if
*
*       Print input data
      print*
      print*, '     INPUT DATA'
      print 101, n, k
      print 102, alpha, beta
      print 103, uplo, trans
      if ((trans.eq.'N').or.(trans.eq.'n')) then
        call PrintArrayZ(0,0,n,k,a,lda,'A')
        call PrintArrayZ(0,0,n,k,b,ldb,'B')
      else
        call PrintArrayZ(0,0,k,n,a,lda,'A')
        call PrintArrayZ(0,0,k,n,b,ldb,'B')
      end if
      if ((uplo.eq.'U').or.(uplo.eq.'u')) then
        call PrintArrayZ(0,1,n,n,c,ldc,'C')
      else
        call PrintArrayZ(0,-1,n,n,c,ldc,'C')
      end if
*
*      Call ZHER2K subroutine
      call ZHER2K(uplo, trans, n, k, alpha, a, lda, b, ldb,
     $            beta, c, ldc)
*
      print*
      print*, '     OUTPUT DATA'
      if ((uplo.eq.'U').or.(uplo.eq.'u')) then
        call PrintArrayZ(1,1,n,n,c,ldc,'C')
      else
        call PrintArrayZ(1,-1,n,n,c,ldc,'C')
      end if

  99  continue
 100  format(2(a1,1x))
 101  format(7x,'N=',i1,'  K=',i1)
 102  format(7x,'ALPHA=(',f5.2,',',f5.2,' )','  BETA=',f4.1)
 103  format(7x,'UPLO=',a1, '  TRANS=',a1)
      stop
      end