!*******************************************************************************
!                             INTEL CONFIDENTIAL
!  Copyright(C) 2001-2008 Intel Corporation. All Rights Reserved.
!  The source code contained  or  described herein and all documents related to
!  the source code ("Material") are owned by Intel Corporation or its suppliers
!  or licensors.  Title to the  Material remains with  Intel Corporation or its
!  suppliers and licensors. The Material contains trade secrets and proprietary
!  and  confidential  information of  Intel or its suppliers and licensors. The
!  Material  is  protected  by  worldwide  copyright  and trade secret laws and
!  treaty  provisions. No part of the Material may be used, copied, reproduced,
!  modified, published, uploaded, posted, transmitted, distributed or disclosed
!  in any way without Intel's prior express written permission.
!  No license  under any  patent, copyright, trade secret or other intellectual
!  property right is granted to or conferred upon you by disclosure or delivery
!  of the Materials,  either expressly, by implication, inducement, estoppel or
!  otherwise.  Any  license  under  such  intellectual property  rights must be
!  express and approved by Intel in writing.
!
!*******************************************************************************
!  Content:
!    vdRngGaussianMV  Example Program Text
!*******************************************************************************

      include 'mkl_vsl.fi'
      include "errcheck.inc"

      program MKL_VSL_TEST

      USE MKL_VSL_TYPE
      USE MKL_VSL

      integer(kind=4) i,j,k
      integer(kind=4) errcode

      integer(kind=4) nn
      integer ndim,info
      integer n

      parameter(n=1000,nn=5,ndim=3)

      integer brng,method,seed
      integer me

      real(kind=8) c(ndim,ndim),t(ndim,ndim),c1(ndim*(ndim+1)/2),a(ndim)
      real(kind=8) r(ndim,n),tt(ndim,ndim)
      real(kind=8) dbSX,dbSY,dbSZ,dbS2X,dbS2Y,dbS2Z,dbSXY,dbSXZ,dbSYZ
      real(kind=8) dbMeanX,dbMeanY,dbMeanZ
      real(kind=8) dbVarX,dbVarY,dbVarZ
      real(kind=8) dbCovXY,dbCovXZ,dbCovYZ

      real(kind=8) dn
      real(kind=8) SX, SY, SZ
      real(kind=8) D2X, D2Y, D2Z
      real(kind=8) QX, QY, QZ
      real(kind=8) DeltaMX,DeltaMY,DeltaMZ
      real(kind=8) DeltaDX,DeltaDY,DeltaDZ

      TYPE (VSL_STREAM_STATE) :: stream

      brng=VSL_BRNG_MCG31
      seed=7777777
      method=VSL_METHOD_DGAUSSIANMV_BOXMULLER2
      me=VSL_MATRIX_STORAGE_FULL

!     Variance-covariance matrix for test
!     (should be symmetric,positive-definite)

!     This is full storage for spotrf subroutine
      c(1,1)=16.0
      c(1,2)=8.0
      c(1,3)=4.0

      c(2,1)=8.0
      c(2,2)=13.0
      c(2,3)=17.0

      c(3,1)=4.0
      c(3,2)=17.0
      c(3,3)=62.0

      c1(1)=16.0
      c1(2)=8.0
      c1(3)=4.0
      c1(4)=13.0
      c1(5)=17.0
      c1(6)=62.0

      a(1)=3.0
      a(2)=5.0
      a(3)=2.0

      dn=n

      t  = c

      print *,'Variance-covariance matrix C'
      write (*,'(3F7.3)') c
      print *,''

      print *,'Mean vector a:'
      write (*,'(3F7.3)') a
      print *,''

      print *,'VSL_MATRIX_STORAGE_FULL'
      print *,'-----------------------'
      print *,''

      call dpotrf('U',ndim,t,ndim,info)

!     **************************************************************************
      do i=1,ndim
        do j=1,ndim
          if(i>j) t(i,j)=0.0
        end do
      end do

      do i=1,ndim
        do j=1,ndim
          tt(j,i)=0.0
          do k=1,ndim
            tt(j,i)=tt(j,i)+t(k,i)*t(k,j)
          end do
        end do
      end do

!     for(i=0;i<NDIM;i++) {
!         for(j=0;j<NDIM;j++) {
!             TT[i][j]=0.0;
!             for(k=0;k<NDIM;k++) {
!                 TT[i][j]+=t[i][k]*t[j][k];
!             }
!         }
!     }

      print *,'DPOTRF subroutine'
      print *,'*****************'
      print *,'Matrix T:             Matrix T*T (should be equal to C):'
      do i=1,ndim
        print 10,t(:,i),tt(:,i)
      end do
10    format(3F7.3,'  ',3F7.3)
      print *,''

!     **************************************************************************

!     Stream initialization
      errcode=vslnewstream(stream,brng,seed)
      call CheckVslError(errcode)

!     Generating random numbers
!     from multivariate normal distribution
      errcode=vdrnggaussianmv(method,stream,n,r,ndim,me,a,t)
      call CheckVslError(errcode)

!     Printing random numbers
      print 11,' Results (first ',nn,' of ',n,')'
      print *,'--------------------------'
11    format(A,I1,A,I5,A)

      do i=1,nn
        print 12,' r(',i,')=(',r(:,i),')'
      end do
12    format(A,I1,A,3F8.3,A)
      print *,''

      dbSX =0.0
      dbS2X=0.0
      dbSY =0.0
      dbS2Y=0.0
      dbSZ =0.0
      dbS2Z=0.0
      dbSXY=0.0
      dbSXZ=0.0
      dbSYZ=0.0

      do i=1,n
        dbSX  = dbSX  + r(1,i)
        dbS2X = dbS2X + r(1,i)*r(1,i)
        dbSY  = dbSY  + r(2,i)
        dbS2Y = dbS2Y + r(2,i)*r(2,i)
        dbSZ  = dbSZ  + r(3,i)
        dbS2Z = dbS2Z + r(3,i)*r(3,i)
        dbSXY = dbSXY + r(1,i)*r(2,i)
        dbSXZ = dbSXZ + r(1,i)*r(3,i)
        dbSYZ = dbSYZ + r(2,i)*r(3,i)
      end do

      dbMeanX = dbSX/n
      dbMeanY = dbSY/n
      dbMeanZ = dbSZ/n
      dbVarX  = dbS2X/n-dbMeanX*dbMeanX
      dbVarY  = dbS2Y/n-dbMeanY*dbMeanY
      dbVarZ  = dbS2Z/n-dbMeanZ*dbMeanZ
      dbCovXY = dbSXY/n-dbMeanX*dbMeanY
      dbCovXZ = dbSXZ/n-dbMeanX*dbMeanZ
      dbCovYZ = dbSYZ/n-dbMeanY*dbMeanZ

!     Printing
      print *,'Sample characteristics:'
      print *,'-----------------------'
      print *,'      Sample             Theory'
      print 13,' Mean :(',dbMeanX,dbMeanY,dbMeanZ,                      &
     &         ')  (',a(1),a(2),a(3),')'
      print 13,' Var. :(',dbVarX,dbVarY,dbVarZ,                         &
     &         ')  (',c(1,1),c(2,2),c(3,3),')'
      print 14,' CovXY: ',dbCovXY,'          ',c(1,2)
      print 14,' CovXZ: ',dbCovXZ,'          ',c(1,3)
      print 14,' CovYZ: ',dbCovYZ,'          ',c(2,3)
13    format(A,F5.1,F5.1,F5.1,A,F5.1,F5.1,F5.1,A)
14    format(A,F6.1,A,F6.1)
      print *,''

      D2X = c(1,1)*c(1,1);
      D2Y = c(2,2)*c(2,2);
      D2Z = c(3,3)*c(3,3);

      QX  = 3.0*D2X;
      QY  = 3.0*D2Y;
      QZ  = 3.0*D2Z;

      SX = ((QX-D2X)/dn)-(2*(QX-2*D2X)/(dn*dn))+((QX-3*D2X)/(dn*dn*dn));
      SY = ((QY-D2Y)/dn)-(2*(QY-2*D2Y)/(dn*dn))+((QY-3*D2Y)/(dn*dn*dn));
      SZ = ((QZ-D2Z)/dn)-(2*(QZ-2*D2Z)/(dn*dn))+((QZ-3*D2Z)/(dn*dn*dn));

      DeltaMX = (a(1)-dbMeanX)/sqrt(c(1,1)/dn);
      DeltaMY = (a(2)-dbMeanY)/sqrt(c(2,2)/dn);
      DeltaMZ = (a(3)-dbMeanZ)/sqrt(c(3,3)/dn);

      DeltaDX = (c(1,1)-dbVarX)/sqrt(SX);
      DeltaDY = (c(2,2)-dbVarY)/sqrt(SY);
      DeltaDZ = (c(3,3)-dbVarZ)/sqrt(SZ);


      if (DeltaMX>3.0 .OR. DeltaDX>3.0 .OR.                             &
     &   DeltaMY>3.0 .OR. DeltaDY>3.0 .OR.                              &
     &   DeltaMZ>3.0 .OR. DeltaDZ>3.0) then
        print *,"Error: sample moments"
        print *,"are disagreed with theory"
        print 15, "    DeltaM: ", DeltaMX, DeltaMY, DeltaMZ
        print 15, "    DeltaD: ", DeltaDX, DeltaDY, DeltaDZ
        print *,  "   ( at least one of the Deltas > 3.0) "
        stop 1
      else
        print *,"Sample moments"
        print *,"are agreed with theory"
        print 15, "    DeltaM: ", DeltaMX, DeltaMY, DeltaMZ
        print 15, "    DeltaD: ", DeltaDX, DeltaDY, DeltaDZ
        print *,  "   ( all Deltas < 3.0) "
      end if
      print *,''

15    format(A,F7.3,F7.3,F7.3)


!     Stream finalization
      errcode=vslDeleteStream(stream)
      call CheckVslError(errcode)

      call dpptrf('L',ndim,c1,info)

      print *,'VSL_MATRIX_STORAGE_PACKED'
      print *,'-------------------------'

!     **************************************************************************
      k=1
      do i=1,ndim
        do j=1,ndim
          if(j<i) then
            t(i,j)=0.0
          else
            t(i,j)=c1(k)
            k=k+1
          endif
        end do
      end do

      do i=1,ndim
        do j=1,ndim
          tt(j,i)=0.0
          do k=1,ndim
            tt(j,i)=tt(j,i)+t(k,i)*t(k,j)
          end do
        end do
      end do

      print *,'DPPTRF subroutine'
      print *,'*****************'
      print *,'Matrix T:             Matrix T*T (should be equal to C):'
      do i=1,ndim
        print 10,t(:,i),tt(:,i)
      end do
      print *,''

!     **************************************************************************

!     Stream initialization
      errcode=vslNewStream(stream,brng,seed)
      call CheckVslError(errcode)

!     Generating random numbers
!     from multivariate normal distribution
      errcode=vdRngGaussianMV(method,stream,n,r,ndim,me,a,t)
      call CheckVslError(errcode)

!     Printing random numbers
      print 11,' Results (first ',nn,' of ',n,')'
      print *,'--------------------------'

      do i=1,nn
        print 12,' r(',i,')=(',r(:,i),')'
      end do
      print *,''

      dbSX =0.0
      dbS2X=0.0
      dbSY =0.0
      dbS2Y=0.0
      dbSZ =0.0
      dbS2Z=0.0
      dbSXY=0.0
      dbSXZ=0.0
      dbSYZ=0.0

      do i=1,n
        dbSX  = dbSX  + r(1,i)
        dbS2X = dbS2X + r(1,i)*r(1,i)
        dbSY  = dbSY  + r(2,i)
        dbS2Y = dbS2Y + r(2,i)*r(2,i)
        dbSZ  = dbSZ  + r(3,i)
        dbS2Z = dbS2Z + r(3,i)*r(3,i)
        dbSXY = dbSXY + r(1,i)*r(2,i)
        dbSXZ = dbSXZ + r(1,i)*r(3,i)
        dbSYZ = dbSYZ + r(2,i)*r(3,i)
      end do

      dbMeanX = dbSX/n
      dbMeanY = dbSY/n
      dbMeanZ = dbSZ/n
      dbVarX  = dbS2X/n-dbMeanX*dbMeanX
      dbVarY  = dbS2Y/n-dbMeanY*dbMeanY
      dbVarZ  = dbS2Z/n-dbMeanZ*dbMeanZ
      dbCovXY = dbSXY/n-dbMeanX*dbMeanY
      dbCovXZ = dbSXZ/n-dbMeanX*dbMeanZ
      dbCovYZ = dbSYZ/n-dbMeanY*dbMeanZ

!     Printing
      print *,'Sample characteristics:'
      print *,'-----------------------'
      print *,'      Sample             Theory'
      print 13,' Mean :(',dbMeanX,dbMeanY,dbMeanZ,                      &
     &         ')  (',a(1),a(2),a(3),')'
      print 13,' Var. :(',dbVarX,dbVarY,dbVarZ,                         &
     &         ')  (',c(1,1),c(2,2),c(3,3),')'
      print 14,' CovXY: ',dbCovXY,'          ',c(1,2)
      print 14,' CovXZ: ',dbCovXZ,'          ',c(1,3)
      print 14,' CovYZ: ',dbCovYZ,'          ',c(2,3)
      print *,''

      DeltaMX = (a(1)-dbMeanX)/sqrt(c(1,1)/dn);
      DeltaMY = (a(2)-dbMeanY)/sqrt(c(2,2)/dn);
      DeltaMZ = (a(3)-dbMeanZ)/sqrt(c(3,3)/dn);

      DeltaDX = (c(1,1)-dbVarX)/sqrt(SX);
      DeltaDY = (c(2,2)-dbVarY)/sqrt(SY);
      DeltaDZ = (c(3,3)-dbVarZ)/sqrt(SZ);

      if (DeltaMX>3.0 .OR. DeltaDX>3.0 .OR.                             &
     &   DeltaMY>3.0 .OR. DeltaDY>3.0 .OR.                              &
     &   DeltaMZ>3.0 .OR. DeltaDZ>3.0) then
        print *,"Error: sample moments"
        print *,"are disagreed with theory"
        print 15, "    DeltaM: ", DeltaMX, DeltaMY, DeltaMZ
        print 15, "    DeltaD: ", DeltaDX, DeltaDY, DeltaDZ
        print *,  "   ( at least one of the Deltas > 3.0) "
        stop 1
      else
        print *,"Sample moments"
        print *,"are agreed with theory"
        print 15, "    DeltaM: ", DeltaMX, DeltaMY, DeltaMZ
        print 15, "    DeltaD: ", DeltaDX, DeltaDY, DeltaDZ
        print *,  "   ( all Deltas < 3.0) "
      end if
      print *,''

!     Stream finalization
      errcode=vslDeleteStream(stream)
      call CheckVslError(errcode)

      end
