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
!    viRngPoisson, abstract BRNG  Example Program Text
!*******************************************************************************
      include 'mkl_vsl.fi'
      include "errcheck.inc"

      program MKL_VSL_ABSTRACT_STREAM

      USE MKL_VSL_TYPE
      USE MKL_VSL

      TYPE (VSL_STREAM_STATE) :: stream
      TYPE (VSL_STREAM_STATE) :: astream

      INTEGER(KIND=4) bufn, printn, i, dif, errcode
      INTEGER brng, method, seed
      INTEGER n

      PARAMETER (bufn=10)
      PARAMETER (n=25)

      REAL(KIND=4) a,b
      REAL(KIND=8) lambda
      REAL(KIND=4) buf(bufn)
      INTEGER(KIND=4) r(n)
      INTEGER(KIND=4) rcpy(n)
      CHARACTER(len=100)  filename

      INTERFACE
        INTEGER(KIND=4) FUNCTION supdate[C] (stream,n,buf,nmin,nmax,idx)
          USE MKL_VSL_TYPE
          TYPE(VSL_STREAM_STATE)         :: stream[reference]
          INTEGER(KIND=4),INTENT(IN)     :: n[reference]
          REAL(KIND=4),   INTENT(OUT)    :: buf[reference](0:n-1)
          INTEGER(KIND=4),INTENT(IN)     :: nmin[reference]
          INTEGER(KIND=4),INTENT(IN)     :: nmax[reference]
          INTEGER(KIND=4),INTENT(IN)     :: idx[reference]
        END FUNCTION
      END INTERFACE


      brng = VSL_BRNG_MCG31
      method = VSL_METHOD_IPOISSON_PTPE
      seed = 123
      printn = 10

      a = 0.0
      b = 1.0
      lambda = 30.0

      read  '(A)',filename
      print *,filename

      open(10, FILE=filename, STATUS='OLD')

!     ***** Initialize *****
      errcode = vslnewstream( stream, brng, seed )
      call CheckVslError(errcode)
      errcode = vslsnewabstractstream( astream, bufn, buf, a, b,        &
     &                                 supdate )
      call CheckVslError(errcode)


!     ***** Call RNG *****
      errcode = virngpoisson( method, stream, n, r, lambda )
      call CheckVslError(errcode)
      errcode = virngpoisson( method, astream, n, rcpy, lambda )
      call CheckVslError(errcode)

      print *, "Abstract BRNG for single precision arrays was used"
      print *, "for generation of rcpy array of Poisson distributed"
      print *, "random numbers"
      print *, ""


!     ***** Check results *****
      dif = 0
      do i=1,n
        if (r(i) .ne. rcpy(i)) then
            dif = dif + 1
        end if
      end do

      if  ( dif .eq. 0 ) then
            print *, 'arrays are identical'
      else
            print *, 'arrays are not identical, dif=',dif
      end if

      print *, ''

!     ***** Printing results *****

      print *,"Sample of virngpoisson."
      print *,"-----------------------"
      print *,""
      print *,"Parameter:"
      print 11," lambda=",lambda

      print *,""
      print *,"Results (first 10 of 25):"
      print *,"---------------------------"
      print *,"       array r array rcpy\n"
      print *,"---------------------------"

      do i=1,printn
        print *, r(i),     rcpy(i)
      end do

      print *,""


!     ***** Deinitialize *****
      errcode = vsldeletestream( stream )
      call CheckVslError(errcode)
      errcode = vsldeletestream( astream )
      call CheckVslError(errcode)

      close(10)

11    format(A,F7.3)

      end program MKL_VSL_ABSTRACT_STREAM

      INTEGER(KIND=4) FUNCTION supdate[C] (stream,n,buf,nmin,nmax,idx)
         USE MKL_VSL_TYPE
         TYPE(VSL_STREAM_STATE)          :: stream[reference]
         INTEGER(KIND=4),INTENT(IN)      :: n[reference]
         REAL(KIND=4),   INTENT(OUT)     :: buf[reference](0:n-1)
         INTEGER(KIND=4),INTENT(IN)      :: nmin[reference]
         INTEGER(KIND=4),INTENT(IN)      :: nmax[reference]
         INTEGER(KIND=4),INTENT(IN)      :: idx[reference]


         integer(kind=4) cnt,i, total
         real(kind=4) c, num

         c = 1.0 / 2147483647.0
         cnt = idx
         total = 0
         do i=1,nmax
          read (10,*, end=12) num
          buf(cnt) = num * c
          cnt = mod(cnt+1, n)
          total = total + 1
         end do

12      supdate = total

      END FUNCTION
