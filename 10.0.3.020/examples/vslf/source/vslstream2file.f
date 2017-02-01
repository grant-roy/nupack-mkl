!*******************************************************************************
!                             INTEL CONFIDENTIAL
!  Copyright(C) 2003-2008 Intel Corporation. All Rights Reserved.
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
!    stream2file functions  Example Program Text
!*******************************************************************************

      include 'mkl_vsl.fi'
      include "errcheck.inc"

      program MKL_VSL_TEST

      use MKL_VSL_TYPE
      USE MKL_VSL

      type (VSL_STREAM_STATE) :: stream
      integer N
      parameter (N=10)
      integer brng,method,seed

      real r_orig(N)
      real r_load(N)

      integer(kind=4) i, nerrors
      integer(kind=4) errcode

      brng=VSL_BRNG_R250
      seed=777
      nerrors=0

!     **** Create the original stream to be saved in a file ****
      errcode = vslnewstream( stream, brng, seed )
      call CheckVslError(errcode)

!     **** Save original stream to a file ****
      errcode = vslsavestreamf(stream, "vslstream2file.dat");
      call CheckVslError(errcode)


!     **** Generate random numbers using original stream ****
      errcode = vsrnguniform(0, stream, N, r_orig, 0.0, 1.0)
      call CheckVslError(errcode)

!     **** Delete original stream ****
      errcode = vsldeletestream(stream)
      call CheckVslError(errcode)

!     **** Load stream that is saved in a file ****
      errcode = vslLoadStreamF(stream, "vslstream2file.dat")
      call CheckVslError(errcode)

!     **** Generate random numbers using the stream loaded from file ****
      errcode = vsrnguniform(0, stream, N, r_load, 0.0, 1.0)
      call CheckVslError(errcode)

!     **** Delete stream loaded from file ****
      errcode = vsldeletestream(stream)
      call CheckVslError(errcode)

!     **** Compare random numbers from original and loaded stream. ****
!     **** Must be identical                                       ****
      do i=1,N
          print '(A,I2,A,F7.5,A,I2,A,F7.5)'," r_orig[",i,"]=",          &
     &          r_orig(i),"     r_load[",i,"]=",r_load(i)
          if ( r_orig(i) /= r_load(i) ) then
!             **** Here if results are not identical ****
              nerrors = nerrors + 1
          end if
      end do

      if (nerrors > 0) then
!         **** Here if results are not identical ****
          print *, "ERROR: Loaded stream differs from original stream."
          stop 1
      else
!         **** Here if results are identical ****
          print *, "PASS: Loaded stream identical with original stream."
      end if

      end program
