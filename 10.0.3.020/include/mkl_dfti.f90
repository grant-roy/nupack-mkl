!*****************************************************************************
!                            INTEL CONFIDENTIAL
! Copyright(C) 2002-2008 Intel Corporation. All Rights Reserved.
! The source code contained  or  described herein and all documents related to
! the source code ("Material") are owned by Intel Corporation or its suppliers
! or licensors.  Title to the  Material remains with  Intel Corporation or its
! suppliers and licensors. The Material contains trade secrets and proprietary
! and  confidential  information of  Intel or its suppliers and licensors. The
! Material  is  protected  by  worldwide  copyright  and trade secret laws and
! treaty  provisions. No part of the Material may be used, copied, reproduced,
! modified, published, uploaded, posted, transmitted, distributed or disclosed
! in any way without Intel's prior express written permission.
! No license  under any  patent, copyright, trade secret or other intellectual
! property right is granted to or conferred upon you by disclosure or delivery
! of the Materials,  either expressly, by implication, inducement, estoppel or
! otherwise.  Any  license  under  such  intellectual property  rights must be
! express and approved by Intel in writing.
!
!*****************************************************************************
! Content:
!    Intel(R) Math Kernel Library (MKL)
!    Discrete Fourier Transform Interface (DFTI)
!*****************************************************************************

MODULE MKL_DFT_TYPE

  TYPE, PUBLIC :: DFTI_DESCRIPTOR
     PRIVATE
     INTEGER :: dontuse
     ! Structure of this type is not used in Fortran code
     ! the pointer to this type is used only
  END TYPE DFTI_DESCRIPTOR

  !======================================================================
  ! Descriptor configuration parameters [default values in brackets]
  !======================================================================

  ! Domain for forward transform. No default value
  INTEGER, PARAMETER :: DFTI_FORWARD_DOMAIN = 0

  ! Dimensionality, or rank. No default value
  INTEGER, PARAMETER :: DFTI_DIMENSION = 1

  ! Length(s) of transform. No default value
  INTEGER, PARAMETER :: DFTI_LENGTHS = 2

  ! Floating point precision. No default value
  INTEGER, PARAMETER :: DFTI_PRECISION = 3

  ! Scale factor for forward transform [1.0]
  INTEGER, PARAMETER :: DFTI_FORWARD_SCALE = 4

  ! Scale factor for backward transform [1.0]
  INTEGER, PARAMETER :: DFTI_BACKWARD_SCALE = 5

  ! Exponent sign for forward transform [DFTI_NEGATIVE]
  ! INTEGER, PARAMETER :: DFTI_FORWARD_SIGN = 6 ! NOT IMPLEMENTED

  ! Number of data sets to be transformed [1]
  INTEGER, PARAMETER :: DFTI_NUMBER_OF_TRANSFORMS = 7

  ! Storage of finite complex-valued sequences in complex domain
  ! [DFTI_COMPLEX_COMPLEX]
  INTEGER, PARAMETER :: DFTI_COMPLEX_STORAGE = 8

  ! Storage of finite real-valued sequences in real domain
  ! [DFTI_REAL_REAL]
  INTEGER, PARAMETER :: DFTI_REAL_STORAGE = 9

  ! Storage of finite complex-valued sequences in conjugate-even
  ! domain [DFTI_COMPLEX_REAL]
  INTEGER, PARAMETER :: DFTI_CONJUGATE_EVEN_STORAGE = 10

  ! Placement of result [DFTI_INPLACE]
  INTEGER, PARAMETER :: DFTI_PLACEMENT = 11

  ! Generalized strides for input data layout
  ! [tigth, col-major for Fortran]
  INTEGER, PARAMETER :: DFTI_INPUT_STRIDES = 12

  ! Generalized strides for output data layout
  ! [tigth, col-major for Fortran]
  INTEGER, PARAMETER :: DFTI_OUTPUT_STRIDES = 13

  ! Distance between first input elements for multiple transforms [0]
  INTEGER, PARAMETER :: DFTI_INPUT_DISTANCE = 14

  ! Distance between first output elements for multiple transforms [0]
  INTEGER, PARAMETER :: DFTI_OUTPUT_DISTANCE = 15

  ! Effort spent in initialization [DFTI_MEDIUM]
  ! INTEGER, PARAMETER :: DFTI_INITIALIZATION_EFFORT = 16 ! NOT IMPLEMENTED

  ! Use of workspace during computation [DFTI_ALLOW]
  ! INTEGER, PARAMETER :: DFTI_WORKSPACE = 17 ! NOT IMPLEMENTED

  ! Ordering of the result [DFTI_ORDERED]
  INTEGER, PARAMETER :: DFTI_ORDERING = 18

  ! Possible transposition of result [DFTI_NONE]
  INTEGER, PARAMETER :: DFTI_TRANSPOSE = 19

  ! User-settable descriptor name [""]
  INTEGER, PARAMETER :: DFTI_DESCRIPTOR_NAME = 20

  ! Packing format for DFTI_COMPLEX_REAL storage of finite
  ! conjugate-even sequences [DFTI_CCS_FORMAT]
  INTEGER, PARAMETER :: DFTI_PACKED_FORMAT = 21

  ! Commit status of the descriptor. Read-only parameter
  INTEGER, PARAMETER :: DFTI_COMMIT_STATUS = 22

  ! Version string for this DFTI implementation. Read-only parameter
  INTEGER, PARAMETER :: DFTI_VERSION = 23

  ! Ordering of the forward transform. Read-only parameter
  ! INTEGER, PARAMETER :: DFTI_FORWARD_ORDERING = 24 ! NOT IMPLEMENTED

  ! Ordering of the backward transform. Read-only parameter
  ! INTEGER, PARAMETER :: DFTI_BACKWARD_ORDERING = 25 ! NOT IMPLEMENTED

  ! Number of user threads that share the descriptor [1]
  INTEGER, PARAMETER :: DFTI_NUMBER_OF_USER_THREADS = 26

  !======================================================================
  ! Values of the descriptor configuration parameters
  !======================================================================

  ! DFTI_COMMIT_STATUS
  INTEGER, PARAMETER :: DFTI_COMMITTED = 30
  INTEGER, PARAMETER :: DFTI_UNCOMMITTED = 31

  ! DFTI_FORWARD_DOMAIN
  INTEGER, PARAMETER :: DFTI_COMPLEX = 32
  INTEGER, PARAMETER :: DFTI_REAL = 33
  ! INTEGER, PARAMETER :: DFTI_CONJUGATE_EVEN = 34 ! NOT IMPLEMENTED

  ! DFTI_PRECISION
  INTEGER, PARAMETER :: DFTI_SINGLE = 35
  INTEGER, PARAMETER :: DFTI_DOUBLE = 36

  ! DFTI_FORWARD_SIGN
  ! INTEGER, PARAMETER :: DFTI_NEGATIVE = 37 ! NOT IMPLEMENTED
  ! INTEGER, PARAMETER :: DFTI_POSITIVE = 38 ! NOT IMPLEMENTED

  ! DFTI_COMPLEX_STORAGE and DFTI_CONJUGATE_EVEN_STORAGE
  INTEGER, PARAMETER :: DFTI_COMPLEX_COMPLEX = 39
  INTEGER, PARAMETER :: DFTI_COMPLEX_REAL = 40

  ! DFTI_REAL_STORAGE
  INTEGER, PARAMETER :: DFTI_REAL_COMPLEX = 41
  INTEGER, PARAMETER :: DFTI_REAL_REAL = 42

  ! DFTI_PLACEMENT
  INTEGER, PARAMETER :: DFTI_INPLACE = 43 ! Result overwrites input
  INTEGER, PARAMETER :: DFTI_NOT_INPLACE  = 44 ! Have another place for result

  ! DFTI_INITIALIZATION_EFFORT
  ! INTEGER, PARAMETER :: DFTI_LOW = 45 ! NOT IMPLEMENTED
  ! INTEGER, PARAMETER :: DFTI_MEDIUM = 46 ! NOT IMPLEMENTED
  ! INTEGER, PARAMETER :: DFTI_HIGH = 47 ! NOT IMPLEMENTED

  ! DFTI_ORDERING
  INTEGER, PARAMETER :: DFTI_ORDERED = 48
  INTEGER, PARAMETER :: DFTI_BACKWARD_SCRAMBLED = 49
  ! INTEGER, PARAMETER :: DFTI_FORWARD_SCRAMBLED  = 50 ! NOT IMPLEMENTED

  ! Allow/avoid certain usages
  INTEGER, PARAMETER :: DFTI_ALLOW = 51 ! Allow transposition or workspace
  ! INTEGER, PARAMETER :: DFTI_AVOID = 52 ! NOT IMPLEMENTED
  INTEGER, PARAMETER :: DFTI_NONE = 53

  ! DFTI_PACKED_FORMAT
  ! (for storing congugate-even finite sequence in real array)
  INTEGER, PARAMETER :: DFTI_CCS_FORMAT = 54  ! Complex conjugate-symmetric
  INTEGER, PARAMETER :: DFTI_PACK_FORMAT = 55 ! Pack format for real DFT
  INTEGER, PARAMETER :: DFTI_PERM_FORMAT = 56 ! Perm format for real DFT
  INTEGER, PARAMETER :: DFTI_CCE_FORMAT = 57  ! Complex conjugate-even

  !======================================================================
  ! Error classes
  !======================================================================
  INTEGER, PARAMETER :: DFTI_NO_ERROR = 0
  INTEGER, PARAMETER :: DFTI_MEMORY_ERROR = 1
  INTEGER, PARAMETER :: DFTI_INVALID_CONFIGURATION = 2
  INTEGER, PARAMETER :: DFTI_INCONSISTENT_CONFIGURATION = 3
  INTEGER, PARAMETER :: DFTI_MULTITHREADED_ERROR = 4
  INTEGER, PARAMETER :: DFTI_BAD_DESCRIPTOR = 5
  INTEGER, PARAMETER :: DFTI_UNIMPLEMENTED = 6
  INTEGER, PARAMETER :: DFTI_MKL_INTERNAL_ERROR = 7
  INTEGER, PARAMETER :: DFTI_NUMBER_OF_THREADS_ERROR = 8
  INTEGER, PARAMETER :: DFTI_1D_LENGTH_EXCEEDS_INT32 = 9

  ! Maximum length of error string
  INTEGER, PARAMETER :: DFTI_MAX_MESSAGE_LENGTH = 40

  ! Maximum length of user-settable descriptor name
  INTEGER, PARAMETER :: DFTI_MAX_NAME_LENGTH = 10

  ! Maximum length of MKL version string
  INTEGER, PARAMETER :: DFTI_VERSION_LENGTH = 198

  ! (deprecated parameter)
  INTEGER, PARAMETER :: DFTI_ERROR_CLASS = 60

  !======================================================================
  ! These real type kind parameters are not for direct use
  !======================================================================

  INTEGER, PARAMETER :: DFTI_SPKP = SELECTED_REAL_KIND(6,37)
  INTEGER, PARAMETER :: DFTI_DPKP = SELECTED_REAL_KIND(15,307)

END MODULE MKL_DFT_TYPE

MODULE MKL_DFTI

  USE MKL_DFT_TYPE

  INTERFACE DftiCreateDescriptor

     FUNCTION dfti_create_descriptor_1d(desc, precision, domain, dim, length)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_create_descriptor_1d
       !MS$ATTRIBUTES REFERENCE :: precision
       !MS$ATTRIBUTES REFERENCE :: domain
       !MS$ATTRIBUTES REFERENCE :: dim
       !MS$ATTRIBUTES REFERENCE :: length
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_create_descriptor_1d
       INTEGER, INTENT(IN) :: precision
       INTEGER, INTENT(IN) :: domain
       INTEGER, INTENT(IN) :: dim, length
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_create_descriptor_1d

     FUNCTION dfti_create_descriptor_highd(desc, precision, domain, dim,length)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_create_descriptor_highd
       !MS$ATTRIBUTES REFERENCE :: precision
       !MS$ATTRIBUTES REFERENCE :: domain
       !MS$ATTRIBUTES REFERENCE :: dim
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_create_descriptor_highd
       INTEGER, INTENT(IN) :: precision
       INTEGER, INTENT(IN) :: domain
       INTEGER, INTENT(IN) :: dim
       INTEGER, INTENT(IN), DIMENSION(*) :: length
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_create_descriptor_highd

  END INTERFACE

  INTERFACE DftiCommitDescriptor

     FUNCTION dfti_commit_descriptor_external(desc)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_commit_descriptor_external
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_commit_descriptor_external
       TYPE(DFTI_DESCRIPTOR), POINTER ::desc
     END FUNCTION dfti_commit_descriptor_external

  END INTERFACE

  INTERFACE DftiSetValue

     FUNCTION dfti_set_value_intval(desc, OptName, IntVal)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_set_value_intval
       !MS$ATTRIBUTES REFERENCE :: OptName
       !MS$ATTRIBUTES REFERENCE :: IntVal
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_set_value_intval
       INTEGER, INTENT(IN) :: OptName
       INTEGER, INTENT(IN) :: IntVal
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_set_value_intval

     FUNCTION dfti_set_value_sglval(desc, OptName, sglval)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_set_value_sglval
       !MS$ATTRIBUTES REFERENCE :: OptName
       !MS$ATTRIBUTES REFERENCE :: sglval
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_set_value_sglval
       INTEGER, INTENT(IN) :: OptName
       REAL(DFTI_SPKP), INTENT(IN) :: sglval
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_set_value_sglval

     FUNCTION dfti_set_value_dblval(desc, OptName, DblVal)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_set_value_dblval
       !MS$ATTRIBUTES REFERENCE :: OptName
       !MS$ATTRIBUTES REFERENCE :: DblVal
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_set_value_dblval
       INTEGER, INTENT(IN) :: OptName
       REAL(DFTI_DPKP), INTENT(IN) :: DblVal
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_set_value_dblval

     FUNCTION dfti_set_value_intvec(desc, OptName, IntVec)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_set_value_intvec
       !MS$ATTRIBUTES REFERENCE :: OptName
       !MS$ATTRIBUTES REFERENCE :: IntVec
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_set_value_intvec
       INTEGER, INTENT(IN) :: OptName
       INTEGER, INTENT(IN), DIMENSION(*) :: IntVec
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_set_value_intvec

     FUNCTION dfti_set_value_chars(desc, OptName, Chars)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_set_value_chars
       !MS$ATTRIBUTES REFERENCE :: OptName
       !MS$ATTRIBUTES REFERENCE :: Chars
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_set_value_chars
       INTEGER, INTENT(IN) :: OptName
       CHARACTER(*), INTENT(IN) :: Chars
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_set_value_chars

  END INTERFACE

  INTERFACE DftiGetValue

     FUNCTION dfti_get_value_intval(desc, OptName, IntVal)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_get_value_intval
       !MS$ATTRIBUTES REFERENCE :: OptName
       !MS$ATTRIBUTES REFERENCE :: IntVal
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_get_value_intval
       INTEGER, INTENT(IN) :: OptName
       INTEGER, INTENT(OUT) :: IntVal
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_get_value_intval

     FUNCTION dfti_get_value_sglval(desc, OptName, sglval)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_get_value_sglval
       !MS$ATTRIBUTES REFERENCE :: OptName
       !MS$ATTRIBUTES REFERENCE :: sglval
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_get_value_sglval
       INTEGER, INTENT(IN) :: OptName
       REAL(DFTI_SPKP), INTENT(OUT) :: sglval
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_get_value_sglval

     FUNCTION dfti_get_value_dblval(desc, OptName, DblVal)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_get_value_dblval
       !MS$ATTRIBUTES REFERENCE :: OptName
       !MS$ATTRIBUTES REFERENCE :: DblVal
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_get_value_dblval
       INTEGER, INTENT(IN) :: OptName
       REAL(DFTI_DPKP), INTENT(OUT) :: DblVal
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_get_value_dblval

     FUNCTION dfti_get_value_intvec(desc, OptName, IntVec)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_get_value_intvec
       !MS$ATTRIBUTES REFERENCE :: OptName
       !MS$ATTRIBUTES REFERENCE :: IntVec
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_get_value_intvec
       INTEGER, INTENT(IN) :: OptName
       INTEGER, INTENT(OUT), DIMENSION(*) :: IntVec
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_get_value_intvec

     FUNCTION dfti_get_value_chars(desc, OptName, Chars)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_get_value_chars
       !MS$ATTRIBUTES REFERENCE :: OptName
       !MS$ATTRIBUTES REFERENCE :: Chars
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_get_value_chars
       INTEGER, INTENT(IN) :: OptName
       CHARACTER(*), INTENT(OUT) :: Chars
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_get_value_chars

  END INTERFACE

  INTERFACE DftiComputeForward
     ! Compute FORWARD_DFT(X) --> Y

     FUNCTION dfti_compute_forward_c(desc, xy)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_forward_c
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: xy
       INTEGER dfti_compute_forward_c
       COMPLEX(DFTI_SPKP), INTENT(INOUT), DIMENSION(*) :: xy
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_forward_c

     FUNCTION dfti_compute_forward_z(desc, xy)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_forward_z
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: xy
       INTEGER dfti_compute_forward_z
       COMPLEX(DFTI_DPKP), INTENT(INOUT), DIMENSION(*) :: xy
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_forward_z

     FUNCTION dfti_compute_forward_c_out(desc, x, y)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_forward_c_out
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: x
       !MS$ATTRIBUTES REFERENCE :: y
       INTEGER dfti_compute_forward_c_out
       COMPLEX(DFTI_SPKP), INTENT(IN), DIMENSION(*) :: x
       COMPLEX(DFTI_SPKP), INTENT(OUT), DIMENSION(*) :: y
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_forward_c_out

     FUNCTION dfti_compute_forward_z_out(desc, x, y)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_forward_z_out
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: x
       !MS$ATTRIBUTES REFERENCE :: y
       INTEGER dfti_compute_forward_z_out
       COMPLEX(DFTI_DPKP), INTENT(IN), DIMENSION(*) :: x
       COMPLEX(DFTI_DPKP), INTENT(OUT), DIMENSION(*) :: y
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_forward_z_out

     FUNCTION dfti_compute_forward_s(desc, xy)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_forward_s
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: xy
       INTEGER dfti_compute_forward_s
       REAL(DFTI_SPKP), INTENT(INOUT), DIMENSION(*) :: xy
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_forward_s

     FUNCTION dfti_compute_forward_d(desc, xy)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_forward_d
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: xy
       INTEGER dfti_compute_forward_d
       REAL(DFTI_DPKP), INTENT(INOUT), DIMENSION(*) :: xy
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_forward_d

     FUNCTION dfti_compute_forward_s_out(desc, x, y)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_forward_s_out
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: x
       !MS$ATTRIBUTES REFERENCE :: y
       INTEGER dfti_compute_forward_s_out
       REAL(DFTI_SPKP), INTENT(IN), DIMENSION(*) :: x
       REAL(DFTI_SPKP), INTENT(OUT), DIMENSION(*) :: y
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_forward_s_out

     FUNCTION dfti_compute_forward_sc_out(desc, x, y)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_forward_sc_out
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: x
       !MS$ATTRIBUTES REFERENCE :: y
       INTEGER dfti_compute_forward_sc_out
       REAL(DFTI_SPKP), INTENT(IN), DIMENSION(*) :: x
       COMPLEX(DFTI_SPKP), INTENT(OUT), DIMENSION(*) :: y
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_forward_sc_out

     FUNCTION dfti_compute_forward_d_out(desc, x, y)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_forward_d_out
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: x
       !MS$ATTRIBUTES REFERENCE :: y
       INTEGER dfti_compute_forward_d_out
       REAL(DFTI_DPKP), INTENT(IN), DIMENSION(*) :: x
       REAL(DFTI_DPKP), INTENT(OUT), DIMENSION(*) :: y
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_forward_d_out

     FUNCTION dfti_compute_forward_dz_out(desc, x, y)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_forward_dz_out
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: x
       !MS$ATTRIBUTES REFERENCE :: y
       INTEGER dfti_compute_forward_dz_out
       REAL(DFTI_DPKP), INTENT(IN), DIMENSION(*) :: x
       COMPLEX(DFTI_DPKP), INTENT(OUT), DIMENSION(*) :: y
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_forward_dz_out

  END INTERFACE

  INTERFACE DftiComputeBackward
     ! Compute INVERSE_DFT(Y) --> X

     FUNCTION dfti_compute_backward_c(desc, yx)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_backward_c
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: yx
       INTEGER dfti_compute_backward_c
       COMPLEX(DFTI_SPKP), INTENT(INOUT), DIMENSION(*) :: yx
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_backward_c

     FUNCTION dfti_compute_backward_z(desc, yx)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_backward_z
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: yx
       INTEGER dfti_compute_backward_z
       COMPLEX(DFTI_DPKP), INTENT(INOUT), DIMENSION(*) :: yx
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_backward_z

     FUNCTION dfti_compute_backward_c_out(desc, y, x)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_backward_c_out
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: y
       !MS$ATTRIBUTES REFERENCE :: x
       INTEGER dfti_compute_backward_c_out
       COMPLEX(DFTI_SPKP), INTENT(IN), DIMENSION(*) :: y
       COMPLEX(DFTI_SPKP), INTENT(OUT), DIMENSION(*) :: x
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_backward_c_out

     FUNCTION dfti_compute_backward_z_out(desc, y, x)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_backward_z_out
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: y
       !MS$ATTRIBUTES REFERENCE :: x
       INTEGER dfti_compute_backward_z_out
       COMPLEX(DFTI_DPKP), INTENT(IN), DIMENSION(*) :: y
       COMPLEX(DFTI_DPKP), INTENT(OUT), DIMENSION(*) :: x
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_backward_z_out

     FUNCTION dfti_compute_backward_s(desc, yx)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_backward_s
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: yx
       INTEGER dfti_compute_backward_s
       REAL(DFTI_SPKP), INTENT(INOUT), DIMENSION(*) :: yx
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_backward_s

     FUNCTION dfti_compute_backward_d(desc, yx)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_backward_d
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: yx
       INTEGER dfti_compute_backward_d
       REAL(DFTI_DPKP), INTENT(INOUT), DIMENSION(*) :: yx
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_backward_d

     FUNCTION dfti_compute_backward_s_out(desc, y, x)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_backward_s_out
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: y
       !MS$ATTRIBUTES REFERENCE :: x
       INTEGER dfti_compute_backward_s_out
       REAL(DFTI_SPKP), INTENT(IN), DIMENSION(*) :: y
       REAL(DFTI_SPKP), INTENT(OUT), DIMENSION(*) :: x
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_backward_s_out

     FUNCTION dfti_compute_backward_cs_out(desc, y, x)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_backward_cs_out
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: y
       !MS$ATTRIBUTES REFERENCE :: x
       INTEGER dfti_compute_backward_cs_out
       COMPLEX(DFTI_SPKP), INTENT(IN), DIMENSION(*) :: y
       REAL(DFTI_SPKP), INTENT(OUT), DIMENSION(*) :: x
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_backward_cs_out

     FUNCTION dfti_compute_backward_d_out(desc, y, x)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_backward_d_out
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: y
       !MS$ATTRIBUTES REFERENCE :: x
       INTEGER dfti_compute_backward_d_out
       REAL(DFTI_DPKP), INTENT(IN), DIMENSION(*) :: y
       REAL(DFTI_DPKP), INTENT(OUT), DIMENSION(*) :: x
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_backward_d_out

     FUNCTION dfti_compute_backward_zd_out(desc, y, x)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_compute_backward_zd_out
       !MS$ATTRIBUTES REFERENCE :: desc
       !MS$ATTRIBUTES REFERENCE :: y
       !MS$ATTRIBUTES REFERENCE :: x
       INTEGER dfti_compute_backward_zd_out
       COMPLEX(DFTI_DPKP), INTENT(IN), DIMENSION(*) :: y
       REAL(DFTI_DPKP), INTENT(OUT), DIMENSION(*) :: x
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_compute_backward_zd_out

  END INTERFACE

  INTERFACE DftiFreeDescriptor

     FUNCTION dfti_free_descriptor_external(desc)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_free_descriptor_external
       !MS$ATTRIBUTES REFERENCE :: desc
       INTEGER dfti_free_descriptor_external
       TYPE(DFTI_DESCRIPTOR), POINTER :: desc
     END FUNCTION dfti_free_descriptor_external

  END INTERFACE

  INTERFACE DftiErrorClass

     FUNCTION dfti_error_class_external(Status, ErrorClass)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_error_class_external
       !MS$ATTRIBUTES REFERENCE :: Status
       !MS$ATTRIBUTES REFERENCE :: ErrorClass
       LOGICAL dfti_error_class_external
       INTEGER, INTENT(IN) :: Status
       INTEGER, INTENT(IN) :: ErrorClass
     END FUNCTION dfti_error_class_external

  END INTERFACE

  INTERFACE DftiErrorMessage

     FUNCTION dfti_error_message_external(Status)
       USE MKL_DFT_TYPE
       !DEC$ATTRIBUTES C :: dfti_error_message_external
       !MS$ATTRIBUTES REFERENCE :: Status
       CHARACTER(LEN=DFTI_MAX_MESSAGE_LENGTH) :: dfti_error_message_external
       INTEGER, INTENT(IN) :: Status
     END FUNCTION DFTI_ERROR_MESSAGE_EXTERNAL

  END INTERFACE

END MODULE MKL_DFTI
