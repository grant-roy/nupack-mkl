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
!   Content:
!       MKL Cluster DFT interface example program (Fortran-interface)
!
!       Forward-Backward 2D complex transform for single precision data.
!
!*******************************************************************************
!   Configuration parameters:
!           DFTI_FORWARD_DOMAIN = DFTI_COMPLEX                  (obligatory)
!           DFTI_PRECISION      = DFTI_SINGLE                   (obligatory)
!           DFTI_DIMENSION      = 2                             (obligatory)
!           DFTI_LENGTHS        = (M, N)                        (obligatory)
!           DFTI_FORWARD_SCALE  = 1.0                           (default)
!           DFTI_BACKWARD_SCALE = 1.0/(M*N)                     (default=1.0)
!
!*******************************************************************************

      PROGRAM DM_COMPLEX_2D_SINGLE_EX2

!      USE MKL_DFTI_DM
      USE MKL_CDFT

      INCLUDE 'mpif.h'

      INCLUDE 'mkl_cdft_examples.fi'


      COMPLEX(4), DIMENSION(:,:), ALLOCATABLE :: X_EXP
      COMPLEX(4), ALLOCATABLE :: X_IN(:,:), LOCAL(:), WORK(:)
      COMPLEX(4) qq
      COMPLEX(8) dummy_complex8
      INTEGER(4) dummy_integer4
      INTEGER dummy_integer

      TYPE(DFTI_DESCRIPTOR_DM), POINTER :: DESC

      INTEGER   NX,NX_OUT,START_X,START_X_OUT,SIZE,ROOTRANK,ELEMENTSIZE,I,J

      INTEGER   M, N
      INTEGER   STATUS
      REAL(8)   SCALE
      INTEGER   LENGTHS(2)

      REAL(4)   MAXERR
      REAL(4), PARAMETER :: EPS = SINGLE_EPS

      INTEGER(4) MPI_ERR
      INTEGER(4) MPI_NPROC
      INTEGER(4) MPI_RANK

!      1. Initiate MPI by calling MPI_Init (Perform MPI initialization)

      CALL MPI_INIT(MPI_ERR)

      IF (MPI_ERR .NE. MPI_SUCCESS) THEN
       PRINT *, 'MPI initialization error'
       PRINT *, 'TEST FAILED'
       STOP
      ENDIF

      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, MPI_NPROC, MPI_ERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, MPI_RANK, MPI_ERR)
      IF (MPI_RANK .EQ. 0) PRINT '(" Program is running on ",I2," processes"/)',MPI_NPROC

!
!     Read input parameters from input file
!     m - size of transform  along first dimension
!     n - size of transform  along second dimension
!
      IF (MPI_RANK .EQ. 0) THEN
       READ*
       READ*, M
       READ*, N
      ENDIF

      ELEMENTSIZE=sizeof(dummy_integer)/sizeof(dummy_integer4)
      CALL MPI_BCAST(M,ELEMENTSIZE,MPI_INTEGER,0,MPI_COMM_WORLD,MPI_ERR)
      CALL MPI_BCAST(N,ELEMENTSIZE,MPI_INTEGER,0,MPI_COMM_WORLD,MPI_ERR)

      IF (LEGEND_PRINT .AND. (MPI_RANK .EQ. 0)) THEN
          PRINT *, 'DM_COMPLEX_2D_SINGLE_EX2'
        PRINT *
          PRINT *, 'Forward-Backward 2D complex transform for single precision data'
          PRINT *
          PRINT *, 'Configuration parameters:'
          PRINT *
          PRINT *, 'DFTI_FORWARD_DOMAIN       = DFTI_COMPLEX'
          PRINT *, 'DFTI_PRECISION            = DFTI_SINGLE '
          PRINT *, 'DFTI_DIMENSION            =   2'
          PRINT '(" DFTI_LENGTHS              = (",I3,",",I3,")")', M, N
          PRINT *, 'DFTI_FORWARD_SCALE        = 1.0 '
          PRINT *, 'DFTI_BACKWARD_SCALE       = 1.0/(M*N)'
          PRINT *
      ENDIF

      LENGTHS(1) = M
      LENGTHS(2) = N

!
!     Allocate dynamic arrays and put input data
!
      ALLOCATE(X_IN(M,N), X_EXP(M,N))

      X_IN = 0.0
      X_IN(1,1) = 1.0
      X_EXP = X_IN
!
!     Put input data and expected result
!
        IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) THEN
        PRINT *,'INPUT Global vector X, N columns)'
        CALL PRINT_DATA_2D_C(X_IN, M, N, N)
      ENDIF
!
!      2. Allocate memory for the descriptor by calling DftiCreateDescriptorDM
!     Free DftiDM descriptor
!

       STATUS = DftiCreateDescriptorDM(MPI_COMM_WORLD,DESC,DFTI_SINGLE,DFTI_COMPLEX,2,LENGTHS)
      IF (MPI_RANK .EQ. 0) PRINT*,'CREATE=', STATUS
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 102
      END IF
!
!     3. Obtain some values of configuration parameters by calls to DftiGetValueDM
!
      STATUS = DftiGetValueDM(DESC,CDFT_LOCAL_SIZE,SIZE)
      IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'GET=',STATUS,',SIZE=',SIZE
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

      STATUS = DftiGetValueDM(DESC,CDFT_LOCAL_NX,NX)
      IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'GET=',STATUS,',NX=',NX
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

      STATUS = DftiGetValueDM(DESC,CDFT_LOCAL_X_START,START_X)
      IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'GET=',STATUS,',START_X=',START_X
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

      STATUS = DftiGetValueDM(DESC,CDFT_LOCAL_OUT_NX,NX_OUT)
      IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'GET=',STATUS,',NX_OUT=',NX_OUT
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

      STATUS = DftiGetValueDM(DESC,CDFT_LOCAL_OUT_X_START,START_X_OUT)
      IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'GET=',STATUS,',START_X_OUT=',START_X_OUT
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

      ALLOCATE(LOCAL(SIZE),WORK(SIZE))

!
!     4. Specify a value(s) of configuration parameters by a call(s) to DftiSetValueDM.
!
      STATUS = DftiSetValueDM(DESC,CDFT_WORKSPACE,WORK)
      IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'SET=',STATUS
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

      STATUS = DftiSetValueDM(DESC,DFTI_PLACEMENT,DFTI_NOT_INPLACE)
      IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'SET out-of-place=',STATUS
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF


!
!     5. Perform initialization that facilitates DFT computation by a call to
!           DftiCommitDescriptorDM.

      STATUS = DftiCommitDescriptorDM(DESC)
      IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'COMMIT=',STATUS
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

!
!     6. Create arrays for local parts of input and output data (if it is needed) and fill the local part of input data with
!          values (for more information, see Distributing Data among Processes).

      ROOTRANK=0
        ELEMENTSIZE=sizeof(qq)
        STATUS = MKL_CDFT_SCATTERDATA_S(MPI_COMM_WORLD,ROOTRANK,ELEMENTSIZE,2,LENGTHS,X_IN,NX,START_X,LOCAL)
        IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'Scatter=',STATUS
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

!
!     7. Compute the transform by calling DftiComputeForwardDM or DftiComputeBackward.
!      (Compute Forward transform)
        STATUS = DftiComputeForwardDM(DESC,LOCAL,WORK)
        IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'ComputeForward=',STATUS
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

!
!       Gather data among processors
!
      STATUS = MKL_CDFT_GATHERDATA_S(MPI_COMM_WORLD,ROOTRANK,ELEMENTSIZE,2,LENGTHS,X_IN,NX,START_X,WORK)
        IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'GATHER=',STATUS
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

!
!    Set Scale number for Backward transform
!
        SCALE = 1.0/(N*M)
        IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0))THEN
        PRINT *
          PRINT *,'DFTI_BACKWARD_SCALE  = 1/(M*N)'
       ENDIF
        STATUS = DftiSetValueDM(DESC,DFTI_BACKWARD_SCALE,SCALE)
        IF (MPI_RANK .EQ. 0) PRINT*,'Set=',STATUS
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF
!
!     Commit DftiDM descriptor
!
      STATUS = DftiCommitDescriptorDM(DESC)
      IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'COMMIT=',STATUS
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

!
!    Spread data among processors
!
      STATUS = MKL_CDFT_SCATTERDATA_S(MPI_COMM_WORLD,ROOTRANK,ELEMENTSIZE,2,LENGTHS,X_IN,NX,START_X,WORK)
        IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'Scatter=',STATUS
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

!
!      Compute Backward transform
!
        STATUS = DftiComputeBackwardDM(DESC,WORK,LOCAL)
        IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'ComputeBackward=',STATUS
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

!
!      8. Gather local output data into the global array using MPI functions or use them otherwise.
!
      STATUS = MKL_CDFT_GATHERDATA_S(MPI_COMM_WORLD,ROOTRANK,ELEMENTSIZE,2,LENGTHS,X_IN,NX,START_X,LOCAL)
        IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'GATHER=',STATUS
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
       GOTO 101
      END IF

!
!    print data after DftiComputeBackwardDM; data assembled together
!
        IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) THEN
        PRINT *,'Backward OUTPUT vector X, N columns)'
        CALL PRINT_DATA_2D_C(X_IN, M, N, N)
      ENDIF

!
!    Check result
!
        MAXERR = MAXVAL(ABS(X_IN - X_EXP))
      IF (ACCURACY_PRINT .AND. (MPI_RANK .EQ. 0)) THEN
        PRINT *
        PRINT '(" ACCURACY = ",G15.6,/)', MAXERR
      ENDIF

      IF (MPI_RANK .EQ. 0) THEN
       IF (MAXERR .LT. EPS) THEN
        PRINT *, 'TEST PASSED'
       ELSE
        PRINT *, 'TEST FAILED'
       ENDIF
      ENDIF

!
!       9. Release memory allocated for a descriptor by a call to DftiFreeDescriptorDM.
!            (Free DftiDM descriptor)
!
101     STATUS =DftiFreeDescriptorDM(DESC)
      IF(ADVANCED_DATA_PRINT .AND. (MPI_RANK .EQ. 0)) PRINT*,'FreeDescriptor=',STATUS
      IF ( STATUS .NE. DFTI_NO_ERROR) THEN
       IF (MPI_RANK .EQ. 0) THEN
       CALL Dfti_Example_Status_Print(STATUS)
       PRINT *, 'TEST FAILED'
       ENDIF
      END IF


!
!     Free memory for dynamic arrays
!
102   DEALLOCATE(X_IN, X_EXP, LOCAL, WORK)

!
!     Finalize MPI
!
103   CALL MPI_FINALIZE(MPI_ERR)

      END PROGRAM
