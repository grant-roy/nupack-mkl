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
!       MKL Cluster DFT example support functions (Fortran-interface)
!
!*******************************************************************************

      SUBROUTINE Dfti_Example_Status_Print(STATUS)

      USE MKL_DFTI

      INTEGER STATUS
      CHARACTER(DFTI_MAX_MESSAGE_LENGTH) Error_Message
      LOGICAL Class_Error

      Class_Error = DftiErrorClass(STATUS, DFTI_ERROR_CLASS)
      IF (.NOT. Class_Error) THEN
       PRINT *,' Status is not a member of Predefined Error Class'
      ELSE
       Error_Message = DftiErrorMessage(STATUS)
       PRINT *, ' Error_Message = ', Error_Message
      ENDIF

      END SUBROUTINE

      SUBROUTINE PRINT_DATA_2D_Z(X, M, N, C)

       COMPLEX(8) X(:,:)
       INTEGER M, N, C, I, J

        DO J = 1, N
         PRINT '(/" Row ",I3,":"/" "\)', J
         DO I = 1, M
	      PRINT '("(",F8.3,",",F8.3,")"\)',REAL(X(I,J)),AIMAG(X(I,J))
	      IF ((MOD(I,C).EQ.0).AND.(I.NE.M)) PRINT '(/" "\)'
	     END DO
        END DO
        PRINT *

      END SUBROUTINE

      SUBROUTINE PRINT_DATA_2D_C(X, M, N, C)

       COMPLEX(4) X(:,:)
       INTEGER M, N, C, I, J

       DO J = 1, N
		PRINT '(/" Row ",I3,":"/" "\)', J
        DO I = 1, M
			PRINT '("(",F8.3,",",F8.3,")"\)',REAL(X(I,J)),AIMAG(X(I,J))
			IF ((MOD(I,C).EQ.0).AND.(I.NE.M)) PRINT '(/" "\)'
		END DO
       END DO
       PRINT *

      END SUBROUTINE

      INTEGER FUNCTION MKL_CDFT_DATA_S(COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS,GLOBAL,NX,START_X,LOCAL,FLAG)

       IMPLICIT NONE

       INCLUDE 'mpif.h'

! MPI related integer data should all be INTEGER(4)
       INTEGER(4), ALLOCATABLE :: COUNTS(:),DISPLS(:),BUF(:)
       INTEGER(4) I,TMP(2),REQ,STAT(MPI_STATUS_SIZE)
       INTEGER(4) COMM,ROOTRANK,NPROC,NRANK,MPI_ERR

       INTEGER ELEMENTSIZE,DIM,LENGTHS(*),NX,START_X,FLAG
       COMPLEX(4) GLOBAL(*),LOCAL(*)
       INTENT(IN) COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS,NX,START_X,FLAG

       INTEGER FD

       CALL MPI_COMM_RANK(COMM,NRANK,MPI_ERR)
       IF (MPI_ERR/=MPI_SUCCESS) GOTO 100
       IF (NRANK==ROOTRANK) THEN
        CALL MPI_COMM_SIZE(COMM,NPROC,MPI_ERR)
        IF (MPI_ERR/=MPI_SUCCESS) GOTO 100
        ALLOCATE(COUNTS(NPROC),DISPLS(NPROC),BUF(2*NPROC))
       END IF

       FD=1
       DO I=1,DIM-1
        FD=FD*LENGTHS(I)
       END DO

       TMP(1)=NX*FD*ELEMENTSIZE
       TMP(2)=(START_X-1)*FD

       CALL MPI_GATHER(TMP,2,MPI_INTEGER,BUF,2,MPI_INTEGER,ROOTRANK,COMM,MPI_ERR)
       IF (MPI_ERR/=MPI_SUCCESS) GOTO 100

       IF (NRANK==ROOTRANK) THEN
        COUNTS=BUF(1:2*NPROC-1:2)
        DISPLS=BUF(2:2*NPROC:2)
       END IF

       IF (FLAG==0) THEN

        CALL MPI_IRECV(LOCAL,TMP(1),MPI_BYTE,ROOTRANK,123,COMM,REQ,MPI_ERR)
        IF (MPI_ERR/=MPI_SUCCESS) GOTO 100

        IF (NRANK==ROOTRANK) THEN
	     DO I=0,NPROC-1
              CALL MPI_SEND(GLOBAL(DISPLS(I+1)+1),COUNTS(I+1),MPI_BYTE,I,123,COMM,MPI_ERR)
	      IF (MPI_ERR/=MPI_SUCCESS) GOTO 100
	     END DO
        END IF

        CALL MPI_WAIT(REQ,STAT,MPI_ERR)
        IF (MPI_ERR/=MPI_SUCCESS) GOTO 100
       ENDIF

       IF (FLAG==1) THEN
        CALL MPI_ISEND(LOCAL,TMP(1),MPI_BYTE,ROOTRANK,222,COMM,REQ,MPI_ERR)
        IF (MPI_ERR/=MPI_SUCCESS) GOTO 100

        IF (NRANK==ROOTRANK) THEN
         DO I=0,NPROC-1
          CALL MPI_RECV(GLOBAL(DISPLS(I+1)+1),COUNTS(I+1),MPI_BYTE,I,222,COMM,STAT,MPI_ERR)
          IF (MPI_ERR/=MPI_SUCCESS) GOTO 100
         END DO
	END IF
	CALL MPI_WAIT(REQ,STAT,MPI_ERR)
       END IF

       IF (NRANK==ROOTRANK) DEALLOCATE(COUNTS,DISPLS,BUF)

100    MKL_CDFT_DATA_S=MPI_ERR

      END FUNCTION

      INTEGER FUNCTION MKL_CDFT_DATA_D(COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS,GLOBAL,NX,START_X,LOCAL,FLAG)

       IMPLICIT NONE

       INCLUDE 'mpif.h'

       INTEGER COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS(*),NX,START_X,FLAG
       COMPLEX(8) GLOBAL(*),LOCAL(*)
       INTENT(IN) COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS,NX,START_X,FLAG

       INTEGER(4) NPROC,NRANK,MPI_ERR
       INTEGER I,FD,TMP(2),REQ,STAT(MPI_STATUS_SIZE)
       INTEGER(4), ALLOCATABLE :: COUNTS(:),DISPLS(:),BUF(:)

       CALL MPI_COMM_RANK(COMM,NRANK,MPI_ERR)
       IF (MPI_ERR/=MPI_SUCCESS) GOTO 100

       IF (NRANK==ROOTRANK) THEN
        CALL MPI_COMM_SIZE(COMM,NPROC,MPI_ERR)
        IF (MPI_ERR/=MPI_SUCCESS) GOTO 100
        ALLOCATE(COUNTS(NPROC),DISPLS(NPROC),BUF(2*NPROC))
       END IF

       FD=1
       DO I=1,DIM-1
        FD=FD*LENGTHS(I)
       END DO

       TMP(1)=NX*FD*ELEMENTSIZE
       TMP(2)=(START_X-1)*FD

       CALL MPI_GATHER(TMP,2,MPI_INTEGER,BUF,2,MPI_INTEGER,ROOTRANK,COMM,MPI_ERR)
       IF (MPI_ERR/=MPI_SUCCESS) GOTO 100

       IF (NRANK==ROOTRANK) THEN
        COUNTS=BUF(1:2*NPROC-1:2)
        DISPLS=BUF(2:2*NPROC:2)
       END IF

       IF (FLAG==0) THEN

        CALL MPI_IRECV(LOCAL,TMP(1),MPI_BYTE,ROOTRANK,123,COMM,REQ,MPI_ERR)
        IF (MPI_ERR/=MPI_SUCCESS) GOTO 100

        IF (NRANK==ROOTRANK) THEN
	     DO I=0,NPROC-1
     	      CALL MPI_SEND(GLOBAL(DISPLS(I+1)+1),COUNTS(I+1),MPI_BYTE,I,123,COMM,MPI_ERR)
	      IF (MPI_ERR/=MPI_SUCCESS) GOTO 100
	     END DO
        END IF

        CALL MPI_WAIT(REQ,STAT,MPI_ERR)
        IF (MPI_ERR/=MPI_SUCCESS) GOTO 100
       ENDIF

       IF (FLAG==1) THEN

        CALL MPI_ISEND(LOCAL,TMP(1),MPI_BYTE,ROOTRANK,222,COMM,REQ,MPI_ERR)
        IF (MPI_ERR/=MPI_SUCCESS) GOTO 100

        IF (NRANK==ROOTRANK) THEN
         DO I=0,NPROC-1
          CALL MPI_RECV(GLOBAL(DISPLS(I+1)+1),COUNTS(I+1),MPI_BYTE,I,222,COMM,STAT,MPI_ERR)
          IF (MPI_ERR/=MPI_SUCCESS) GOTO 100
         END DO
	END IF
	CALL MPI_WAIT(REQ,STAT,MPI_ERR)
        IF (MPI_ERR/=MPI_SUCCESS) GOTO 100

       END IF

       IF (NRANK==ROOTRANK) DEALLOCATE(COUNTS,DISPLS,BUF)
100    MKL_CDFT_DATA_D=MPI_ERR

      END FUNCTION

      INTEGER FUNCTION MKL_CDFT_SCATTERDATA_S(COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS,GLOBAL_IN,NX,START_X,LOCAL_IN)

       INTEGER COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS(*),NX,START_X
       COMPLEX(4) GLOBAL_IN(*),LOCAL_IN(*)

       MKL_CDFT_SCATTERDATA_S=MKL_CDFT_DATA_S(COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS,GLOBAL_IN,NX,START_X,LOCAL_IN,0)

      END FUNCTION

      INTEGER FUNCTION MKL_CDFT_SCATTERDATA_D(COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS,GLOBAL_IN,NX,START_X,LOCAL_IN)

       INTEGER COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS(*),NX,START_X
       COMPLEX(8) GLOBAL_IN(*),LOCAL_IN(*)

       MKL_CDFT_SCATTERDATA_D=MKL_CDFT_DATA_D(COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS,GLOBAL_IN,NX,START_X,LOCAL_IN,0)

      END FUNCTION

      INTEGER FUNCTION MKL_CDFT_GATHERDATA_S(COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS,GLOBAL_OUT,NX,START_X,LOCAL_OUT)

       INTEGER COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS(*),NX,START_X
       COMPLEX(4) GLOBAL_OUT(*),LOCAL_OUT(*)

       MKL_CDFT_GATHERDATA_S=MKL_CDFT_DATA_S(COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS,GLOBAL_OUT,NX,START_X,LOCAL_OUT,1)

      END FUNCTION

      INTEGER FUNCTION MKL_CDFT_GATHERDATA_D(COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS,GLOBAL_OUT,NX,START_X,LOCAL_OUT)

       INTEGER COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS(*),NX,START_X
       COMPLEX(8) GLOBAL_OUT(*),LOCAL_OUT(*)

       MKL_CDFT_GATHERDATA_D=MKL_CDFT_DATA_D(COMM,ROOTRANK,ELEMENTSIZE,DIM,LENGTHS,GLOBAL_OUT,NX,START_X,LOCAL_OUT,1)

      END FUNCTION

