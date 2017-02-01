*     DSYCON (F07MGF) Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LWORK
      PARAMETER        (NMAX=8,LDA=NMAX,LWORK=64*NMAX)
*     .. Local Scalars ..
      DOUBLE PRECISION ANORM, RCOND
      INTEGER          I, INFO, J, N
      CHARACTER        UPLO
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), WORK(LWORK)
      INTEGER          IPIV(NMAX), IWORK(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION F06RCF, X02AJF
      EXTERNAL         F06RCF, X02AJF
*     .. External Subroutines ..
      EXTERNAL         DSYCON, DSYTRF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'DSYCON Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) UPLO
         IF (UPLO.EQ.'U') THEN
            READ (NIN,*) ((A(I,J),J=I,N),I=1,N)
         ELSE IF (UPLO.EQ.'L') THEN
            READ (NIN,*) ((A(I,J),J=1,I),I=1,N)
         END IF
*
*        Compute norm of A
*
         ANORM = F06RCF('1-norm',UPLO,N,A,LDA,WORK)
*
*        Factorize A
*
         CALL DSYTRF(UPLO,N,A,LDA,IPIV,WORK,LWORK,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.EQ.0) THEN
*
*           Estimate condition number
*
            CALL DSYCON(UPLO,N,A,LDA,IPIV,ANORM,RCOND,WORK,IWORK,INFO)
*
            IF (RCOND.GE.X02AJF()) THEN
               WRITE (NOUT,99999) 'Estimate of condition number =',
     +           1.0D0/RCOND
            ELSE
               WRITE (NOUT,*) 'A is singular to working precision'
            END IF
         ELSE
            WRITE (NOUT,*) 'The factor D is singular'
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,1P,D10.2)
      END