*     ZSYTRF (F07NRF) Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LWORK
      PARAMETER        (NMAX=8,LDA=NMAX,LWORK=64*NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, N
      CHARACTER        UPLO
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), WORK(LWORK)
      INTEGER          IPIV(NMAX)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         ZSYTRF, X04DBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'ZSYTRF Example Program Results'
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
*        Factorize A
*
         CALL ZSYTRF(UPLO,N,A,LDA,IPIV,WORK,LWORK,INFO)
*
         WRITE (NOUT,*)
*
*        Print details of factorization
*
         IFAIL = 0
         CALL X04DBF(UPLO,'Nonunit',N,N,A,LDA,'Bracketed','F7.4',
     +               'Details of factorization','Integer',RLABS,
     +               'Integer',CLABS,80,0,IFAIL)
*
*        Print pivot indices
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'IPIV'
         WRITE (NOUT,99999) (IPIV(I),I=1,N)
*
         IF (INFO.NE.0) WRITE (NOUT,*) 'The factor D is singular'
*
      END IF
      STOP
*
99999 FORMAT ((1X,I12,3I18))
      END
