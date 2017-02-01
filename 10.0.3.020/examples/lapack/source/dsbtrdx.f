*     DSBTRD (F08HEF) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, KMAX, LDAB, LDQ
      PARAMETER        (NMAX=8,KMAX=8,LDAB=KMAX+1,LDQ=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, KD, N
      CHARACTER        UPLO
*     .. Local Arrays ..
      DOUBLE PRECISION AB(LDAB,NMAX), D(NMAX), E(NMAX-1), Q(LDQ,NMAX),
     +                 WORK(2*NMAX-2)
*     .. External Subroutines ..
      EXTERNAL         DSBTRD, DSTEQR, X04CAF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'DSBTRD Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, KD
      IF (N.LE.NMAX .AND. KD.LE.KMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) UPLO
         IF (UPLO.EQ.'U') THEN
            DO 20 I = 1, N
               READ (NIN,*) (AB(KD+1+I-J,J),J=I,MIN(N,I+KD))
   20       CONTINUE
         ELSE IF (UPLO.EQ.'L') THEN
            DO 40 I = 1, N
               READ (NIN,*) (AB(1+I-J,J),J=MAX(1,I-KD),I)
   40       CONTINUE
         END IF
*
*        Reduce A to tridiagonal form T = (Q**T)*A*Q (and form Q)
*
         CALL DSBTRD('V',UPLO,N,KD,AB,LDAB,D,E,Q,LDQ,WORK,INFO)
*
*        Calculate all the eigenvalues and eigenvectors of A
*
         CALL DSTEQR('V',N,D,E,Q,LDQ,WORK,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.GT.0) THEN
            WRITE (NOUT,*) 'Failure to converge.'
         ELSE
*
*           Print eigenvalues and eigenvectors
*
            WRITE (NOUT,*) 'Eigenvalues'
            WRITE (NOUT,99999) (D(I),I=1,N)
            WRITE (NOUT,*)
            IFAIL = 0
*
            CALL X04CAF('General',' ',N,N,Q,LDQ,'Eigenvectors',IFAIL)
*
         END IF
      END IF
      STOP
*
99999 FORMAT (3X,(8F8.4))
      END
