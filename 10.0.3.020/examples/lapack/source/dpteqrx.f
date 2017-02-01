*     DPTEQR (F08JGF) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDZ
      PARAMETER        (NMAX=8,LDZ=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, N
*     .. Local Arrays ..
      DOUBLE PRECISION D(NMAX), E(NMAX-1), WORK(4*NMAX-4), Z(LDZ,NMAX)
*     .. External Subroutines ..
      EXTERNAL         DPTEQR, X04CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'DPTEQR Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read T from data file
*
         READ (NIN,*) (D(I),I=1,N)
         READ (NIN,*) (E(I),I=1,N-1)
*
*        Calculate all the eigenvalues and eigenvectors of T
*
         CALL DPTEQR('I',N,D,E,Z,LDZ,WORK,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.GT.0 .AND. INFO.LE.N) THEN
            WRITE (NOUT,*) 'T is not positive-definite.'
         ELSE IF (INFO.GT.N) THEN
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
            CALL X04CAF('General',' ',N,N,Z,LDZ,'Eigenvectors',IFAIL)
*
         END IF
      END IF
      STOP
*
99999 FORMAT (3X,(8F8.4))
      END
