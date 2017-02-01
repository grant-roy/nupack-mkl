*     ZTRSEN (F08QUF) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDT, LDQ, LWORK
      PARAMETER        (NMAX=8,LDT=NMAX,LDQ=NMAX,LWORK=NMAX*NMAX/2)
*     .. Local Scalars ..
      DOUBLE PRECISION S, SEP
      INTEGER          I, IFAIL, INFO, J, M, N
*     .. Local Arrays ..
      COMPLEX*16       Q(LDQ,NMAX), T(LDT,NMAX), W(NMAX), WORK(LWORK)
      LOGICAL          SELECT(NMAX)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         X04DBF, ZTRSEN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'ZTRSEN Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read T and Q from data file
*
         READ (NIN,*) ((T(I,J),J=1,N),I=1,N)
         READ (NIN,*)
         READ (NIN,*) ((Q(I,J),J=1,N),I=1,N)
         READ (NIN,*)
*
         READ (NIN,*) (SELECT(I),I=1,N)
*
*        Reorder the Schur factorization
*
         CALL ZTRSEN('Both','Vectors',SELECT,N,T,LDT,Q,LDQ,W,M,S,SEP,
     +               WORK,LWORK,INFO)
*
         WRITE (NOUT,*)
         IFAIL = 0
*
         CALL X04DBF('General',' ',N,N,T,LDT,'Bracketed','F7.4',
     +               'Reordered Schur form','Integer',RLABS,'Integer',
     +               CLABS,80,0,IFAIL)
*
         WRITE (NOUT,*)
         IFAIL = 0
*
         CALL X04DBF('General',' ',N,M,Q,LDQ,'Bracketed','F7.4',
     +               'Basis of invariant subspace','Integer',RLABS,
     +               'Integer',CLABS,80,0,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Condition number estimate',
     +     ' of the selected cluster of eigenvalues = ', 1.0D0/S
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Condition number estimate of the spec',
     +     'ified invariant subspace    = ', 1.0D0/SEP
      END IF
      STOP
*
99999 FORMAT (1X,A,A,D10.2)
      END
