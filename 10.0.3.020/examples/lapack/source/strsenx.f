*     STRSEN (F08QGE) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDT, LDQ, LWORK, LIWORK
      PARAMETER        (NMAX=8,LDT=NMAX,LDQ=NMAX,LWORK=NMAX*NMAX/2,
     +                 LIWORK=NMAX*NMAX/2)
*     .. Local Scalars ..
      REAL             S, SEP
      INTEGER          I, IFAIL, INFO, J, M, N
*     .. Local Arrays ..
      REAL             Q(LDQ,NMAX), T(LDT,NMAX), WI(NMAX), WORK(LWORK),
     +                 WR(NMAX)
      INTEGER          IWORK(LIWORK)
      LOGICAL          SELECT(NMAX)
*     .. External Subroutines ..
      EXTERNAL         STRSEN, X04CAE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'STRSEN Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read T and Q from data file
*
         READ (NIN,*) ((T(I,J),J=1,N),I=1,N)
         READ (NIN,*) ((Q(I,J),J=1,N),I=1,N)
*
         READ (NIN,*) (SELECT(I),I=1,N)
*
*        Reorder the Schur factorization
*
         CALL STRSEN('Both','Vectors',SELECT,N,T,LDT,Q,LDQ,WR,WI,M,S,
     +               SEP,WORK,LWORK,IWORK,LIWORK,INFO)
*
         WRITE (NOUT,*)
         IFAIL = 0
*
         CALL X04CAE('General',' ',N,N,T,LDT,'Reordered Schur form',
     +               IFAIL)
*
         WRITE (NOUT,*)
         IFAIL = 0
*
         CALL X04CAE('General',' ',N,M,Q,LDQ,
     +               'Basis of invariant subspace',IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Condition number estimate',
     +     ' of the selected cluster of eigenvalues = ', 1.0/S
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Condition number estimate of the spec',
     +     'ified invariant subspace    = ', 1.0/SEP
      END IF
      STOP
*
99999 FORMAT (1X,A,A,E10.2)
      END
