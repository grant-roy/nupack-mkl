*     DTREXC (F08QFF) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDT, LDQ
      PARAMETER        (NMAX=8,LDT=NMAX,LDQ=1)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, IFST, ILST, INFO, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION Q(LDQ,1), T(LDT,NMAX), WORK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         DTREXC, X04CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'DTREXC Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read T from data file
*
         READ (NIN,*) ((T(I,J),J=1,N),I=1,N)
*
         READ (NIN,*) IFST, ILST
*
*        Reorder the Schur factorization T
*
         CALL DTREXC('No update',N,T,LDT,Q,LDQ,IFST,ILST,WORK,INFO)
*
*        Print reordered Schur form
*
         WRITE (NOUT,*)
         IFAIL = 0
*
         CALL X04CAF('General',' ',N,N,T,LDT,'Reordered Schur form',
     +               IFAIL)
*
      END IF
      STOP
      END
