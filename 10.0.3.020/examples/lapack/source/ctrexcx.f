*     CTREXC (F08QTE) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDT, LDQ
      PARAMETER        (NMAX=8,LDT=NMAX,LDQ=1)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, IFST, ILST, INFO, J, N
*     .. Local Arrays ..
      COMPLEX          Q(LDQ,1), T(LDT,NMAX)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         CTREXC, X04DBE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'CTREXC Example Program Results'
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
         CALL CTREXC('No update',N,T,LDT,Q,LDQ,IFST,ILST,INFO)
*
*        Print reordered Schur form
*
         WRITE (NOUT,*)
         IFAIL = 0
*
         CALL X04DBE('General',' ',N,N,T,LDT,'Bracketed','F7.4',
     +               'Reordered Schur form','Integer',RLABS,'Integer',
     +               CLABS,80,0,IFAIL)
*
      END IF
      STOP
      END
