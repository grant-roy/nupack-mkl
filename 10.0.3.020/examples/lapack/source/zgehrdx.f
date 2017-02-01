*     ZGEHRD (F08NSF) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LWORK
      PARAMETER        (NMAX=8,LDA=NMAX,LWORK=64*NMAX)
      COMPLEX*16       ZERO
      PARAMETER        (ZERO=(0.0D0,0.0D0))
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, N
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), TAU(NMAX-1), WORK(LWORK)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         X04DBF, ZGEHRD
*     .. Executable Statements ..
      WRITE (NOUT,*) 'ZGEHRD Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
*
*        Reduce A to upper Hessenberg form
*
         CALL ZGEHRD(N,1,N,A,LDA,TAU,WORK,LWORK,INFO)
*
*        Set the elements below the first sub-diagonal to zero
*
         DO 40 I = 1, N - 2
            DO 20 J = I + 2, N
               A(J,I) = ZERO
   20       CONTINUE
   40    CONTINUE
*
*        Print upper Hessenberg form
*
         WRITE (NOUT,*)
         IFAIL = 0
*
         CALL X04DBF('General',' ',N,N,A,LDA,'Bracketed','F7.4',
     +               'Upper Hessenberg form','Integer',RLABS,'Integer',
     +               CLABS,80,0,IFAIL)
*
      END IF
      STOP
      END
