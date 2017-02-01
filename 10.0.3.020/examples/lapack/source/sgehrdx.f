*     SGEHRD (F08NEE) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LWORK
      PARAMETER        (NMAX=8,LDA=NMAX,LWORK=64*NMAX)
      REAL             ZERO
      PARAMETER        (ZERO=0.0)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, N
*     .. Local Arrays ..
      REAL             A(LDA,NMAX), TAU(NMAX-1), WORK(LWORK)
*     .. External Subroutines ..
      EXTERNAL         SGEHRD, X04CAE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'SGEHRD Example Program Results'
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
         CALL SGEHRD(N,1,N,A,LDA,TAU,WORK,LWORK,INFO)
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
         CALL X04CAE('General',' ',N,N,A,LDA,'Upper Hessenberg form',
     +               IFAIL)
*
      END IF
      STOP
      END