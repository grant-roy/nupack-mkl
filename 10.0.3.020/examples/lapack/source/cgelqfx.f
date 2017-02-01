*     CGELQF (F08AVE) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, LDA, LDB, NRHMAX, LWORK
      PARAMETER        (MMAX=8,NMAX=8,LDA=MMAX,LDB=NMAX,NRHMAX=NMAX,
     +                 LWORK=64*NMAX)
      COMPLEX          ZERO, ONE
      PARAMETER        (ZERO=(0.0,0.0),ONE=(1.0,0.0))
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, M, N, NRHS
*     .. Local Arrays ..
      COMPLEX          A(LDA,NMAX), B(LDB,NRHMAX), TAU(NMAX),
     +                 WORK(LWORK)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         CGELQF, CTRSM, CUNMLQ, F06THE, X04DBE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'CGELQF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N, NRHS
      IF (M.LE.MMAX .AND. N.LE.NMAX .AND. M.LE.N .AND. NRHS.LE.NRHMAX)
     +    THEN
*
*        Read A and B from data file
*
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
         READ (NIN,*) ((B(I,J),J=1,NRHS),I=1,M)
*
*        Compute the LQ factorization of A
*
         CALL CGELQF(M,N,A,LDA,TAU,WORK,LWORK,INFO)
*
*        Solve L*Y = B, storing the result in B
*
         CALL CTRSM('Left','Lower','No transpose','Non-Unit',M,NRHS,ONE,
     +              A,LDA,B,LDB)
*
*        Set rows (M+1) to N of B to zero
*
         IF (M.LT.N) CALL F06THE('General',N-M,NRHS,ZERO,ZERO,B(M+1,1),
     +                           LDB)
*
*        Compute minimum-norm solution X = (Q**H)*B in B
*
         CALL CUNMLQ('Left','Conjugate transpose',N,NRHS,M,A,LDA,TAU,B,
     +               LDB,WORK,LWORK,INFO)
*
*        Print minimum-norm solution(s)
*
         WRITE (NOUT,*)
         IFAIL = 0
*
         CALL X04DBE('General',' ',N,NRHS,B,LDB,'Bracketed','F7.4',
     +               'Minimum-norm solution(s)','Integer',RLABS,
     +               'Integer',CLABS,80,0,IFAIL)
*
      END IF
      STOP
      END
