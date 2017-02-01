*     SGEBAL (F08NHE) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LDH, LWORK, LDVL, LDVR
      PARAMETER        (NMAX=8,LDA=NMAX,LDH=NMAX,LWORK=64*NMAX,LDVL=1,
     +                 LDVR=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, IHI, ILO, INFO, J, M, N
*     .. Local Arrays ..
      REAL             A(LDA,NMAX), H(LDH,NMAX), SCALE(NMAX), TAU(NMAX),
     +                 VL(LDVL,1), VR(LDVR,NMAX), WI(NMAX), WORK(LWORK),
     +                 WR(NMAX)
      LOGICAL          SELECT(1)
*     .. External Subroutines ..
      EXTERNAL         F06QFE, SGEBAK, SGEBAL, SGEHRD, SHSEQR, SORGHR,
     +                 STREVC, X04CAE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'SGEBAL Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
*
*        Balance A
*
         CALL SGEBAL('Both',N,A,LDA,ILO,IHI,SCALE,INFO)
*
*        Reduce A to upper Hessenberg form H = (Q**T)*A*Q
*
         CALL SGEHRD(N,ILO,IHI,A,LDA,TAU,WORK,LWORK,INFO)
*
*        Copy A to H
*
         CALL F06QFE('General',N,N,A,LDA,H,LDH)
*
*        Copy A into VR
*
         CALL F06QFE('General',N,N,A,LDA,VR,LDVR)
*
*        Form Q explicitly, storing the result in VR
*
         CALL SORGHR(N,1,N,VR,LDVR,TAU,WORK,LWORK,INFO)
*
*        Calculate the eigenvalues and Schur factorization of A
*
         CALL SHSEQR('Schur form','Vectors',N,ILO,IHI,H,LDH,WR,WI,VR,
     +               LDVR,WORK,LWORK,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.GT.0) THEN
            WRITE (NOUT,*) 'Failure to converge.'
         ELSE
            WRITE (NOUT,*) 'Eigenvalues'
            WRITE (NOUT,99999) (' (',WR(I),',',WI(I),')',I=1,N)
*
*           Calculate the eigenvectors of A, storing the result in VR
*
            CALL STREVC('Right','Back',SELECT,N,H,LDH,VL,LDVL,VR,
     +                  LDVR,N,M,WORK,INFO)
*
*           Backtransform eigenvectors
*
            CALL SGEBAK('Both','Right',N,ILO,IHI,SCALE,M,VR,LDVR,INFO)
*
*           Print eigenvectors
*
            WRITE (NOUT,*)
            IFAIL = 0
*
            CALL X04CAE('General',' ',N,M,VR,LDVR,
     +                  'Contents of array VR',IFAIL)
*
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,F8.4,A,F8.4,A)
      END
