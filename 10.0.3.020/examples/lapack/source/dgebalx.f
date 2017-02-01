*     DGEBAL (F08NHF) Example Program Text
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
      DOUBLE PRECISION A(LDA,NMAX), H(LDH,NMAX), SCALE(NMAX), TAU(NMAX),
     +                 VL(LDVL,1), VR(LDVR,NMAX), WI(NMAX), WORK(LWORK),
     +                 WR(NMAX)
      LOGICAL          SELECT(1)
*     .. External Subroutines ..
      EXTERNAL         DGEBAK, DGEBAL, DGEHRD, DHSEQR, DORGHR, DTREVC,
     +                 F06QFF, X04CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'DGEBAL Example Program Results'
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
         CALL DGEBAL('Both',N,A,LDA,ILO,IHI,SCALE,INFO)
*
*        Reduce A to upper Hessenberg form H = (Q**T)*A*Q
*
         CALL DGEHRD(N,ILO,IHI,A,LDA,TAU,WORK,LWORK,INFO)
*
*        Copy A to H
*
         CALL F06QFF('General',N,N,A,LDA,H,LDH)
*
*        Copy A into VR
*
         CALL F06QFF('General',N,N,A,LDA,VR,LDVR)
*
*        Form Q explicitly, storing the result in VR
*
         CALL DORGHR(N,1,N,VR,LDVR,TAU,WORK,LWORK,INFO)
*
*        Calculate the eigenvalues and Schur factorization of A
*
         CALL DHSEQR('Schur form','Vectors',N,ILO,IHI,H,LDH,WR,WI,VR,
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
            CALL DTREVC('Right','Back',SELECT,N,H,LDH,VL,LDVL,VR,
     +                  LDVR,N,M,WORK,INFO)
*
*           Backtransform eigenvectors
*
            CALL DGEBAK('Both','Right',N,ILO,IHI,SCALE,M,VR,LDVR,INFO)
*
*           Print eigenvectors
*
            WRITE (NOUT,*)
            IFAIL = 0
*
            CALL X04CAF('General',' ',N,M,VR,LDVR,
     +                  'Contents of array VR',IFAIL)
*
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,F8.4,A,F8.4,A)
      END
