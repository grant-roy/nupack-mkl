*     ZGEBAL (F08NVF) Example Program Text
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
      COMPLEX*16       A(LDA,NMAX), H(LDH,NMAX), TAU(NMAX), VL(LDVL,1),
     +                 VR(LDVR,NMAX), W(NMAX), WORK(LWORK)
      DOUBLE PRECISION RWORK(NMAX), SCALE(NMAX)
      LOGICAL          SELECT(1)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         F06TFF, X04DBF, ZGEBAK, ZGEBAL, ZGEHRD, ZHSEQR,
     +                 ZTREVC, ZUNGHR
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE, DIMAG
*     .. Executable Statements ..
      WRITE (NOUT,*) 'ZGEBAL Example Program Results'
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
         CALL ZGEBAL('Both',N,A,LDA,ILO,IHI,SCALE,INFO)
*
*        Reduce A to upper Hessenberg form H = (Q**H)*A*Q
*
         CALL ZGEHRD(N,ILO,IHI,A,LDA,TAU,WORK,LWORK,INFO)
*
*        Copy A to H
*
         CALL F06TFF('General',N,N,A,LDA,H,LDH)
*
*        Copy A into VR
*
         CALL F06TFF('General',N,N,A,LDA,VR,LDVR)
*
*        Form Q explicitly, storing the result in VR
*
         CALL ZUNGHR(N,1,N,VR,LDVR,TAU,WORK,LWORK,INFO)
*
*        Calculate the eigenvalues and Schur factorization of A
*
         CALL ZHSEQR('Schur form','Vectors',N,ILO,IHI,H,LDH,W,VR,LDVR,
     +               WORK,LWORK,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.GT.0) THEN
            WRITE (NOUT,*) 'Failure to converge.'
         ELSE
            WRITE (NOUT,*) 'Eigenvalues'
*            WRITE (NOUT,99999) (' (',DBLE(W(I)),',',DIMAG(W(I)),')',
*     +        I=1,N)
            WRITE (NOUT,99999)
     *            (' (',DBLE(W(I)),',',DIMAG(W(I)),')',I=1,N)
*
*           Calculate the eigenvectors of A, storing the result in VR
*
            CALL ZTREVC('Right','Back',SELECT,N,H,LDH,VL,LDVL,VR,
     +                  LDVR,N,M,WORK,RWORK,INFO)
*
*           Backtransform eigenvectors
*
            CALL ZGEBAK('Both','Right',N,ILO,IHI,SCALE,M,VR,LDVR,INFO)
*
*           Print eigenvectors
*
            WRITE (NOUT,*)
            IFAIL = 0
*
            CALL X04DBF('General',' ',N,M,VR,LDVR,'Bracketed','F7.4',
     +                  'Contents of array VR','Integer',RLABS,
     +                  'Integer',CLABS,80,0,IFAIL)
*
         END IF
      END IF
      STOP
*
99999 FORMAT ((3X,4(A,F7.4,A,F7.4,A,:)))
      END
