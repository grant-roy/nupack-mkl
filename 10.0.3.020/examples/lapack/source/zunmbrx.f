*     ZUNMBR (F08KUF) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, LDA, LDPH, LDU, LWORK
      PARAMETER        (MMAX=8,NMAX=8,LDA=MMAX,LDPH=NMAX,LDU=MMAX,
     +                 LWORK=64*(MMAX+NMAX))
      COMPLEX*16       ZERO
      PARAMETER        (ZERO=(0.0D0,0.0D0))
*     .. Local Scalars ..
      INTEGER          I, IC, IFAIL, INFO, J, M, N
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), PH(LDPH,NMAX), TAU(NMAX),
     +                 TAUP(NMAX), TAUQ(NMAX), U(LDU,NMAX), WORK(LWORK)
      DOUBLE PRECISION D(NMAX), E(NMAX-1)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         F06TFF, F06THF, X04DBF, ZGEBRD, ZGELQF, ZGEQRF,
     +                 ZUNGLQ, ZUNGQR, ZUNMBR
*     .. Executable Statements ..
      WRITE (NOUT,*) 'ZUNMBR Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      DO 20 IC = 1, 2
         READ (NIN,*) M, N
         IF (M.LE.MMAX .AND. N.LE.NMAX) THEN
*
*           Read A from data file
*
            READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
*
            IF (M.GE.N) THEN
*
*              Compute the QR factorization of A
*
               CALL ZGEQRF(M,N,A,LDA,TAU,WORK,LWORK,INFO)
*
*              Copy A to U
*
               CALL F06TFF('Lower',M,N,A,LDA,U,LDU)
*
*              Form Q explicitly, storing the result in U
*
               CALL ZUNGQR(M,M,N,U,LDU,TAU,WORK,LWORK,INFO)
*
*              Copy R to PH (used as workspace)
*
               CALL F06TFF('Upper',N,N,A,LDA,PH,LDPH)
*
*              Set the strictly lower triangular part of R to zero
*
               CALL F06THF('Lower',N-1,N-1,ZERO,ZERO,PH(2,1),LDPH)
*
*              Bidiagonalize R
*
               CALL ZGEBRD(N,N,PH,LDPH,D,E,TAUQ,TAUP,WORK,LWORK,INFO)
*
*              Update Q, storing the result in U
*
               CALL ZUNMBR('Q','Right','No transpose',M,N,N,PH,LDPH,
     +                     TAUQ,U,LDU,WORK,LWORK,INFO)
*
*              Print bidiagonal form and matrix Q
*
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Example 1: bidiagonal matrix B'
               WRITE (NOUT,*) 'Diagonal'
               WRITE (NOUT,99999) (D(I),I=1,N)
               WRITE (NOUT,*) 'Super-diagonal'
               WRITE (NOUT,99999) (E(I),I=1,N-1)
               WRITE (NOUT,*)
               IFAIL = 0
*
               CALL X04DBF('General',' ',M,N,U,LDU,'Bracketed','F7.4',
     +                     'Example 1: matrix Q','Integer',RLABS,
     +                     'Integer',CLABS,80,0,IFAIL)
*
            ELSE
*
*              Compute the LQ factorization of A
*
               CALL ZGELQF(M,N,A,LDA,TAU,WORK,LWORK,INFO)
*
*              Copy A to PH
*
               CALL F06TFF('Upper',M,N,A,LDA,PH,LDPH)
*
*              Form Q explicitly, storing the result in PH
*
               CALL ZUNGLQ(N,N,M,PH,LDPH,TAU,WORK,LWORK,INFO)
*
*              Copy L to U (used as workspace)
*
               CALL F06TFF('Lower',M,M,A,LDA,U,LDU)
*
*              Set the strictly upper triangular part of L to zero
*
               CALL F06THF('Upper',M-1,M-1,ZERO,ZERO,U(1,2),LDU)
*
*              Bidiagonalize L
*
               CALL ZGEBRD(M,M,U,LDU,D,E,TAUQ,TAUP,WORK,LWORK,INFO)
*
*              Update P**H, storing the result in PH
*
               CALL ZUNMBR('P','Left','Conjugate transpose',M,N,M,U,LDU,
     +                     TAUP,PH,LDPH,WORK,LWORK,INFO)
*
*              Print bidiagonal form and matrix P**H
*
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Example 2: bidiagonal matrix B'
               WRITE (NOUT,*) 'Diagonal'
               WRITE (NOUT,99999) (D(I),I=1,M)
               WRITE (NOUT,*) 'Super-diagonal'
               WRITE (NOUT,99999) (E(I),I=1,M-1)
               WRITE (NOUT,*)
               IFAIL = 0
*
               CALL X04DBF('General',' ',M,N,PH,LDPH,'Bracketed','F7.4',
     +                     'Example 2: matrix P**H','Integer',RLABS,
     +                     'Integer',CLABS,80,0,IFAIL)
*
            END IF
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (3X,(8F8.4))
      END
