      SUBROUTINE GETDET(NOCC,WA,PIV,DET0,INFO)
      IMPLICIT NONE
      INTEGER NOCC,PIV(NOCC),INFO,I
      REAL WA(NOCC,NOCC),DET0

      IF (INFO.NE.0) THEN
         DET0 = 0.D0
      ELSE
         DET0 = 1.D0
         DO I = 1,NOCC
            IF (PIV(I).NE.I) THEN
               DET0 = -DET0*WA(I,I)
            ELSE
               DET0 =  DET0*WA(I,i)
            ENDIF
         ENDDO
      ENDIF
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE GETDET ***
C
      END
