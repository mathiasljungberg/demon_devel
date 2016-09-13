      SUBROUTINE COMPRESS(IOP,JOP,K,A,M,B,N)
      IMPLICIT NONE
      
      INTEGER IOP,JOP,N,M,I,J,K,I_IND,J_IND
      REAL A(M,M),B(N,N)

      I_IND = 0
      DO I = 1,K
         IF (I.NE.IOP) THEN
            I_IND = I_IND + 1
            J_IND = 0
            DO J = 1,K
               IF (J.NE.JOP) THEN
                  J_IND = J_IND + 1
                  B(I_IND,J_IND) = A(I,J)
               ENDIF
            ENDDO
         ENDIF
      ENDDO
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE COMPRESS ***
C
      END
