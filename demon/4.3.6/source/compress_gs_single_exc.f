      SUBROUTINE COMPRESS_SINGLE_EXC(IOP,JOP,K,A,M,B,N)
      IMPLICIT NONE
      
      INTEGER IOP,JOP,N,M,I,J,K,I_IND,J_IND
      REAL A(M,M),B(N,N)

c     for I, just take all occupied states
c     for J, swap orbital IOP for JOP 
      DO I = 1,K
         I_IND = I
         DO J = 1,K
            IF (J.EQ.IOP) THEN
               J_IND = JOP
            ELSE
               J_IND = J
            ENDIF
            
            B(I,J) = A(I_IND,J_IND)
         
         ENDDO
      ENDDO
C
C     ------------------------------------------------------------------
C
C     *** End of SUBROUTINE COMPRESS ***
C
      END
