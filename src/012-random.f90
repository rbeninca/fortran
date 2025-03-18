!Programa gera um número aleatório ao ser chamado no terminal
PROGRAM RANDOM
IMPLICIT NONE

!VARS 
    REAL :: rand
    CALL RANDOM_NUMBER(rand);
    WRITE (*,*) "Número aleatório gerado: ", rand
       
    


END PROGRAM RANDOM