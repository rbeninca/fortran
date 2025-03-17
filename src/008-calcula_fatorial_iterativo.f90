!Programa calcula fatorial
PROGRAM PROGRAM_FATORIAL
    IMPLICIT NONE;
    WRITE (*,"(I8.8)")  fatorial(5);

    
CONTAINS 
    INTEGER  FUNCTION fatorial (n) 
        !args   
        INTEGER , INTENT(IN) ::n
        !vars 
        INTEGER :: i=1
        !code
        fatorial=1; 
        
        IF (n>0) THEN 
            DO WHILE ( i<=n)
                fatorial=fatorial*i
                i=i+1;
            END DO
        END IF 
    END FUNCTION fatorial



END PROGRAM PROGRAM_FATORIAL