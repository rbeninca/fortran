PROGRAM Cap8b
    IMPLICIT NONE
    INTEGER :: B, C, D
    REAL :: M
    INTEGER :: i, count
    CHARACTER(len=100) :: nome
    OPEN(UNIT=10, FILE='Cap8b.TXT', STATUS='REPLACE') 
    nome = "Romulo de Aguiar Beninca"      
    B = -13
    C = 43
    D = 5

    M = 0.0 
    count = 0
    
    DO i = B, C, D       
        M = M + i  
        count = count + 1  
        IF (i > 0) THEN
             PRINT *, "Ciclo interrompido em i = ", i
            EXIT  
        END IF
    END DO
    IF (count > 0) THEN
        M = M / REAL(count) 
    ELSE
        M = 0.0  
    END IF    
    WRITE(10,*) "Nome: ", TRIM(nome)
    WRITE(10,*) "Média: ", M
    CLOSE(10)  
    CALL SYSTEM('start notepad Cap8b.TXT') 

END PROGRAM Cap8b
