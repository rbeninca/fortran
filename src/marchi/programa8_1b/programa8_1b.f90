PROGRAM Cap8b
    IMPLICIT NONE
    INTEGER :: B, C, D
    REAL :: M
    INTEGER :: i
    CHARACTER(len=100) :: nome
    OPEN(UNIT=10, FILE='Cap8b.TXT', STATUS='REPLACE') 
    nome = "Romulo de Aguiar Beninca" 

    ! Definir os valores de B, C e D
    B = -13
    C = 43
    D = 5

    M = 0.0     
    DO i = B, C, D
	  M = M + I  
	  PRINT *,M
      IF (i > 0) EXIT          
    END DO
    M =M/ ABS( (INT(M / D)))
    WRITE(10,*) "Nome: ", TRIM(nome)
    WRITE(10,*) "Média: ", M
	WRITE(*,*) "Média: ", M


    CLOSE(10)  

    
    CALL SYSTEM('start notepad Cap8b.TXT')  ! Isso abre o arquivo no Notepad

END PROGRAM Cap8b
