PROGRAM contador_ate
!vars 
    INTEGER :: i=0, n 
    
    WRITE (*,'(A)', ADVANCE="NO") "Informe até que número contar:"
    READ (*,"(I8)")  n

    DO WHILE (i<=n)
        WRITE (*,"(I8.8,1X)", ADVANCE="NO") i
        i=i+1
    END DO 

END PROGRAM contador_ate