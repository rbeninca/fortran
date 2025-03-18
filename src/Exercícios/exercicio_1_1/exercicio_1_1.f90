PROGRAM exercicio_1_1
!PROGRAMA IMPRIMR A DATA ATUAL 
!VARS
    INTEGER :: valores_datahora(8)
!BEGIN PROGRAM 
    CALL DATE_AND_TIME(VALUES=valores_datahora)
    WRITE (*,*)  "Data Atual:",valores_datahora(3),"/",valores_datahora(2),"/",valores_datahora(1)
    WRITE (*,'(A,I2.2,A,I2.2,A,I4.4)') "Data Atual:",valores_datahora(3),":",valores_datahora(2),":",valores_datahora(1)
END PROGRAM 