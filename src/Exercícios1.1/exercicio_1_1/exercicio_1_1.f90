PROGRAM exercicio_1_1
!PROGRAMA IMPRIMR A DATA ATUAL 
!VARS
    INTEGER :: date(8)
!BEGIN PROGRAM 

    CALL DATE_AND_TIME(values=date)
    WRITE (*,"(A, I2.2, A, I2.2 , A , I4.4)")  "Data Atual:", date(3), "/", date(2), "/", date(1)
END PROGRAM 