SUBROUTINE CHECA(A,B , C)
 INTEGER A, B , C
 IF  (.NOT. (A<B .AND.  B<C)) THEN 
	PRINT *,"Erro na ordenacao"
 ELSE 
	PRINT *,"Correto"
 END IF 


END SUBROUTINE