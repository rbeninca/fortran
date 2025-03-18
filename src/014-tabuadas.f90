!Programa calcula tabuadas de 0 a 10
PROGRAM tabuadas


INTEGER :: tabuadaN,num,comprimento_n
REAL  :: aux;
CHARACTER (LEN=100):: fmt 

tabuadaN=0
num=0
DO WHILE (num<=10)
    tabuadaN=0;
    DO WHILE (tabuadaN<=10)
        !calculo comprimento do produto de maior grandeza da tabuada
        IF (tabuadaN .NE. 0) THEN 
            aux=tabuadaN*10
        ELSE 
            aux=1.0*10
        END IF 
        comprimento_n=INT(LOG10(ABS(aux)))+1
        
        !configurando string de formatação fmt para tabuada
        WRITE (fmt,*) "(A,I",comprimento_n,",A,I",comprimento_n,",A,I",comprimento_n,"A)"
        !calculando a tabuada
        WRITE (*,fmt,ADVANCE="NO") "|",tabuadaN,"*",num,"=",num*tabuadaN ,"|";
        
        IF (tabuadaN==10) THEN!queba linha no após imprimir  as 10 tabuadas
            WRITE (*,fmt) "|",tabuadaN,"*",num,"=",num*tabuadaN ,"|";
        END IF
        tabuadaN=tabuadaN+1   
    END DO
     num=num+1
END DO 
END PROGRAM tabuadas