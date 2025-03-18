PROGRAM READREAL
!VARS
REAL :: A, B, C , MEDIA

!a) ler três números reais
PRINT *, "Informe os valores de A  B e C"
WRITE (*, '(A)', ADVANCE="NO") "A:"
READ (*,*)  A
WRITE (*, '(A)', ADVANCE="NO") "B:"
READ (*,*)  B
WRITE (*, '(A)', ADVANCE="NO") "C:"
READ (*,*)  C
!b) calcular a média aritmética deles
MEDIA=(A+B+C)/3
!c) escrever os valores lidos e o valor da média aritmética juntamente com comentários para identificá-los
WRITE (*,'(A,F2.2,/A,F2.2,/A,F2.2, /A,F6.2)' )  "A=",A ,  "B=", B , "C=",C , "MEDIA=((A+B+C)/3)=" , MEDIA

END PROGRAM READREAL