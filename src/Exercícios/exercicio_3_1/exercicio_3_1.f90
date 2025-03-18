!Editar um programa-fonte em FORTRAN para executar o seguinte algoritmo (passos):
!a) ler três números reais
!b) calcular a média aritmética deles
!c) escrever os valores lidos e o valor da média aritmética juntamente com comentários para identificá-los
PROGRAM exercicio_3_1
IMPLICIT NONE
!VARS
REAL :: A, B , C , MEDIA
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
WRITE (*,'(A,F8.2,/A,G0,/A,G0, /A,G0)' )  "A=",A ,  "B=", B , "C=",C , "MEDIA=((A+B+C)/3)=" , MEDIA
END PROGRAM exercicio_3_1


