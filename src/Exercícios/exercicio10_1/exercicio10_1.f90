PROGRAM programa10_1
   IMPLICIT NONE
   INTEGER LINHA, LINHAS, COLUNA, COLUNAS
   INTEGER, ALLOCATABLE, DIMENSION(:,:) :: MATRIZ
   WRITE(*,*) "Entre com o numero de linhas da matriz"
   READ(*,*) LINHAS
   WRITE(*,*) "Entre com o numero de colunas da matriz"
   READ(*,*) COLUNAS
   ALLOCATE ( MATRIZ ( LINHAS, COLUNAS ) )
   DO LINHA = 1, LINHAS
      WRITE(*,*) "Entre com os valores dos elementos da matriz da linha =", LINHA
      DO COLUNA = 1, COLUNAS
         WRITE(*,*) "Entre com o valor do elemento da matriz da coluna =", COLUNA
         READ(*,*) MATRIZ(LINHA,COLUNA)
      END DO
   END DO
   WRITE(*,*) "Escrita da MATRIZ sem formato"
   WRITE(*,*) MATRIZ
   WRITE(*,*) "Escrita da MATRIZ na mesma sequencia dos dados"
   DO LINHA = 1, LINHAS
      DO COLUNA = 1, COLUNAS
         WRITE(*,1) LINHA, COLUNA, MATRIZ(LINHA,COLUNA)
1        FORMAT( 3X, "MATRIZ(", I1, ",", I1, ") = ", I5)
      END DO
   END DO
END PROGRAM programa10_1
