PROGRAM helloprints
    WRITE (*,'(A5)') 'Hello, World!'     ! (A5) é um formato de 5 caracteres de texto (string)
    WRITE (*,'(I2)')  10                   ! (I2) é um formato de 2 caracteres de inteiro
    WRITE (*,'(I2)')  111                  ! IMPRIME ASTERISCOS NO LUGAR DE 111 (porque o formato é de 2 caracteres)
    WRITE (*,'(F5.2)')  10.0               ! (F5.2) é um formato de 5 caracteres de float com 2 casas decimais
    WRITE (*,'(F5.2)')  10.123456789       ! IMPRIME ASTERISCOS NO LUGAR DE 10.123456789 (porque o formato é de 5 caracteres)
    WRITE (*,'(A5,I2)') 'Hello, World!' ,2
    WRITE (*,'(A5,I2,A3)') 'Hello, World!' ,2,"Olá mundão perdido!!"
    WRITE (*,'(A5,I2,A3)') 'Hello, World!' ,2,"Olá mundão perdido!!"
    PRINT *, 'Hello, World!'     ! pode ser usado para imprimir na tela de forma simples sem formatação
END PROGRAM  helloprints