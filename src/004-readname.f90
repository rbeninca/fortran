program leianome

character(len=50)::nome
PRINT *, "qual é seu nome ?"
READ (*,"(A)") nome  ! na formação A que neste caso é nome completo, caso  usase A* seria para uma string ou * separaria até o espaço 
PRINT *, "Olá ", trim(nome), "!"  ! trim serve para retirar os espaços em branco do final da string, caso existam

end program leianome
