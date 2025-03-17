program dataatual
    integer :: valores_datahora(8)
    integer :: dia, mes, ano, hora, minuto, segundo, ms
    !integer :: call date_and_time(values=(dia, mes, ano,hora,minuto,segundo,ms))
    call date_and_time(values=valores_datahora)
    ano=valores_datahora(1)
    mes=valores_datahora(2)
    dia=valores_datahora(3)
    hora=valores_datahora(5)
    minuto=valores_datahora(6)
    segundo=valores_datahora(7)
    ms=valores_datahora(8)
    write (*,'(A,I2,A,I2.2,A,I4,A,I2,A,I2,A,I2,A,I3.3)') "Times: ",dia, "/", mes , "/" , ano , "--" , hora , ":", minuto, ":", segundo, ":", ms

end program dataatual
