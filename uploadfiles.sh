#!/bin/bash
#
# Script para gerenciar o upload de firmware e filesystem em projetos PlatformIO para ESP8266/ESP32.
# Este script utiliza os comandos nativos do PlatformIO, garantindo que as configurações
# do seu projeto (platformio.ini) sejam respeitadas.
#

echo "----------------------------------------------------"
echo "  Gerenciador de Upload para PlatformIO"
echo "----------------------------------------------------"
echo "Escolha uma opção:"
echo "1) Enviar apenas o sistema de arquivos (pasta 'data')"
echo "2) Apagar TODA a memória flash e depois enviar o sistema de arquivos"
echo "3) Apagar TODA a memória flash e depois enviar o FIRMWARE"
echo "4) Sair"
echo "----------------------------------------------------"
read -p "Opção: " option

case $option in
    1)
        echo ">>> Enviando o sistema de arquivos (uploadfs)..."
        pio run --target uploadfs
        ;;
    2)
        echo ">>> Apagando TODA a memória flash (erase)..."
        # O '&&' garante que o upload só acontecerá se o 'erase' for bem-sucedido.
        pio run --target erase && pio run --target uploadfs
        ;;
    3)
        echo ">>> Apagando TODA a memória flash (erase)..."
        # Após apagar, o comando 'upload' compila e envia o firmware principal.
        pio run --target erase && pio run --target upload
        ;;
    4)
        echo "Saindo..."
        exit 0
        ;;
    *)
        echo "Opção inválida!"
        exit 1
        ;;
esac

# A mensagem de sucesso ou erro será exibida pelo próprio PlatformIO.
echo "----------------------------------------------------"
echo "Operação concluída."
echo "----------------------------------------------------"