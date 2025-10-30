# Configurar Ethernet como DHCP Cliente

## Problema Identificado

O TV box está com IP fixo `10.1.1.1` na interface ethernet, mas você deseja usar DHCP dinâmico.

## Solução

Criei um script `configure_ethernet_dhcp.sh` que configura a ethernet para obter IP automaticamente via DHCP.

## Como Usar

### No TV Box (via SSH):

```bash
# 1. Clonar/atualizar repositório
cd /home/rbeninca/balancaGFIG
git pull origin main

# 2. Executar o script
sudo bash configure_ethernet_dhcp.sh
```

### O que o script faz:

1. **Detecta a interface ethernet** automaticamente
2. **Remove configurações de IP fixo** anteriores
3. **Configura DHCP** usando um dos métodos abaixo (em ordem de preferência):
   - NetworkManager (recomendado)
   - systemd-networkd (fallback)
   - dhclient (fallback final)
4. **Verifica a configuração** e testa conectividade
5. **Mostra o IP obtido** via DHCP

## Resultado Esperado

```
[SUCCESS] Interface ethernet detectada: eth0
[STEP] Removendo configurações de IP fixo...
[SUCCESS] Ethernet configurada como DHCP cliente via NetworkManager!

Estado da interface ethernet:
2: eth0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500
    inet 192.168.x.x/24 brd 192.168.x.255 scope global dynamic eth0
    ...
    
✓ Internet disponível!
```

## Se Houver Problemas

### Ver status da conexão:
```bash
nmcli connection show
nmcli device status
```

### Ver logs do NetworkManager:
```bash
journalctl -u NetworkManager -f
```

### Reativar manualmente:
```bash
sudo nmcli connection down "Ethernet-DHCP"
sudo nmcli connection up "Ethernet-DHCP"
```

### Ver IP obtido:
```bash
ip addr show eth0
```

## Hotspot WiFi

Se o hotspot WiFi estiver ainda ativo (com IP `10.1.1.1`), ele funcionará como **segundo acesso** via WiFi. Isso é útil para ter:

- **Ethernet**: DHCP dinâmico (para conectar à rede)
- **WiFi hotspot**: `10.1.1.1` (para acesso local sem fio)

Se quiser remover o hotspot:
```bash
sudo nmcli connection delete "rbeninca-Hotspot"
# ou
sudo nmcli connection delete "BenincaGaspar"
```

## Próximas Etapas

1. Execute o script no TV box
2. Verifique se obteve um IP via DHCP
3. Teste conectividade com `ping 8.8.8.8`
4. O Docker continuará funcionando normalmente

---

**Arquivo do script**: `configure_ethernet_dhcp.sh`  
**Branch**: `main`  
**Status**: ✅ Pronto para usar
