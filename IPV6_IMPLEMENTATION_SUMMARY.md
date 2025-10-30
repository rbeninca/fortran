# Implementação de IPv6 e Correção de WebSocket

## Resumo Executivo

Implementação completa de suporte a **IPv6 dual-stack** (IPv4 + IPv6) com **WebSocket estável** para acesso à balança via endereço IPv6 público.

- ✅ HTTP e WebSocket funcionando em dual-stack IPv4/IPv6
- ✅ Acesso público via IPv6: `http://[2804:4950:1:803e:2e1a:e0b1:897:d766]/`
- ✅ WebSocket com keepalive e reconexão automática
- ✅ Logging detalhado para debugging

## Problemas Resolvidos

### 1. Acesso IPv6 Bloqueado
**Problema**: Endereço IPv6 público não era acessível do container Docker
**Solução**: Alterou `docker-compose.yml` para usar `network_mode: host` na service balanca

### 2. Servidor HTTP Bound a IPv4 Apenas
**Problema**: `BIND_HOST = "0.0.0.0"` e `address_family = AF_INET` vinculava apenas IPv4
**Solução**: 
- Alterou `BIND_HOST = "::"`  (IPv6 any address)
- Alterou `address_family = AF_INET6` (suporte dual-stack)
- Configurou `IPV6_V6ONLY = 0` para aceitar IPv4 via IPv6

### 3. WebSocket Desconectando a Cada ~60 Segundos
**Problema**: Conexões WebSocket encerrando regularmente, impossível manter conexão estável
**Solução**:
- Adicionou `ping_interval=30s` (servidor envia ping a cada 30 segundos)
- Adicionou `ping_timeout=10s` (espera 10 segundos por pong)
- Adicionou reconexão automática no cliente JavaScript

### 4. Servidor MySQL Inacessível com `network_mode: host`
**Problema**: Container não conseguia conectar a MySQL com `network_mode: host`
**Solução**: Alterou `MYSQL_HOST` de "db" (nome DNS) para "127.0.0.1" (localhost)

## Mudanças Implementadas

### docker-compose.yml
```yaml
balanca:
  network_mode: host  # Acesso direto ao IPv6 público
  environment:
    MYSQL_HOST: 127.0.0.1  # Localhost em vez de nome DNS
```

### server.py
```python
# Bind dual-stack
BIND_HOST = "::"  # IPv6 any address

# HTTP Server
class DualStackTCPServer(SocketServer.TCPServer):
    address_family = socket.AF_INET6
    def server_bind(self):
        self.socket.setsockopt(socket.IPPROTO_IPV6, socket.IPV6_V6ONLY, 0)
        # Permite IPv4 e IPv6

# WebSocket Server
sock = socket.socket(socket.AF_INET6, socket.SOCK_STREAM)
sock.setsockopt(socket.IPPROTO_IPV6, socket.IPV6_V6ONLY, 0)

async with websockets.serve(
    ws_handler,
    sock=sock,
    ping_interval=30,    # Keepalive: ping a cada 30s
    ping_timeout=10,     # Timeout de pong: 10s
    close_timeout=10,    # Timeout de fechamento: 10s
    max_queue=32         # Buffer de mensagens
):
```

### data/dataWorker.js
```javascript
// Constantes de reconexão
const MAX_RECONNECT_ATTEMPTS = 10;
const RECONNECT_INTERVAL = 3000; // 3 segundos

// Reconexão automática ao fechar conexão
socket.onclose = (event) => {
    if (reconnectAttempts < MAX_RECONNECT_ATTEMPTS) {
        reconnectAttempts++;
        reconnectTimeout = setTimeout(() => {
            connectWebSocket();
        }, RECONNECT_INTERVAL);
    }
};
```

## Testes e Validação

### ✅ Acesso IPv4
```bash
curl http://192.168.1.17/api/time
# {"time": "2025-10-30T13:38:15.835260-03:00"}
```

### ✅ Acesso IPv6
```bash
curl http://[2804:4950:1:803e:2e1a:e0b1:897:d766]/api/time
# {"time": "2025-10-30T13:38:15.847271-03:00"}
```

### ✅ WebSocket Dual-Stack
- Logging mostra conexões de ambos IPv4 (`::ffff:192.168.1.11`) e IPv6 (`2804:4950:1:803e:...`)
- Keepalive funcionando: `ping_interval=30s, ping_timeout=10s`
- Reconexão automática funcionando quando conexão cai

### ✅ Banco de Dados
- Conexão com MySQL estável mesmo com `network_mode: host`

## Commits da Feature Branch

```
946c0ef chore: melhorar mensagens de logging do WebSocket keepalive
6feee95 chore: melhorar logs de configuração do WebSocket
be38910 feat: adicionar reconexão automática ao WebSocket do cliente
310dc6c fix: remover parâmetro ping_interval_secs inválido no WebSocket
03c8b89 feat: adicionar logging mais detalhado para WebSocket keepalive
c41aff8 fix: adicionar ping/pong keepalive no WebSocket para evitar desconexão
7bfecd4 fix: habilitar dual-stack no WebSocket também
87453d4 fix: habilitar IPv6 dual-stack no servidor HTTP
306e73c fix: alterar MYSQL_HOST para localhost para funcionar com network_mode host
baf0812 fix: usar network_mode host para acesso direto ao IPv6 público
```

## Como Usar

### Acesso Local (IPv4)
```
http://192.168.1.17/
```

### Acesso Remoto (IPv6)
```
http://[2804:4950:1:803e:2e1a:e0b1:897:d766]/
```

Substitua o endereço IPv6 pelo seu endereço público IPv6.

## Monitoramento

Verifique saúde do WebSocket nos logs do container:

```bash
docker logs balanca | grep -E "(WebSocket|keepalive|Nova conexão)"
```

Exemplo de saída esperada:
```
[INFO] WebSocket ativo em :::81 (dual-stack IPv4+IPv6)
[INFO]   Keepalive habilitado: ping_interval=30s, ping_timeout=10s, close_timeout=10s, max_queue=32
[INFO] [WS] Nova conexão de ('::ffff:192.168.1.11', 48420, 0, 0). Total de clientes: 1
[INFO] [WS] Nova conexão de ('2804:4950:1:803e:dbf5:294c:580b:1955', 54000, 0, 0). Total de clientes: 2
```

## Próximas Etapas

1. **Merge para main**: Quando testado e validado em produção
2. **Documentação do usuário**: Atualizar README com instruções de acesso IPv6
3. **Monitoramento**: Implementar alertas se WebSocket ficar desconectado
4. **Otimização**: Ajustar `ping_interval` se necessário baseado em observações de latência

## Notas Técnicas

- **Dual-stack socket**: Uso de `AF_INET6` com `IPV6_V6ONLY=0` permite que um único socket aceite conexões IPv4 e IPv6
- **Keepalive**: WebSocket implementa keepalive através de ping/pong frames - cliente deve responder com pong quando receber ping
- **Reconexão**: Cliente tenta reconectar até 10 vezes com intervalo de 3 segundos entre tentativas
- **network_mode: host**: Garante que container tem acesso direto ao IPv6 público da host, mas necessita comunicação localhost para MySQL

