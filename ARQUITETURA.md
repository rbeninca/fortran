# 📐 Arquitetura do Sistema - Balança Digital GFIG

## 📋 Índice

1. [Visão Geral](#visão-geral)
2. [Arquitetura do Sistema](#arquitetura-do-sistema)
3. [Componentes](#componentes)
4. [Protocolo de Comunicação](#protocolo-de-comunicação)
5. [Endpoints e APIs](#endpoints-e-apis)
6. [Funcionalidades](#funcionalidades)
7. [Configuração e Deploy](#configuração-e-deploy)
8. [Fluxo de Dados](#fluxo-de-dados)

---

## 🎯 Visão Geral

### Propósito
Sistema de balança digital de alta precisão para testes estáticos de motores de mini-foguetes experimentais, com monitoramento em tempo real via interface web.

### Características Principais
- ⚡ Leitura de força em tempo real (até 921600 baud)
- 🌐 Interface web responsiva com gráficos interativos
- 📊 Gravação de sessões de testes no banco de dados
- 🔧 Configuração persistente (ESP32 EEPROM + LocalStorage)
- 🌍 Suporte IPv4 e IPv6 dual-stack
- 🐳 Deploy via Docker Compose
- 📡 Comunicação binária otimizada (16 bytes/leitura)

---

## 🏗️ Arquitetura do Sistema

```
┌─────────────────────────────────────────────────────────────────────┐
│                         CAMADA WEB                                  │
│  ┌───────────────────────────────────────────────────────────┐     │
│  │  Navegador Web (Cliente)                                  │     │
│  │  - HTML5 + CSS3 + JavaScript ES6                          │     │
│  │  - ApexCharts (gráficos)                                  │     │
│  │  - Web Workers (processamento assíncrono)                 │     │
│  │  - LocalStorage (configurações locais)                    │     │
│  └─────────────────┬─────────────────────────────────────────┘     │
│                    │                                                │
│                    │ HTTP/HTTPS (porta 80)                          │
│                    │ WebSocket ws:// ou wss:// (porta 81)           │
│                    ▼                                                │
└────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│                    CAMADA DE SERVIDOR                               │
│  ┌───────────────────────────────────────────────────────────┐     │
│  │  Docker Container: balanca                                │     │
│  │  ┌─────────────────────────────────────────────────┐      │     │
│  │  │  server.py (Python 3.11)                        │      │     │
│  │  │  - HTTP Server (arquivos estáticos)             │      │     │
│  │  │  - WebSocket Server (asyncio + websockets)      │      │     │
│  │  │  - Serial Handler (pyserial)                    │      │     │
│  │  │  - Binary Protocol Parser                       │      │     │
│  │  │  - MySQL Client (pymysql)                       │      │     │
│  │  └─────────────────┬───────────────────────────────┘      │     │
│  │                    │                                       │     │
│  │                    │ USB Serial (921600 baud)              │     │
│  │                    │ Protocolo Binário                     │     │
│  └────────────────────┼───────────────────────────────────────┘     │
│                       │                                             │
│  ┌────────────────────┼───────────────────────────────────────┐     │
│  │  Docker Container: │balanca_mysql                          │     │
│  │  ┌─────────────────▼─────────────────────────────────┐     │     │
│  │  │  MariaDB 11                                       │     │     │
│  │  │  - Database: balanca_gfig                         │     │     │
│  │  │  - Tables: sessoes, leituras                      │     │     │
│  │  │  - Porta: 3306 (IPv4 + IPv6)                      │     │     │
│  │  └───────────────────────────────────────────────────┘     │     │
│  └─────────────────────────────────────────────────────────────┘     │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│                    CAMADA DE HARDWARE                               │
│  ┌───────────────────────────────────────────────────────────┐     │
│  │  ESP32/ESP8266 (Microcontrolador)                         │     │
│  │  ┌─────────────────────────────────────────────────┐      │     │
│  │  │  main.cpp (C++/Arduino)                         │      │     │
│  │  │  - Binary Protocol Implementation               │      │     │
│  │  │  - HX711 Driver (célula de carga)               │      │     │
│  │  │  - EEPROM Storage (configurações)               │      │     │
│  │  │  - Serial Communication (921600 baud)           │      │     │
│  │  │  - Display SSD1306 (OLED)                       │      │     │
│  │  └─────────────────┬───────────────────────────────┘      │     │
│  │                    │                                       │     │
│  │                    │ I²C                                   │     │
│  │                    ▼                                       │     │
│  │  ┌─────────────────────────────────────────────────┐      │     │
│  │  │  HX711 (ADC 24-bit)                             │      │     │
│  │  │  - Amplificador para célula de carga            │      │     │
│  │  │  - Ganho: 128x                                  │      │     │
│  │  └─────────────────┬───────────────────────────────┘      │     │
│  │                    │                                       │     │
│  │                    ▼                                       │     │
│  │  ┌─────────────────────────────────────────────────┐      │     │
│  │  │  Célula de Carga (Load Cell)                    │      │     │
│  │  │  - Capacidade configurável (ex: 5kg, 40kg)      │      │     │
│  │  │  - Saída: mV/V proporcional à força             │      │     │
│  │  └─────────────────────────────────────────────────┘      │     │
│  └───────────────────────────────────────────────────────────┘     │
└─────────────────────────────────────────────────────────────────────┘
```

---

## 🧩 Componentes

### 1. **ESP32/ESP8266 (Firmware)**
**Arquivo:** `src/main.cpp`

**Responsabilidades:**
- Leitura da célula de carga via HX711
- Processamento de sinais (EMA, filtros, zona morta)
- Gerenciamento de calibração e tara
- Armazenamento de configurações na EEPROM
- Comunicação serial binária com o servidor
- Display de status local (OLED)

**Tecnologias:**
- C++/Arduino Framework
- PlatformIO (build system)
- Bibliotecas: HX711, Adafruit_SSD1306, EEPROM, ArduinoJson

**Configurações Armazenadas:**
```cpp
- Fator de conversão (calibração)
- Gravidade local (m/s²)
- Offset de tara
- Parâmetros de estabilização
- Capacidade máxima da célula
- Acurácia/precisão
```

---

### 2. **Servidor Python (Gateway)**
**Arquivo:** `server.py`

**Responsabilidades:**
- **HTTP Server:** Servir arquivos estáticos (HTML, CSS, JS)
- **WebSocket Server:** Comunicação bidirecional em tempo real
- **Serial Handler:** Comunicação USB com ESP32
- **Protocol Parser:** Conversão binário ↔ JSON
- **MySQL Client:** Persistência de sessões de teste
- **Dual-Stack IPv4/IPv6:** Suporte a ambos os protocolos

**Tecnologias:**
- Python 3.11
- asyncio + websockets
- pyserial
- pymysql
- http.server (stdlib)

**Portas:**
```
HTTP:      80  (IPv4 + IPv6)
WebSocket: 81  (IPv4 + IPv6)
Serial:    /dev/ttyUSB0 @ 921600 baud
```

---

### 3. **Interface Web (Cliente)**
**Arquivos:** `data/` e `docs/`

**Estrutura:**
```
data/
├── index.html           # Página principal
├── rede.html            # Configuração de rede
├── script.js            # Lógica principal da UI
├── dataWorker.js        # Web Worker (WebSocket)
├── script_grafico_sessao.js  # Gráficos de sessão
├── funcoespdf.js        # Exportação PDF
├── estilo.css           # Estilos
├── apexcharts           # Biblioteca de gráficos
├── chartist.min.js      # Biblioteca alternativa
└── chartist.min.css
```

**Responsabilidades:**
- Visualização de dados em tempo real
- Gráficos interativos (ApexCharts)
- Controle de sessões de teste
- Configuração de parâmetros
- Exportação de dados (PDF, JSON)
- Gerenciamento de conexão WebSocket

**Tecnologias:**
- HTML5, CSS3, JavaScript ES6+
- Web Workers (processamento assíncrono)
- ApexCharts (gráficos)
- LocalStorage (persistência local)

---

### 4. **Banco de Dados**
**Container:** `balanca_mysql` (MariaDB 11)

**Schema:**

```sql
-- Tabela de Sessões
CREATE TABLE sessoes (
    id INT AUTO_INCREMENT PRIMARY KEY,
    nome VARCHAR(255) NOT NULL,
    data_inicio DATETIME NOT NULL,
    data_fim DATETIME,
    duracao_segundos FLOAT,
    forca_maxima_n FLOAT,
    forca_media_n FLOAT,
    num_leituras INT,
    metadata JSON,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_data_inicio (data_inicio),
    INDEX idx_nome (nome)
);

-- Tabela de Leituras
CREATE TABLE leituras (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    sessao_id INT NOT NULL,
    tempo_mcu_s FLOAT NOT NULL,
    forca_n FLOAT NOT NULL,
    timestamp DATETIME(3) NOT NULL,
    FOREIGN KEY (sessao_id) REFERENCES sessoes(id) ON DELETE CASCADE,
    INDEX idx_sessao (sessao_id),
    INDEX idx_tempo (tempo_mcu_s)
);
```

**Características:**
- Armazenamento de sessões de testes
- Leituras com precisão de milissegundos
- Metadados em JSON (extensível)
- Índices para consultas rápidas
- Healthcheck automático

---

## 📡 Protocolo de Comunicação

### Protocolo Binário (ESP32 ↔ Servidor)

**Características:**
- Magic Number: `0xA1B2` (identificação de pacote)
- Versão: `0x01`
- CRC16-CCITT (validação de integridade)
- Little-endian
- Pacotes de tamanho fixo

#### **1. Pacote DATA (ESP → Servidor)**
**Tamanho:** 16 bytes  
**Tipo:** `0x01`

```cpp
struct PacketData {
    uint16_t magic;      // 0xA1B2 (2 bytes)
    uint8_t  version;    // 0x01 (1 byte)
    uint8_t  type;       // 0x01 (1 byte)
    uint32_t t_ms;       // Timestamp millis() (4 bytes)
    float    forca_N;    // Força em Newtons (4 bytes)
    uint8_t  status;     // 0=Pesando, 1=Tarar, 2=Calibrar, 3=Pronta (1 byte)
    uint8_t  reserved;   // Padding (1 byte)
    uint16_t crc;        // CRC16 (2 bytes)
};
```

**Conversão para JSON:**
```json
{
    "type": "data",
    "tempo": 123.456,
    "forca": 98.765,
    "mysql_connected": true
}
```

#### **2. Pacote CONFIG (ESP → Servidor)**
**Tamanho:** 64 bytes  
**Tipo:** `0x02`

```cpp
struct PacketConfig {
    uint16_t magic;                    // 0xA1B2
    uint8_t  version;                  // 0x01
    uint8_t  type;                     // 0x02
    float    conversionFactor;         // Fator de calibração
    float    gravity;                  // Gravidade (m/s²)
    uint16_t leiturasEstaveis;         // Leituras necessárias para estabilizar
    float    toleranciaEstabilidade;   // Tolerância (N)
    uint16_t numAmostrasMedia;         // Amostras para média móvel
    uint16_t numAmostrasCalibracao;    // Amostras para calibração
    uint8_t  usarMediaMovel;           // Flag booleana
    uint8_t  usarEMA;                  // Flag EMA
    uint16_t timeoutCalibracao;        // Timeout em segundos
    int32_t  tareOffset;               // Offset de tara
    float    capacidadeMaximaGramas;   // Capacidade da célula (g)
    float    percentualAcuracia;       // Acurácia (%)
    uint8_t  mode;                     // Modo de operação
    uint8_t  reserved[23];             // Padding
    uint16_t crc;                      // CRC16
};
```

**Conversão para JSON:**
```json
{
    "type": "config",
    "conversionFactor": 215.5,
    "gravity": 9.80665,
    "leiturasEstaveis": 5,
    "toleranciaEstabilidade": 0.5,
    "numAmostrasMedia": 10,
    "tareOffset": 12345,
    "capacidadeMaximaGramas": 5000.0,
    "percentualAcuracia": 0.05,
    ...
}
```

#### **3. Pacote STATUS (ESP → Servidor)**
**Tamanho:** 14 bytes  
**Tipo:** `0x03`

```cpp
struct PacketStatus {
    uint16_t magic;         // 0xA1B2
    uint8_t  version;       // 0x01
    uint8_t  type;          // 0x03
    uint8_t  statusCode;    // 0=INFO, 1=SUCCESS, 2=WARNING, 3=ERROR
    uint8_t  messageCode;   // Código da mensagem
    uint32_t timestamp;     // millis()
    uint8_t  reserved[2];   // Padding
    uint16_t crc;           // CRC16
};
```

**Códigos de Status:**
- `0x00` - INFO
- `0x01` - SUCCESS
- `0x02` - WARNING
- `0x03` - ERROR

**Códigos de Mensagem:**
- `0x10` - Tara concluída
- `0x11` - Calibração concluída
- `0x12` - Calibração falhou
- `0x20` - Configuração atualizada
- `0xF0` - Erro genérico

---

### Comandos (Servidor → ESP32)

#### **1. CMD_TARA (Comando de Tara)**
**Tamanho:** 8 bytes  
**Tipo:** `0x10`

```cpp
struct CmdTara {
    uint16_t magic;      // 0xA1B2
    uint8_t  version;    // 0x01
    uint8_t  type;       // 0x10
    uint8_t  reserved[2];
    uint16_t crc;
};
```

**JSON do cliente:**
```json
{
    "cmd": "t"
}
```

#### **2. CMD_CALIBRATE (Comando de Calibração)**
**Tamanho:** 10 bytes  
**Tipo:** `0x11`

```cpp
struct CmdCalibrate {
    uint16_t magic;      // 0xA1B2
    uint8_t  version;    // 0x01
    uint8_t  type;       // 0x11
    float    massa_g;    // Massa de calibração em gramas
    uint16_t crc;
};
```

**JSON do cliente:**
```json
{
    "cmd": "c",
    "massa_g": 1000.0
}
```

#### **3. CMD_GET_CONFIG (Solicitar Configurações)**
**Tamanho:** 8 bytes  
**Tipo:** `0x12`

```cpp
struct CmdGetConfig {
    uint16_t magic;      // 0xA1B2
    uint8_t  version;    // 0x01
    uint8_t  type;       // 0x12
    uint8_t  reserved[2];
    uint16_t crc;
};
```

**JSON do cliente:**
```json
{
    "cmd": "get_config"
}
```

#### **4. CMD_SET_PARAM (Definir Parâmetro)**
**Tamanho:** 18 bytes  
**Tipo:** `0x13`

```cpp
struct CmdSetParam {
    uint16_t magic;      // 0xA1B2
    uint8_t  version;    // 0x01
    uint8_t  type;       // 0x13
    uint8_t  paramId;    // ID do parâmetro
    uint8_t  reserved;
    float    value;      // Valor float (8 bytes de payload)
    uint8_t  padding[4];
    uint16_t crc;
};
```

**IDs de Parâmetros:**
```cpp
0x01 - PARAM_GRAVITY      (gravidade)
0x02 - PARAM_CONV_FACTOR  (fator de conversão)
0x03 - PARAM_LEIT_ESTAV   (leituras estáveis)
0x04 - PARAM_TOLERANCIA   (tolerância)
0x05 - PARAM_MODE         (modo)
0x06 - PARAM_USE_EMA      (usar EMA)
0x07 - PARAM_NUM_AMOSTRAS (número de amostras)
0x08 - PARAM_TARE_OFFSET  (offset de tara)
0x09 - PARAM_TIMEOUT_CAL  (timeout calibração)
0x0A - PARAM_CAPACIDADE   (capacidade máxima)
0x0B - PARAM_ACURACIA     (acurácia percentual)
```

**JSON do cliente:**
```json
{
    "cmd": "set",
    "param": "gravity",
    "value": 9.807
}
```

---

### Protocolo WebSocket (Servidor ↔ Cliente Web)

**URL:** `ws://[host]:81` ou `wss://[host]:81`

**Formato:** JSON

#### **Mensagens do Servidor para Cliente:**

**1. Dados de Leitura:**
```json
{
    "type": "data",
    "tempo": 123.456,
    "forca": 98.765,
    "mysql_connected": true
}
```

**2. Configurações:**
```json
{
    "type": "config",
    "conversionFactor": 215.5,
    "gravity": 9.80665,
    "leiturasEstaveis": 5,
    "toleranciaEstabilidade": 0.5,
    "capacidadeMaximaGramas": 5000.0,
    "percentualAcuracia": 0.05,
    "tareOffset": 0,
    "timeoutCalibracao": 10000,
    "numAmostrasMedia": 10
}
```

**3. Status/Notificações:**
```json
{
    "type": "status",
    "message": "Tara concluída com sucesso!"
}
```

**4. Confirmação de Salvamento MySQL:**
```json
{
    "type": "mysql_save_success",
    "message": "Sessão salva com sucesso",
    "sessionId": 42
}
```

**5. Erro MySQL:**
```json
{
    "type": "mysql_save_error",
    "message": "Erro ao salvar sessão"
}
```

**6. Atualização de Status MySQL:**
```json
{
    "type": "mysql_status_update",
    "payload": true
}
```

#### **Mensagens do Cliente para Servidor:**

**1. Comando de Tara:**
```json
{
    "cmd": "t"
}
```

**2. Comando de Calibração:**
```json
{
    "cmd": "c",
    "massa_g": 1000.0
}
```

**3. Solicitar Configurações:**
```json
{
    "cmd": "get_config"
}
```

**4. Definir Parâmetro:**
```json
{
    "cmd": "set",
    "param": "gravity",
    "value": 9.807
}
```

**5. Salvar Sessão no MySQL:**
```json
{
    "cmd": "save_session_to_mysql",
    "payload": {
        "nome": "Teste Motor 01",
        "dataInicio": "2025-10-31T10:30:00",
        "dataFim": "2025-10-31T10:35:00",
        "duracaoSegundos": 300,
        "forcaMaximaN": 125.5,
        "forcaMediaN": 45.2,
        "numLeituras": 3000,
        "leituras": [
            {"tempo_mcu_s": 0.1, "forca_n": 0.0, "timestamp": "2025-10-31T10:30:00.100"},
            {"tempo_mcu_s": 0.2, "forca_n": 12.5, "timestamp": "2025-10-31T10:30:00.200"},
            ...
        ],
        "metadata": {
            "motor": "Alpha-1",
            "propelente": "KNSB",
            "temperatura": "25°C"
        }
    }
}
```

---

## 🔌 Endpoints e APIs

### HTTP API (Porta 80)

#### **1. Servir Arquivos Estáticos**
```
GET /
GET /index.html
GET /script.js
GET /estilo.css
GET /apexcharts
GET /dataWorker.js
...
```

**Resposta:** Arquivos HTML, CSS, JS

#### **2. API de Tempo do Servidor**
```
GET /api/time
```

**Resposta:**
```json
{
    "timestamp": "2025-10-31T14:30:00.123Z",
    "timezone": "America/Sao_Paulo",
    "offset": "-03:00"
}
```

#### **3. API de Sessões**
```
GET /api/sessoes
GET /api/sessoes/:id
POST /api/sessoes
DELETE /api/sessoes/:id
```

**GET /api/sessoes - Listar todas as sessões:**
```json
[
    {
        "id": 1,
        "nome": "Teste Motor 01",
        "data_inicio": "2025-10-31T10:30:00",
        "data_fim": "2025-10-31T10:35:00",
        "duracao_segundos": 300,
        "forca_maxima_n": 125.5,
        "forca_media_n": 45.2,
        "num_leituras": 3000,
        "created_at": "2025-10-31T10:36:00"
    }
]
```

**GET /api/sessoes/:id - Detalhes da sessão com leituras:**
```json
{
    "sessao": {
        "id": 1,
        "nome": "Teste Motor 01",
        "data_inicio": "2025-10-31T10:30:00",
        ...
    },
    "leituras": [
        {"tempo_mcu_s": 0.1, "forca_n": 0.0, "timestamp": "..."},
        {"tempo_mcu_s": 0.2, "forca_n": 12.5, "timestamp": "..."},
        ...
    ]
}
```

**DELETE /api/sessoes/:id - Deletar sessão:**
```json
{
    "success": true,
    "message": "Sessão deletada com sucesso"
}
```

---

## ⚙️ Funcionalidades

### 1. **Medição em Tempo Real**
- Taxa de atualização: até 100 Hz (configurável)
- Unidades: Newtons (N), Grama-força (gf), Quilograma-força (kgf)
- Display em tempo real: Força atual, EMA, Máxima, Mínima
- Barra de esforço da célula (0-100%)
- Leituras por segundo (RPS)

### 2. **Filtros e Processamento**
- **EMA (Exponential Moving Average):** Suavização de sinal
- **Zona Morta:** Ignora variações pequenas perto de zero
- **Arredondamento Inteligente:** Baseado na acurácia da célula
- **Anti-Noising:** Filtro adaptativo com análise de desvio padrão

### 3. **Calibração**
- **Tara:** Zerar balança (Shift+T)
- **Calibração:** Com massa conhecida (Shift+C)
- **Análise de Ruído:** Calcula threshold automático (Shift+A)
- Parâmetros salvos na EEPROM do ESP32

### 4. **Gráficos Interativos**
- **Modos:**
  - Deslizante: Janela móvel (últimos N pontos)
  - Acumulado: Todos os pontos
- **Visualização:**
  - Somente pontos
  - Somente linha
  - Pontos + linha
- **Interpolação:**
  - Linha suave (cardinal)
  - Linha reta (linear)
- **Controles:**
  - Labels de dados
  - Grade ON/OFF
  - Tela cheia
  - Range automático/fixo
  - Pausar/Retomar

### 5. **Sessões de Teste**
- **Gravação:**
  - Iniciar sessão com nome
  - Captura contínua de leituras
  - Encerrar e salvar no MySQL
- **Metadados:**
  - Nome do teste
  - Data/hora início e fim
  - Duração total
  - Força máxima e média
  - Número de leituras
  - Metadados customizados (JSON)
- **Gerenciamento:**
  - Listar todas as sessões
  - Visualizar gráfico de sessão salva
  - Exportar sessão (JSON, PDF)
  - Deletar sessão

### 6. **Importação/Exportação**
- **Importar:**
  - Arquivos .txt (formato: tempo força)
  - Arquivos .json (sessões exportadas)
- **Exportar:**
  - PDF com gráfico e metadados
  - JSON completo da sessão

### 7. **Configurações Persistentes**

**ESP32 (EEPROM):**
- Fator de conversão
- Gravidade
- Offset de tara
- Leituras estáveis
- Tolerância
- Timeout de calibração
- Capacidade da célula
- Acurácia

**LocalStorage (Navegador):**
- URL do WebSocket
- Taxa de atualização do gráfico
- Tema (claro/escuro)
- Preferências de visualização

### 8. **Monitoramento e Status**
- **Indicadores:**
  - WebSocket: Conectado/Desconectado
  - MySQL: Conectado/Desconectado
  - Balança: Status do ESP32
- **Relógio do Servidor:** Sincronizado
- **Alertas:**
  - Sobrecarga da célula
  - Falha de estabilização
  - Erros de conexão

### 9. **Atalhos de Teclado**
```
Shift + T  →  Tara
Shift + C  →  Calibração
Shift + A  →  Análise de ruído
Shift + D  →  Debug
P          →  Pausar gráfico
L          →  Limpar gráfico
```

---

## 🚀 Configuração e Deploy

### Pré-requisitos
- Docker + Docker Compose
- Dispositivo serial USB (ESP32/ESP8266)
- Porta 80 e 81 disponíveis
- Porta 3306 para MySQL (opcional)

### Deploy com Docker Compose

**1. Configurar variáveis de ambiente:**
```bash
# Editar docker-compose.yml
SERIAL_PORT: "/dev/ttyUSB0"      # Ou /dev/ttyACM0
SERIAL_BAUD: "921600"            # Taxa de comunicação
MYSQL_HOST: "127.0.0.1"          # Localhost (network_mode: host)
```

**2. Build e iniciar:**
```bash
docker compose build --no-cache
docker compose up -d
```

**3. Verificar logs:**
```bash
docker logs balanca -f
docker logs balanca_mysql -f
```

**4. Acessar interface:**
```
HTTP:  http://[ip]:80
IPv6:  http://[2804:4950:1:803e:...]:80
```

### Configuração de Rede

**1. Criar Access Point (Hotspot):**
```bash
bash hotspot.sh
```

**2. Configurar Ethernet DHCP:**
```bash
bash configure_ethernet_dhcp.sh
```

**3. Configurar WiFi:**
```bash
bash setup_wifi.sh
```

### Deploy em Produção

**Raspberry Pi / TV Box:**
```bash
# 1. Clonar repositório
git clone https://github.com/rbeninca/balancaGFIG
cd balancaGFIG

# 2. Configurar permissões serial
sudo usermod -a -G dialout $USER

# 3. Iniciar serviços
docker compose up -d

# 4. Habilitar auto-start
sudo systemctl enable docker
```

---

## 🔄 Fluxo de Dados

### Fluxo de Leitura (ESP32 → Cliente Web)

```
1. Célula de Carga
   └─> Deformação física (força aplicada)
       └─> Variação de resistência (Wheatstone bridge)

2. HX711 (ADC 24-bit)
   └─> Amplificação (128x)
   └─> Conversão analógico → digital
   └─> Leitura bruta (int32)

3. ESP32 (main.cpp)
   └─> Aplicar offset de tara
   └─> Multiplicar por fator de conversão → Newtons
   └─> Aplicar filtros (EMA, zona morta, arredondamento)
   └─> Montar PacketData binário (16 bytes)
   └─> Calcular CRC16
   └─> Enviar via Serial USB @ 921600 baud

4. Servidor Python (server.py)
   └─> Ler bytes da porta serial
   └─> Validar magic number e CRC
   └─> Parsear estrutura binária
   └─> Converter para JSON
   └─> Broadcast via WebSocket para todos os clientes

5. Cliente Web (dataWorker.js)
   └─> Receber JSON via WebSocket
   └─> Armazenar em buffer
   └─> Calcular EMA, RPS, máximo, mínimo
   └─> Enviar para thread principal (postMessage)

6. Interface Web (script.js)
   └─> Atualizar display numérico
   └─> Atualizar barra de esforço
   └─> Adicionar ponto ao gráfico
   └─> Atualizar tabela (se aba ativa)
   └─> Salvar em sessão (se gravando)
```

### Fluxo de Comando (Cliente Web → ESP32)

```
1. Interface Web (script.js)
   └─> Evento de usuário (botão, atalho)
   └─> Montar JSON do comando
   └─> Enviar para Web Worker (postMessage)

2. Web Worker (dataWorker.js)
   └─> Receber comando
   └─> Validar estrutura JSON
   └─> Enviar via WebSocket

3. Servidor Python (server.py)
   └─> Receber JSON do WebSocket
   └─> Identificar tipo de comando
   └─> Montar estrutura binária apropriada
   └─> Calcular CRC16
   └─> Enviar bytes via Serial

4. ESP32 (main.cpp)
   └─> Ler bytes da Serial
   └─> Validar magic number e CRC
   └─> Identificar tipo de comando
   └─> Executar ação:
       - Tara: Zerar offset
       - Calibração: Calcular fator de conversão
       - Set Param: Atualizar parâmetro
       - Get Config: Enviar PacketConfig
   └─> Salvar na EEPROM (se necessário)
   └─> Enviar PacketStatus (confirmação)

5. Servidor Python (server.py)
   └─> Receber PacketStatus
   └─> Parsear e converter para JSON
   └─> Broadcast para clientes WebSocket

6. Cliente Web
   └─> Receber confirmação
   └─> Exibir notificação ao usuário
   └─> Atualizar UI (se necessário)
```

### Fluxo de Salvamento de Sessão

```
1. Interface Web (script.js)
   └─> Usuário clica "Iniciar Sessão"
   └─> Criar objeto de sessão em memória
   └─> Iniciar captura de leituras

2. Durante Gravação
   └─> Cada leitura é adicionada ao array
   └─> Cálculo contínuo de: máximo, média, contagem

3. Encerrar Sessão
   └─> Calcular duração total
   └─> Montar objeto completo:
       {
         nome, dataInicio, dataFim,
         leituras[], metadata{}
       }
   └─> Enviar via WebSocket

4. Servidor Python (server.py)
   └─> Receber JSON da sessão
   └─> Conectar ao MySQL
   └─> Iniciar transação
   └─> INSERT INTO sessoes (...)
   └─> Obter session_id
   └─> INSERT INTO leituras (...) (bulk insert)
   └─> COMMIT
   └─> Enviar confirmação ao cliente

5. Cliente Web
   └─> Receber confirmação
   └─> Exibir notificação de sucesso
   └─> Recarregar lista de sessões
   └─> Limpar buffer local
```

---

## 📊 Diagrama de Sequência - Calibração

```
Cliente Web          Web Worker        Servidor Python      ESP32
    |                    |                    |               |
    |  Shift+C           |                    |               |
    |  massa_g=1000      |                    |               |
    |------------------->|                    |               |
    |                    |  {cmd:"c",         |               |
    |                    |   massa_g:1000}    |               |
    |                    |------------------->|               |
    |                    |                    | CMD_CALIBRATE |
    |                    |                    | (10 bytes)    |
    |                    |                    |-------------->|
    |                    |                    |               | Aguardar estabilização
    |                    |                    |               | Calcular fator
    |                    |                    |               | Salvar EEPROM
    |                    |                    | PacketStatus  |
    |                    |                    | (SUCCESS)     |
    |                    |                    |<--------------|
    |                    |  {type:"status",   |               |
    |                    |   message:"Calib   |               |
    |                    |   concluída"}      |               |
    |                    |<-------------------|               |
    |  Notificação       |                    |               |
    |  "Calibração OK"   |                    |               |
    |<-------------------|                    |               |
    |                    |                    | PacketConfig  |
    |                    |                    | (nova config) |
    |                    |                    |<--------------|
    |                    |  {type:"config",   |               |
    |                    |   conversionFactor:|               |
    |                    |   215.5...}        |               |
    |                    |<-------------------|               |
    |  Atualizar form    |                    |               |
    |<-------------------|                    |               |
```

---

## 🔐 Segurança e Boas Práticas

### Segurança
- [ ] Validação de CRC em todos os pacotes binários
- [ ] Sanitização de inputs JSON
- [ ] Timeout em operações de calibração
- [ ] Healthcheck do MySQL
- [ ] Logs estruturados
- [ ] Limites de memória nos containers

### Performance
- [ ] Protocolo binário compacto (16 bytes/leitura)
- [ ] Web Workers para processamento assíncrono
- [ ] Bulk insert no MySQL (lotes de leituras)
- [ ] Índices de banco de dados otimizados
- [ ] Compressão de logs Docker

### Manutenibilidade
- [ ] Versionamento do protocolo binário
- [ ] Documentação inline no código
- [ ] Testes de integração
- [ ] Scripts de automação (deploy, backup)
- [ ] Monitoramento de recursos

---

## 📚 Referências

### Documentos do Projeto
- [INSTALL.md](INSTALL.md) - Guia de instalação
- [ETHERNET_DHCP_CONFIG.md](ETHERNET_DHCP_CONFIG.md) - Configuração de rede
- [IPV6_IMPLEMENTATION_SUMMARY.md](IPV6_IMPLEMENTATION_SUMMARY.md) - Suporte IPv6
- [PERFORMANCE_OPTIMIZATION.md](PERFORMANCE_OPTIMIZATION.md) - Otimizações
- [REPOSITORY_ANALYSIS.md](REPOSITORY_ANALYSIS.md) - Análise do repositório

### Tecnologias
- [PlatformIO](https://platformio.org/) - Build system ESP32
- [Python asyncio](https://docs.python.org/3/library/asyncio.html)
- [websockets](https://websockets.readthedocs.io/)
- [pyserial](https://pythonhosted.org/pyserial/)
- [ApexCharts](https://apexcharts.com/)
- [MariaDB](https://mariadb.org/)

### Hardware
- [HX711 Datasheet](https://cdn.sparkfun.com/datasheets/Sensors/ForceFlex/hx711_english.pdf)
- [ESP32 Documentation](https://docs.espressif.com/projects/esp-idf/en/latest/esp32/)

---

**Versão:** 1.0  
**Data:** 31/10/2025  
**Autor:** Sistema de Balança GFIG
