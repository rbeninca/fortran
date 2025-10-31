# 🔄 Proposta de Refatoração - Arquitetura MVC

## 📋 Índice

1. [Problemas Atuais](#problemas-atuais)
2. [Arquitetura Proposta](#arquitetura-proposta)
3. [Backend (Python)](#backend-python)
4. [Frontend (JavaScript)](#frontend-javascript)
5. [ESP32 (Firmware)](#esp32-firmware)
6. [Plano de Migração](#plano-de-migração)
7. [Benefícios Esperados](#benefícios-esperados)

---

## ⚠️ Problemas Atuais

### Backend (`server.py`)
```python
# ❌ PROBLEMA: Arquivo monolítico de 1001 linhas
# - Responsabilidades misturadas
# - HTTP, WebSocket, Serial, MySQL, Protocol parsing no mesmo arquivo
# - Difícil de testar unitariamente
# - Difícil de manter e evoluir
```

**Violações de SOLID:**
- **SRP (Single Responsibility):** Um arquivo faz HTTP + WS + Serial + DB + Protocol
- **OCP (Open/Closed):** Adicionar novo tipo de pacote requer editar múltiplas seções
- **DIP (Dependency Inversion):** Acoplamento direto com pymysql, pyserial

### Frontend (JavaScript)

**`script.js` - 3075 linhas:**
```javascript
// ❌ PROBLEMA: Deus object
// - UI, lógica de negócio, comunicação, persistência tudo junto
// - Funções globais sem namespacing
// - Difícil de testar
// - State management caótico
```

**`dataWorker.js` - 557 linhas:**
```javascript
// ❌ PROBLEMA: Worker com responsabilidades demais
// - WebSocket + protocolo + parsing + buffering
// - Lógica de reconexão misturada com processamento
```

### ESP32 (`main.cpp`)

**911 linhas:**
```cpp
// ❌ PROBLEMA: Código procedural em loop()
// - Protocolo, HX711, EEPROM, Display tudo em main.cpp
// - Difícil de testar componentes isoladamente
// - Falta abstração
```

---

## 🏗️ Arquitetura Proposta

### Princípios Adotados

1. **MVC (Model-View-Controller)**
2. **SOLID Principles**
3. **Separation of Concerns**
4. **Dependency Injection**
5. **Layered Architecture**

### Estrutura Geral

```
┌─────────────────────────────────────────────────────────┐
│                    PRESENTATION LAYER                   │
│  ┌───────────────────────────────────────────────────┐  │
│  │  Views (HTML Templates)                           │  │
│  │  Controllers (Event Handlers)                     │  │
│  │  ViewModels (State Management)                    │  │
│  └───────────────────────────────────────────────────┘  │
└─────────────────────────┬───────────────────────────────┘
                          │
┌─────────────────────────▼───────────────────────────────┐
│                   APPLICATION LAYER                     │
│  ┌───────────────────────────────────────────────────┐  │
│  │  Services (Business Logic)                        │  │
│  │  Use Cases (Application Flow)                     │  │
│  │  DTOs (Data Transfer Objects)                     │  │
│  └───────────────────────────────────────────────────┘  │
└─────────────────────────┬───────────────────────────────┘
                          │
┌─────────────────────────▼───────────────────────────────┐
│                     DOMAIN LAYER                        │
│  ┌───────────────────────────────────────────────────┐  │
│  │  Models (Entities)                                │  │
│  │  Repositories (Data Access Interfaces)            │  │
│  │  Domain Services                                  │  │
│  └───────────────────────────────────────────────────┘  │
└─────────────────────────┬───────────────────────────────┘
                          │
┌─────────────────────────▼───────────────────────────────┐
│                 INFRASTRUCTURE LAYER                    │
│  ┌───────────────────────────────────────────────────┐  │
│  │  Database (MySQL Implementation)                  │  │
│  │  Serial (PySerial Implementation)                 │  │
│  │  WebSocket (websockets Implementation)            │  │
│  │  Protocol (Binary Protocol Implementation)        │  │
│  └───────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

---

## 🐍 Backend (Python)

### Nova Estrutura de Diretórios

```
backend/
├── main.py                     # Entry point
├── config/
│   ├── __init__.py
│   ├── settings.py             # Configurações (env vars)
│   └── logging_config.py       # Configuração de logs
│
├── domain/
│   ├── __init__.py
│   ├── models/
│   │   ├── __init__.py
│   │   ├── leitura.py          # Classe Leitura
│   │   ├── sessao.py           # Classe Sessao
│   │   └── configuracao.py     # Classe Configuracao
│   │
│   ├── repositories/
│   │   ├── __init__.py
│   │   ├── base.py             # Interface Repository
│   │   ├── sessao_repository.py
│   │   └── leitura_repository.py
│   │
│   └── services/
│       ├── __init__.py
│       └── calibration_service.py
│
├── application/
│   ├── __init__.py
│   ├── use_cases/
│   │   ├── __init__.py
│   │   ├── salvar_sessao.py    # Use case: Salvar sessão
│   │   ├── calibrar_balanca.py # Use case: Calibrar
│   │   └── obter_leituras.py   # Use case: Obter leituras
│   │
│   └── dtos/
│       ├── __init__.py
│       ├── leitura_dto.py
│       └── sessao_dto.py
│
├── infrastructure/
│   ├── __init__.py
│   ├── database/
│   │   ├── __init__.py
│   │   ├── connection.py       # Connection pool
│   │   ├── migrations/         # SQL migrations
│   │   └── repositories/
│   │       ├── __init__.py
│   │       ├── mysql_sessao_repository.py
│   │       └── mysql_leitura_repository.py
│   │
│   ├── serial/
│   │   ├── __init__.py
│   │   ├── serial_adapter.py   # Abstração pyserial
│   │   └── auto_detect.py      # Auto-detect porta
│   │
│   ├── protocol/
│   │   ├── __init__.py
│   │   ├── binary_protocol.py  # Parser binário
│   │   ├── packet_types.py     # Estruturas de pacotes
│   │   ├── crc.py              # CRC16-CCITT
│   │   └── converters.py       # Binary ↔ JSON
│   │
│   └── websocket/
│       ├── __init__.py
│       ├── ws_server.py        # WebSocket server
│       ├── connection_manager.py
│       └── message_handler.py
│
├── presentation/
│   ├── __init__.py
│   ├── http/
│   │   ├── __init__.py
│   │   ├── server.py           # HTTP server
│   │   ├── routes/
│   │   │   ├── __init__.py
│   │   │   ├── api_routes.py   # /api/*
│   │   │   └── static_routes.py
│   │   │
│   │   └── controllers/
│   │       ├── __init__.py
│   │       ├── sessao_controller.py
│   │       └── config_controller.py
│   │
│   └── websocket/
│       ├── __init__.py
│       └── ws_controller.py    # WebSocket message routing
│
└── tests/
    ├── unit/
    │   ├── domain/
    │   ├── application/
    │   └── infrastructure/
    │
    └── integration/
        ├── test_serial.py
        ├── test_database.py
        └── test_websocket.py
```

### Exemplo de Implementação

#### `domain/models/leitura.py`
```python
"""
Entidade de domínio: Leitura
"""
from dataclasses import dataclass
from datetime import datetime
from typing import Optional

@dataclass(frozen=True)
class Leitura:
    """Representa uma leitura de força da balança"""
    tempo_mcu_s: float
    forca_n: float
    timestamp: datetime
    sessao_id: Optional[int] = None
    id: Optional[int] = None
    
    def __post_init__(self):
        if self.tempo_mcu_s < 0:
            raise ValueError("tempo_mcu_s deve ser >= 0")
        
    def to_dict(self) -> dict:
        return {
            'id': self.id,
            'tempo_mcu_s': self.tempo_mcu_s,
            'forca_n': self.forca_n,
            'timestamp': self.timestamp.isoformat(),
            'sessao_id': self.sessao_id
        }
    
    @classmethod
    def from_dict(cls, data: dict) -> 'Leitura':
        return cls(
            tempo_mcu_s=data['tempo_mcu_s'],
            forca_n=data['forca_n'],
            timestamp=datetime.fromisoformat(data['timestamp']),
            sessao_id=data.get('sessao_id'),
            id=data.get('id')
        )
```

#### `domain/repositories/sessao_repository.py`
```python
"""
Interface do repositório de sessões (Dependency Inversion Principle)
"""
from abc import ABC, abstractmethod
from typing import List, Optional
from domain.models.sessao import Sessao

class SessaoRepository(ABC):
    """Interface para acesso a dados de sessões"""
    
    @abstractmethod
    async def save(self, sessao: Sessao) -> int:
        """Salva uma sessão e retorna o ID"""
        pass
    
    @abstractmethod
    async def find_by_id(self, sessao_id: int) -> Optional[Sessao]:
        """Busca sessão por ID"""
        pass
    
    @abstractmethod
    async def find_all(self) -> List[Sessao]:
        """Lista todas as sessões"""
        pass
    
    @abstractmethod
    async def delete(self, sessao_id: int) -> bool:
        """Deleta uma sessão"""
        pass
```

#### `infrastructure/database/repositories/mysql_sessao_repository.py`
```python
"""
Implementação MySQL do repositório de sessões
"""
from typing import List, Optional
import pymysql
from domain.models.sessao import Sessao
from domain.repositories.sessao_repository import SessaoRepository
from infrastructure.database.connection import get_connection

class MySQLSessaoRepository(SessaoRepository):
    """Implementação MySQL do SessaoRepository"""
    
    def __init__(self):
        self.connection_pool = get_connection()
    
    async def save(self, sessao: Sessao) -> int:
        """Salva sessão no MySQL"""
        conn = self.connection_pool.get_connection()
        try:
            with conn.cursor() as cursor:
                sql = """
                    INSERT INTO sessoes 
                    (nome, data_inicio, data_fim, duracao_segundos, 
                     forca_maxima_n, forca_media_n, num_leituras, metadata)
                    VALUES (%s, %s, %s, %s, %s, %s, %s, %s)
                """
                cursor.execute(sql, (
                    sessao.nome,
                    sessao.data_inicio,
                    sessao.data_fim,
                    sessao.duracao_segundos,
                    sessao.forca_maxima_n,
                    sessao.forca_media_n,
                    sessao.num_leituras,
                    sessao.metadata_json
                ))
                conn.commit()
                return cursor.lastrowid
        finally:
            conn.close()
    
    async def find_by_id(self, sessao_id: int) -> Optional[Sessao]:
        """Busca sessão por ID"""
        conn = self.connection_pool.get_connection()
        try:
            with conn.cursor(pymysql.cursors.DictCursor) as cursor:
                sql = "SELECT * FROM sessoes WHERE id = %s"
                cursor.execute(sql, (sessao_id,))
                row = cursor.fetchone()
                return Sessao.from_dict(row) if row else None
        finally:
            conn.close()
    
    # ... outros métodos
```

#### `application/use_cases/salvar_sessao.py`
```python
"""
Use Case: Salvar Sessão de Teste
"""
from typing import List
from domain.models.sessao import Sessao
from domain.models.leitura import Leitura
from domain.repositories.sessao_repository import SessaoRepository
from domain.repositories.leitura_repository import LeituraRepository
from application.dtos.sessao_dto import SessaoDTO

class SalvarSessaoUseCase:
    """Caso de uso para salvar uma sessão de teste"""
    
    def __init__(
        self,
        sessao_repository: SessaoRepository,
        leitura_repository: LeituraRepository
    ):
        self.sessao_repo = sessao_repository
        self.leitura_repo = leitura_repository
    
    async def execute(self, sessao_dto: SessaoDTO) -> int:
        """
        Salva uma sessão com suas leituras
        
        Returns:
            ID da sessão salva
        """
        # 1. Converter DTO para entidade
        sessao = Sessao(
            nome=sessao_dto.nome,
            data_inicio=sessao_dto.data_inicio,
            data_fim=sessao_dto.data_fim,
            duracao_segundos=sessao_dto.duracao_segundos,
            forca_maxima_n=sessao_dto.forca_maxima_n,
            forca_media_n=sessao_dto.forca_media_n,
            num_leituras=len(sessao_dto.leituras),
            metadata=sessao_dto.metadata
        )
        
        # 2. Salvar sessão (transaction)
        sessao_id = await self.sessao_repo.save(sessao)
        
        # 3. Salvar leituras em batch
        leituras = [
            Leitura(
                tempo_mcu_s=l.tempo_mcu_s,
                forca_n=l.forca_n,
                timestamp=l.timestamp,
                sessao_id=sessao_id
            )
            for l in sessao_dto.leituras
        ]
        
        await self.leitura_repo.save_batch(leituras)
        
        return sessao_id
```

#### `infrastructure/protocol/binary_protocol.py`
```python
"""
Parser de protocolo binário
"""
import struct
from typing import Optional, Dict, Any
from infrastructure.protocol.crc import crc16_ccitt
from infrastructure.protocol.packet_types import PacketType

class BinaryProtocol:
    """Parser e builder de pacotes binários"""
    
    MAGIC = 0xA1B2
    VERSION = 0x01
    
    @staticmethod
    def parse_packet(data: bytes) -> Optional[Dict[str, Any]]:
        """
        Parse pacote binário para dict
        
        Args:
            data: Bytes do pacote
            
        Returns:
            Dict com dados parseados ou None se inválido
        """
        if len(data) < 8:
            return None
        
        # Header: magic(2) + version(1) + type(1)
        magic, version, packet_type = struct.unpack('<HBB', data[:4])
        
        if magic != BinaryProtocol.MAGIC:
            return None
        
        if version != BinaryProtocol.VERSION:
            return None
        
        # Validar CRC
        crc_received = struct.unpack('<H', data[-2:])[0]
        crc_calculated = crc16_ccitt(data[:-2])
        
        if crc_received != crc_calculated:
            return None
        
        # Parse baseado no tipo
        if packet_type == PacketType.DATA:
            return BinaryProtocol._parse_data_packet(data)
        elif packet_type == PacketType.CONFIG:
            return BinaryProtocol._parse_config_packet(data)
        elif packet_type == PacketType.STATUS:
            return BinaryProtocol._parse_status_packet(data)
        
        return None
    
    @staticmethod
    def _parse_data_packet(data: bytes) -> Dict[str, Any]:
        """Parse PacketData (16 bytes)"""
        if len(data) != 16:
            return None
        
        _, _, _, t_ms, forca_n, status, _ = struct.unpack(
            '<HBBIfBBH',
            data
        )
        
        return {
            'type': 'data',
            'tempo': t_ms / 1000.0,
            'forca': forca_n,
            'status': status
        }
    
    @staticmethod
    def build_tara_command() -> bytes:
        """Constrói comando de tara (8 bytes)"""
        packet = struct.pack('<HBBBB', 
            BinaryProtocol.MAGIC,
            BinaryProtocol.VERSION,
            0x10,  # CMD_TARA
            0, 0   # Reserved
        )
        crc = crc16_ccitt(packet)
        return packet + struct.pack('<H', crc)
    
    @staticmethod
    def build_calibrate_command(massa_g: float) -> bytes:
        """Constrói comando de calibração (10 bytes)"""
        packet = struct.pack('<HBBf',
            BinaryProtocol.MAGIC,
            BinaryProtocol.VERSION,
            0x11,  # CMD_CALIBRATE
            massa_g
        )
        crc = crc16_ccitt(packet)
        return packet + struct.pack('<H', crc)
    
    # ... outros builders
```

#### `presentation/http/controllers/sessao_controller.py`
```python
"""
Controller HTTP para sessões
"""
from http.server import BaseHTTPRequestHandler
from application.use_cases.salvar_sessao import SalvarSessaoUseCase
from application.use_cases.obter_sessoes import ObterSessoesUseCase
from application.dtos.sessao_dto import SessaoDTO
import json

class SessaoController:
    """Controller para endpoints de sessões"""
    
    def __init__(
        self,
        salvar_sessao_use_case: SalvarSessaoUseCase,
        obter_sessoes_use_case: ObterSessoesUseCase
    ):
        self.salvar_use_case = salvar_sessao_use_case
        self.obter_use_case = obter_sessoes_use_case
    
    async def list_sessoes(self, request: BaseHTTPRequestHandler):
        """GET /api/sessoes"""
        sessoes = await self.obter_use_case.execute()
        
        request.send_response(200)
        request.send_header('Content-Type', 'application/json')
        request.end_headers()
        
        response = [s.to_dict() for s in sessoes]
        request.wfile.write(json.dumps(response).encode())
    
    async def create_sessao(self, request: BaseHTTPRequestHandler):
        """POST /api/sessoes"""
        content_length = int(request.headers['Content-Length'])
        body = request.rfile.read(content_length)
        data = json.loads(body)
        
        # Converter para DTO
        sessao_dto = SessaoDTO.from_dict(data)
        
        # Executar use case
        sessao_id = await self.salvar_use_case.execute(sessao_dto)
        
        request.send_response(201)
        request.send_header('Content-Type', 'application/json')
        request.end_headers()
        
        response = {'id': sessao_id, 'message': 'Sessão salva com sucesso'}
        request.wfile.write(json.dumps(response).encode())
    
    # ... outros métodos
```

#### `main.py`
```python
"""
Entry point da aplicação
"""
import asyncio
from config.settings import Settings
from config.logging_config import setup_logging
from infrastructure.database.connection import DatabaseConnection
from infrastructure.database.repositories.mysql_sessao_repository import MySQLSessaoRepository
from infrastructure.database.repositories.mysql_leitura_repository import MySQLLeituraRepository
from infrastructure.serial.serial_adapter import SerialAdapter
from infrastructure.protocol.binary_protocol import BinaryProtocol
from infrastructure.websocket.ws_server import WebSocketServer
from application.use_cases.salvar_sessao import SalvarSessaoUseCase
from application.use_cases.obter_sessoes import ObterSessoesUseCase
from presentation.http.server import HTTPServer
from presentation.http.controllers.sessao_controller import SessaoController

async def main():
    """Inicializa a aplicação"""
    
    # 1. Setup
    settings = Settings()
    setup_logging(settings.log_level)
    
    # 2. Infrastructure
    db = DatabaseConnection(settings.database_config)
    serial = SerialAdapter(settings.serial_port, settings.serial_baud)
    protocol = BinaryProtocol()
    
    # 3. Repositories (DI)
    sessao_repo = MySQLSessaoRepository()
    leitura_repo = MySQLLeituraRepository()
    
    # 4. Use Cases (DI)
    salvar_sessao = SalvarSessaoUseCase(sessao_repo, leitura_repo)
    obter_sessoes = ObterSessoesUseCase(sessao_repo)
    
    # 5. Controllers (DI)
    sessao_controller = SessaoController(salvar_sessao, obter_sessoes)
    
    # 6. Servers
    http_server = HTTPServer(
        port=settings.http_port,
        controllers={'sessao': sessao_controller}
    )
    
    ws_server = WebSocketServer(
        port=settings.ws_port,
        serial_adapter=serial,
        protocol=protocol
    )
    
    # 7. Start
    await asyncio.gather(
        http_server.start(),
        ws_server.start(),
        serial.start_reading()
    )

if __name__ == '__main__':
    asyncio.run(main())
```

---

## 🎨 Frontend (JavaScript)

### Nova Estrutura de Diretórios

```
frontend/
├── index.html
├── rede.html
│
├── src/
│   ├── main.js                 # Entry point
│   │
│   ├── core/
│   │   ├── EventBus.js         # Pub/Sub pattern
│   │   ├── Router.js           # Gerenciamento de abas
│   │   └── DependencyContainer.js
│   │
│   ├── models/
│   │   ├── Leitura.js
│   │   ├── Sessao.js
│   │   └── Configuracao.js
│   │
│   ├── services/
│   │   ├── WebSocketService.js
│   │   ├── APIService.js       # HTTP API client
│   │   ├── StorageService.js   # LocalStorage
│   │   └── WorkerService.js    # Web Worker manager
│   │
│   ├── repositories/
│   │   ├── SessaoRepository.js
│   │   └── ConfigRepository.js
│   │
│   ├── controllers/
│   │   ├── GraficoController.js
│   │   ├── TabelaController.js
│   │   ├── SessaoController.js
│   │   └── ConfigController.js
│   │
│   ├── views/
│   │   ├── components/
│   │   │   ├── GraficoView.js
│   │   │   ├── TabelaView.js
│   │   │   ├── StatusBar.js
│   │   │   ├── ControlPanel.js
│   │   │   └── NotificationManager.js
│   │   │
│   │   └── pages/
│   │       ├── GraficoPage.js
│   │       ├── GravacoesPage.js
│   │       └── ConfigPage.js
│   │
│   ├── workers/
│   │   ├── WorkerOrchestrator.js    # ⭐ Coordenador principal
│   │   ├── websocket/
│   │   │   └── WebSocketWorker.js   # Comunicação WebSocket
│   │   ├── processing/
│   │   │   ├── DataProcessingWorker.js  # Processamento de dados
│   │   │   └── ChartWorker.js           # Cálculos do gráfico
│   │   ├── storage/
│   │   │   └── StorageWorker.js     # Persistência assíncrona
│   │   └── export/
│   │       └── ExportWorker.js      # PDF/JSON export
│   │
│   ├── utils/
│   │   ├── formatters.js       # Formatação de números
│   │   ├── validators.js       # Validações
│   │   ├── converters.js       # Conversões de unidades
│   │   └── exporters.js        # PDF, JSON export
│   │
│   └── config/
│       ├── constants.js
│       └── chartConfig.js
│
├── assets/
│   ├── css/
│   │   ├── main.css
│   │   ├── components/
│   │   └── themes/
│   │
│   ├── js/
│   │   └── vendor/
│   │       ├── apexcharts.min.js
│   │       └── chartist.min.js
│   │
│   └── images/
│
└── tests/
    ├── unit/
    ├── integration/
    └── e2e/
```

### Exemplo de Implementação

#### `models/Leitura.js`
```javascript
/**
 * Model: Leitura
 */
export class Leitura {
    constructor(tempoMcuS, forcaN, timestamp = new Date()) {
        this._tempoMcuS = tempoMcuS;
        this._forcaN = forcaN;
        this._timestamp = timestamp;
    }
    
    get tempoMcuS() {
        return this._tempoMcuS;
    }
    
    get forcaN() {
        return this._forcaN;
    }
    
    get timestamp() {
        return this._timestamp;
    }
    
    /**
     * Converte força para outras unidades
     */
    getForcaGf() {
        return this._forcaN * 101.97162129779;
    }
    
    getForcaKgf() {
        return this._forcaN / 9.80665;
    }
    
    toJSON() {
        return {
            tempo_mcu_s: this._tempoMcuS,
            forca_n: this._forcaN,
            timestamp: this._timestamp.toISOString()
        };
    }
    
    static fromJSON(json) {
        return new Leitura(
            json.tempo_mcu_s,
            json.forca_n,
            new Date(json.timestamp)
        );
    }
}
```

#### `services/WebSocketService.js`
```javascript
/**
 * Service: WebSocket Communication
 */
import { EventBus } from '../core/EventBus.js';

export class WebSocketService {
    constructor(url, reconnectConfig = {}) {
        this.url = url;
        this.reconnectAttempts = 0;
        this.maxReconnectAttempts = reconnectConfig.maxAttempts || 10;
        this.reconnectInterval = reconnectConfig.interval || 3000;
        this.eventBus = EventBus.getInstance();
        this.socket = null;
    }
    
    connect() {
        if (this._shouldSkipConnection()) {
            this.eventBus.emit('ws:offline', 'GitHub Pages - Modo offline');
            return;
        }
        
        try {
            this.socket = new WebSocket(this.url);
            this._setupEventHandlers();
        } catch (error) {
            this.eventBus.emit('ws:error', error.message);
        }
    }
    
    send(message) {
        if (!this.socket || this.socket.readyState !== WebSocket.OPEN) {
            throw new Error('WebSocket não conectado');
        }
        
        const json = JSON.stringify(message);
        this.socket.send(json);
    }
    
    disconnect() {
        if (this.socket) {
            this.socket.close();
            this.socket = null;
        }
    }
    
    _shouldSkipConnection() {
        return location.hostname.includes('github.io');
    }
    
    _setupEventHandlers() {
        this.socket.onopen = () => {
            this.reconnectAttempts = 0;
            this.eventBus.emit('ws:connected');
        };
        
        this.socket.onmessage = (event) => {
            const data = JSON.parse(event.data);
            this.eventBus.emit(`ws:${data.type}`, data);
        };
        
        this.socket.onclose = () => {
            this.eventBus.emit('ws:disconnected');
            this._attemptReconnect();
        };
        
        this.socket.onerror = (error) => {
            this.eventBus.emit('ws:error', error);
        };
    }
    
    _attemptReconnect() {
        if (this.reconnectAttempts >= this.maxReconnectAttempts) {
            this.eventBus.emit('ws:reconnect-failed');
            return;
        }
        
        this.reconnectAttempts++;
        
        setTimeout(() => {
            this.connect();
        }, this.reconnectInterval);
    }
}
```

#### `controllers/GraficoController.js`
```javascript
/**
 * Controller: Gráfico
 */
import { EventBus } from '../core/EventBus.js';
import { GraficoView } from '../views/components/GraficoView.js';
import { Leitura } from '../models/Leitura.js';

export class GraficoController {
    constructor(graficoView) {
        this.view = graficoView;
        this.eventBus = EventBus.getInstance();
        this.dataBuffer = [];
        this.mode = 'deslizante';
        this.maxPoints = 100;
        this.isPaused = false;
        
        this._setupEventListeners();
    }
    
    _setupEventListeners() {
        // Receber dados do WebSocket
        this.eventBus.on('ws:data', (data) => {
            this.handleNovaLeitura(data);
        });
        
        // Comandos da UI
        this.eventBus.on('grafico:clear', () => this.clearGrafico());
        this.eventBus.on('grafico:setMode', (mode) => this.setMode(mode));
        this.eventBus.on('grafico:togglePause', () => this.togglePause());
    }
    
    handleNovaLeitura(data) {
        if (this.isPaused) return;
        
        const leitura = new Leitura(data.tempo, data.forca);
        
        if (this.mode === 'deslizante') {
            this.dataBuffer.push(leitura);
            
            if (this.dataBuffer.length > this.maxPoints) {
                this.dataBuffer.shift();
            }
        } else {
            this.dataBuffer.push(leitura);
        }
        
        this.view.updateChart(this.dataBuffer);
    }
    
    setMode(mode) {
        this.mode = mode;
        this.eventBus.emit('grafico:modeChanged', mode);
    }
    
    clearGrafico() {
        this.dataBuffer = [];
        this.view.clearChart();
    }
    
    togglePause() {
        this.isPaused = !this.isPaused;
        this.eventBus.emit('grafico:pauseChanged', this.isPaused);
    }
}
```

#### `views/components/GraficoView.js`
```javascript
/**
 * View: Gráfico
 */
export class GraficoView {
    constructor(containerId, chartOptions) {
        this.container = document.getElementById(containerId);
        this.chart = null;
        this.options = chartOptions;
        this._initialize();
    }
    
    _initialize() {
        this.chart = new ApexCharts(this.container, this.options);
        this.chart.render();
    }
    
    updateChart(leituras) {
        const seriesData = leituras.map(l => ({
            x: l.tempoMcuS,
            y: l.forcaN
        }));
        
        this.chart.updateSeries([{
            data: seriesData
        }]);
    }
    
    clearChart() {
        this.chart.updateSeries([{
            data: []
        }]);
    }
    
    setDisplayUnit(unit) {
        // Atualizar labels do eixo Y
        this.chart.updateOptions({
            yaxis: {
                title: { text: `Força (${unit})` }
            }
        });
    }
    
    destroy() {
        if (this.chart) {
            this.chart.destroy();
        }
    }
}
```

#### `core/EventBus.js`
```javascript
/**
 * EventBus: Publish-Subscribe Pattern
 * Desacopla componentes através de eventos
 */
export class EventBus {
    static instance = null;
    
    constructor() {
        if (EventBus.instance) {
            return EventBus.instance;
        }
        
        this.listeners = new Map();
        EventBus.instance = this;
    }
    
    static getInstance() {
        if (!EventBus.instance) {
            EventBus.instance = new EventBus();
        }
        return EventBus.instance;
    }
    
    on(event, callback) {
        if (!this.listeners.has(event)) {
            this.listeners.set(event, []);
        }
        this.listeners.get(event).push(callback);
    }
    
    off(event, callback) {
        if (!this.listeners.has(event)) return;
        
        const callbacks = this.listeners.get(event);
        const index = callbacks.indexOf(callback);
        
        if (index > -1) {
            callbacks.splice(index, 1);
        }
    }
    
    emit(event, data) {
        if (!this.listeners.has(event)) return;
        
        this.listeners.get(event).forEach(callback => {
            try {
                callback(data);
            } catch (error) {
                console.error(`Erro no listener de ${event}:`, error);
            }
        });
    }
    
    clear() {
        this.listeners.clear();
    }
}
```

---

### 🔀 Arquitetura de Múltiplos Web Workers

#### Problema Atual

**Worker Monolítico (`dataWorker.js`):**
```javascript
// ❌ Um único worker faz TUDO:
// - Gerencia conexão WebSocket
// - Parse de protocolo
// - Buffering de dados
// - Processamento de leituras
// - Cálculo de EMA
// - Reconexão automática
```

**Consequências:**
- ⚠️ Worker sobrecarregado
- ⚠️ Difícil escalar processamento
- ⚠️ Thread bloqueada durante cálculos pesados
- ⚠️ Sem isolamento de responsabilidades

---

#### Solução: Orquestrador + Workers Especializados

```
┌─────────────────────────────────────────────────────────────┐
│                      MAIN THREAD (UI)                       │
│  ┌───────────────────────────────────────────────────────┐  │
│  │  Views, Controllers, DOM Manipulation                 │  │
│  └───────────────┬───────────────────────────────────────┘  │
└──────────────────┼──────────────────────────────────────────┘
                   │
                   │ PostMessage API
                   │
┌──────────────────▼──────────────────────────────────────────┐
│           WORKER ORCHESTRATOR (Coordenador)                 │
│  ┌───────────────────────────────────────────────────────┐  │
│  │  • Gerencia ciclo de vida dos workers                 │  │
│  │  • Roteia mensagens entre workers                     │  │
│  │  │  • Coordena pipeline de processamento              │  │
│  │  • Load balancing entre workers                       │  │
│  └─┬──────┬──────┬──────┬─────────────────────────────┬──┘  │
└────┼──────┼──────┼──────┼─────────────────────────────┼─────┘
     │      │      │      │                             │
     │      │      │      │                             │
┌────▼──┐ ┌▼────┐ ┌▼────┐ ┌▼──────────┐   ┌───────────▼────┐
│ WS    │ │Data │ │Chart│ │ Storage   │   │ Export         │
│Worker │ │Proc │ │Calc │ │Worker     │   │ Worker         │
│       │ │     │ │     │ │           │   │                │
│WebSkt │ │EMA  │ │Viz  │ │IndexedDB  │   │PDF/JSON        │
│Proto  │ │Filter│ │Math │ │Cache      │   │               │
└───────┘ └─────┘ └─────┘ └───────────┘   └────────────────┘
```

---

#### Implementação: WorkerOrchestrator

**`workers/WorkerOrchestrator.js`:**
```javascript
/**
 * Orquestrador de Web Workers
 * 
 * Responsabilidades:
 * - Criar e gerenciar workers
 * - Rotear mensagens entre workers
 * - Load balancing
 * - Fallback se worker falhar
 */

export class WorkerOrchestrator {
    constructor() {
        this.workers = new Map();
        this.messageQueue = [];
        this.isReady = false;
    }
    
    /**
     * Inicializa todos os workers
     */
    async init() {
        try {
            // 1. WebSocket Worker - Alta prioridade
            await this._createWorker('websocket', 'workers/websocket/WebSocketWorker.js', {
                priority: 'high',
                singleton: true
            });
            
            // 2. Data Processing Worker - Pool de 2 workers
            await this._createWorkerPool('dataProcessing', 
                'workers/processing/DataProcessingWorker.js', 
                { poolSize: 2 }
            );
            
            // 3. Chart Worker - Background
            await this._createWorker('chart', 'workers/processing/ChartWorker.js', {
                priority: 'low'
            });
            
            // 4. Storage Worker - Baixa prioridade
            await this._createWorker('storage', 'workers/storage/StorageWorker.js', {
                priority: 'low',
                lazy: true  // Cria apenas quando necessário
            });
            
            // 5. Export Worker - Sob demanda
            await this._createWorker('export', 'workers/export/ExportWorker.js', {
                priority: 'low',
                lazy: true,
                terminate: true  // Termina após uso
            });
            
            this.isReady = true;
            console.log('✅ Todos os workers inicializados');
        } catch (error) {
            console.error('❌ Erro ao inicializar workers:', error);
            throw error;
        }
    }
    
    /**
     * Envia mensagem para um worker específico
     */
    postMessage(workerName, message) {
        const worker = this.workers.get(workerName);
        
        if (!worker) {
            console.error(`Worker '${workerName}' não encontrado`);
            return;
        }
        
        // Se é pool, faz load balancing
        if (worker.pool) {
            const leastBusy = this._getLeastBusyWorker(worker.pool);
            leastBusy.postMessage(message);
        } else {
            worker.instance.postMessage(message);
        }
    }
    
    /**
     * Broadcast para todos os workers
     */
    broadcast(message) {
        for (const [name, worker] of this.workers) {
            if (worker.pool) {
                worker.pool.forEach(w => w.postMessage(message));
            } else {
                worker.instance.postMessage(message);
            }
        }
    }
    
    /**
     * Registra handler para mensagens de um worker
     */
    on(workerName, eventType, handler) {
        const worker = this.workers.get(workerName);
        
        if (!worker) {
            console.error(`Worker '${workerName}' não encontrado`);
            return;
        }
        
        const wrappedHandler = (event) => {
            const { type, payload } = event.data;
            if (type === eventType) {
                handler(payload);
            }
        };
        
        if (worker.pool) {
            worker.pool.forEach(w => {
                w.addEventListener('message', wrappedHandler);
            });
        } else {
            worker.instance.addEventListener('message', wrappedHandler);
        }
    }
    
    /**
     * Pipeline: encadeia processamento entre workers
     * 
     * Exemplo:
     * orchestrator.pipeline([
     *   { worker: 'websocket', action: 'receive' },
     *   { worker: 'dataProcessing', action: 'parse' },
     *   { worker: 'dataProcessing', action: 'filter' },
     *   { worker: 'chart', action: 'update' }
     * ]);
     */
    pipeline(steps) {
        return async (initialData) => {
            let data = initialData;
            
            for (const step of steps) {
                data = await this._executeStep(step.worker, step.action, data);
            }
            
            return data;
        };
    }
    
    /**
     * Termina um worker
     */
    terminate(workerName) {
        const worker = this.workers.get(workerName);
        
        if (!worker) return;
        
        if (worker.pool) {
            worker.pool.forEach(w => w.terminate());
        } else {
            worker.instance.terminate();
        }
        
        this.workers.delete(workerName);
        console.log(`🗑️ Worker '${workerName}' terminado`);
    }
    
    /**
     * Termina todos os workers
     */
    terminateAll() {
        for (const [name] of this.workers) {
            this.terminate(name);
        }
        
        this.isReady = false;
    }
    
    // =========== Métodos Privados ===========
    
    async _createWorker(name, path, options = {}) {
        try {
            const worker = new Worker(path, { type: 'module' });
            
            // Aguarda worker estar pronto
            await this._waitForReady(worker);
            
            this.workers.set(name, {
                instance: worker,
                path,
                options,
                messageCount: 0,
                errors: 0
            });
            
            // Error handling
            worker.addEventListener('error', (error) => {
                console.error(`❌ Erro no worker '${name}':`, error);
                this.workers.get(name).errors++;
                
                // Auto-restart se muitos erros
                if (this.workers.get(name).errors > 3) {
                    console.warn(`🔄 Reiniciando worker '${name}'...`);
                    this.terminate(name);
                    this._createWorker(name, path, options);
                }
            });
            
            console.log(`✅ Worker '${name}' criado`);
        } catch (error) {
            console.error(`❌ Falha ao criar worker '${name}':`, error);
            throw error;
        }
    }
    
    async _createWorkerPool(name, path, options = {}) {
        const poolSize = options.poolSize || 2;
        const pool = [];
        
        for (let i = 0; i < poolSize; i++) {
            const worker = new Worker(path, { type: 'module' });
            await this._waitForReady(worker);
            pool.push(worker);
        }
        
        this.workers.set(name, {
            pool,
            path,
            options,
            messageCount: 0
        });
        
        console.log(`✅ Worker pool '${name}' criado (${poolSize} workers)`);
    }
    
    _waitForReady(worker) {
        return new Promise((resolve) => {
            const handler = (event) => {
                if (event.data.type === 'ready') {
                    worker.removeEventListener('message', handler);
                    resolve();
                }
            };
            
            worker.addEventListener('message', handler);
        });
    }
    
    _getLeastBusyWorker(pool) {
        // Simples: round-robin
        // Pode ser melhorado com métricas reais de carga
        const index = this.workers.get('dataProcessing').messageCount % pool.length;
        this.workers.get('dataProcessing').messageCount++;
        return pool[index];
    }
    
    async _executeStep(workerName, action, data) {
        return new Promise((resolve, reject) => {
            const worker = this.workers.get(workerName);
            
            const handler = (event) => {
                if (event.data.action === action) {
                    worker.instance.removeEventListener('message', handler);
                    resolve(event.data.result);
                }
            };
            
            worker.instance.addEventListener('message', handler);
            worker.instance.postMessage({ action, data });
            
            // Timeout de 5s
            setTimeout(() => {
                reject(new Error(`Timeout executando ${workerName}.${action}`));
            }, 5000);
        });
    }
}
```

---

#### Worker Especializado: WebSocketWorker

**`workers/websocket/WebSocketWorker.js`:**
```javascript
/**
 * Web Worker: Comunicação WebSocket
 * 
 * Responsabilidades:
 * - Gerenciar conexão WebSocket
 * - Reconexão automática
 * - Parse de protocolo JSON
 * - Buffer de mensagens fragmentadas
 */

let socket = null;
let wsURL = '';
let reconnectAttempts = 0;
const MAX_RECONNECT_ATTEMPTS = 10;
const RECONNECT_INTERVAL = 3000;
let messageBuffer = '';

// Sinaliza que worker está pronto
self.postMessage({ type: 'ready' });

/**
 * Handler de mensagens do Orchestrator
 */
self.onmessage = (event) => {
    const { action, data } = event.data;
    
    switch (action) {
        case 'connect':
            connectWebSocket(data.url);
            break;
            
        case 'disconnect':
            disconnectWebSocket();
            break;
            
        case 'send':
            sendMessage(data);
            break;
            
        case 'set_url':
            wsURL = data.url;
            break;
            
        default:
            console.warn('[WebSocketWorker] Ação desconhecida:', action);
    }
};

function connectWebSocket(url) {
    wsURL = url || wsURL;
    
    // Detecta GitHub Pages
    if (self.location.hostname.includes('github.io')) {
        self.postMessage({
            type: 'status',
            status: 'offline',
            message: 'GitHub Pages - Modo offline'
        });
        return;
    }
    
    try {
        socket = new WebSocket(wsURL);
        setupEventHandlers();
    } catch (error) {
        self.postMessage({
            type: 'error',
            error: error.message
        });
    }
}

function setup####EventHandlers() {
    socket.onopen = () => {
        reconnectAttempts = 0;
        
        self.postMessage({
            type: 'connected',
            url: wsURL
        });
    };
    
    socket.onmessage = (event) => {
        // Acumula no buffer
        messageBuffer += event.data;
        
        // Parse de mensagens JSON completas
        const messages = parseJSONMessages(messageBuffer);
        
        messages.forEach(msg => {
            // Envia para o Orchestrator rotear para DataProcessingWorker
            self.postMessage({
                type: 'data',
                payload: msg
            });
        });
    };
    
    socket.onclose = (event) => {
        self.postMessage({
            type: 'disconnected',
            code: event.code,
            reason: event.reason
        });
        
        attemptReconnect();
    };
    
    socket.onerror = (error) => {
        self.postMessage({
            type: 'error',
            error: 'WebSocket error'
        });
    };
}

function sendMessage(data) {
    if (!socket || socket.readyState !== WebSocket.OPEN) {
        self.postMessage({
            type: 'error',
            error: 'WebSocket não conectado'
        });
        return;
    }
    
    const json = JSON.stringify(data);
    socket.send(json);
}

function disconnectWebSocket() {
    if (socket) {
        socket.close();
        socket = null;
    }
}

function attemptReconnect() {
    if (reconnectAttempts >= MAX_RECONNECT_ATTEMPTS) {
        self.postMessage({
            type: 'reconnect_failed',
            attempts: reconnectAttempts
        });
        return;
    }
    
    reconnectAttempts++;
    
    setTimeout(() => {
        connectWebSocket();
    }, RECONNECT_INTERVAL);
}

function parseJSONMessages(buffer) {
    const messages = [];
    let startIndex = 0;
    
    while (startIndex < buffer.length) {
        const char = buffer[startIndex];
        
        if (char !== '{' && char !== '[') {
            startIndex++;
            continue;
        }
        
        let braceCount = 0;
        let inString = false;
        let escaped = false;
        let endIndex = -1;
        
        for (let i = startIndex; i < buffer.length; i++) {
            const c = buffer[i];
            
            if (c === '"' && !escaped) {
                inString = !inString;
            }
            
            if (c === '\\' && inString) {
                escaped = !escaped;
            } else {
                escaped = false;
            }
            
            if (!inString) {
                if (c === '{' || c === '[') braceCount++;
                if (c === '}' || c === ']') braceCount--;
                
                if (braceCount === 0) {
                    endIndex = i;
                    break;
                }
            }
        }
        
        if (endIndex !== -1) {
            const jsonString = buffer.substring(startIndex, endIndex + 1);
            
            try {
                const parsed = JSON.parse(jsonString);
                messages.push(parsed);
                startIndex = endIndex + 1;
            } catch (error) {
                console.error('[WebSocketWorker] JSON inválido:', error);
                startIndex++;
            }
        } else {
            break;
        }
    }
    
    // Remove mensagens parseadas do buffer
    messageBuffer = buffer.substring(startIndex);
    
    return messages;
}
```

---

#### Worker Especializado: DataProcessingWorker

**`workers/processing/DataProcessingWorker.js`:**
```javascript
/**
 * Web Worker: Processamento de Dados
 * 
 * Responsabilidades:
 * - Parse de pacotes do protocolo
 * - Cálculo de EMA (Média Móvel Exponencial)
 * - Filtros (zona morta, arredondamento)
 * - Conversões de unidades
 * - Cálculo de estatísticas (RPS, max, min)
 */

// Estado do worker
let emaValue = 0;
let emaAlpha = 0.2;
let emaInitialized = false;
let maxForce = -Infinity;
let minForce = Infinity;
let gravity = 9.80665;

// Filtros
let filtroZonaMortaAtivo = true;
let arredondamentoAtivo = true;
let zonaMortaThreshold = 0.5;  // gramas
let casasDecimais = 2;

// Configuração da célula
let capacidadeMaximaGramas = 5000;
let percentualAcuracia = 0.05;

// Sinaliza pronto
self.postMessage({ type: 'ready' });

/**
 * Handler de mensagens
 */
self.onmessage = (event) => {
    const { action, data } = event.data;
    
    switch (action) {
        case 'process_data':
            const processed = processDataPoint(data);
            self.postMessage({
                type: 'processed_data',
                payload: processed
            });
            break;
            
        case 'set_config':
            updateConfig(data);
            break;
            
        case 'reset_stats':
            resetStatistics();
            break;
            
        default:
            console.warn('[DataProcessingWorker] Ação desconhecida:', action);
    }
};

/**
 * Processa um ponto de dados recebido
 */
function processDataPoint(rawData) {
    if (rawData.type !== 'data') {
        return null;  // Ignora outros tipos
    }
    
    let forcaN = rawData.forca;
    
    // 1. Aplica filtro de zona morta
    if (filtroZonaMortaAtivo) {
        const forcaGf = forcaN * 101.97162129779;
        if (Math.abs(forcaGf) < zonaMortaThreshold) {
            forcaN = 0;
        }
    }
    
    // 2. Calcula EMA
    const ema = calculateEMA(forcaN);
    
    // 3. Atualiza estatísticas
    if (forcaN > maxForce) maxForce = forcaN;
    if (forcaN < minForce) minForce = forcaN;
    
    // 4. Conversões de unidades
    const forcaGf = forcaN * 101.97162129779;
    const forcaKgf = forcaN / gravity;
    
    // 5. Aplica arredondamento
    let forcaFinal = forcaN;
    if (arredondamentoAtivo) {
        forcaFinal = parseFloat(forcaN.toFixed(casasDecimais));
    }
    
    // 6. Calcula % da capacidade
    const percentualCarga = (Math.abs(forcaGf) / capacidadeMaximaGramas) * 100;
    
    return {
        tempo: rawData.tempo,
        forca_n: forcaFinal,
        forca_gf: forcaGf,
        forca_kgf: forcaKgf,
        ema: ema,
        max_force: maxForce,
        min_force: minForce,
        percentual_carga: percentualCarga,
        timestamp: new Date().toISOString()
    };
}

/**
 * Calcula Média Móvel Exponencial
 */
function calculateEMA(newValue) {
    if (!emaInitialized) {
        emaValue = newValue;
        emaInitialized = true;
    } else {
        emaValue = (emaAlpha * newValue) + ((1 - emaAlpha) * emaValue);
    }
    
    return emaValue;
}

/**
 * Atualiza configurações
 */
function updateConfig(config) {
    if (config.gravity !== undefined) {
        gravity = config.gravity;
    }
    
    if (config.emaAlpha !== undefined) {
        emaAlpha = config.emaAlpha;
    }
    
    if (config.filtroZonaMorta !== undefined) {
        filtroZonaMortaAtivo = config.filtroZonaMorta;
    }
    
    if (config.zonaMortaThreshold !== undefined) {
        zonaMortaThreshold = config.zonaMortaThreshold;
    }
    
    if (config.arredondamento !== undefined) {
        arredondamentoAtivo = config.arredondamento;
        casasDecimais = config.casasDecimais || 2;
    }
    
    if (config.capacidadeMaximaGramas !== undefined) {
        capacidadeMaximaGramas = config.capacidadeMaximaGramas;
    }
    
    if (config.percentualAcuracia !== undefined) {
        percentualAcuracia = config.percentualAcuracia;
    }
    
    self.postMessage({
        type: 'config_updated',
        config: {
            gravity,
            emaAlpha,
            filtroZonaMortaAtivo,
            zonaMortaThreshold,
            arredondamentoAtivo,
            casasDecimais,
            capacidadeMaximaGramas,
            percentualAcuracia
        }
    });
}

/**
 * Reseta estatísticas
 */
function resetStatistics() {
    maxForce = -Infinity;
    minForce = Infinity;
    emaValue = 0;
    emaInitialized = false;
    
    self.postMessage({
        type: 'stats_reset'
    });
}
```

---

#### Worker Especializado: ChartWorker

**`workers/processing/ChartWorker.js`:**
```javascript
/**
 * Web Worker: Cálculos do Gráfico
 * 
 * Responsabilidades:
 * - Cálculos matemáticos pesados para o gráfico
 * - Interpolação de curvas
 * - Downsampling de dados
 * - Geração de estatísticas visuais
 */

self.postMessage({ type: 'ready' });

self.onmessage = (event) => {
    const { action, data } = event.data;
    
    switch (action) {
        case 'interpolate':
            const interpolated = interpolateCurve(data.points, data.method);
            self.postMessage({
                type: 'interpolated',
                payload: interpolated
            });
            break;
            
        case 'downsample':
            const downsampled = downsampleData(data.points, data.targetSize);
            self.postMessage({
                type: 'downsampled',
                payload: downsampled
            });
            break;
            
        case 'calculate_stats':
            const stats = calculateStats(data.points);
            self.postMessage({
                type: 'stats',
                payload: stats
            });
            break;
            
        default:
            console.warn('[ChartWorker] Ação desconhecida:', action);
    }
};

/**
 * Interpola curva
 */
function interpolateCurve(points, method = 'smooth') {
    if (method === 'smooth') {
        return cardinalSpline(points, 0.5, 20);
    } else {
        return points;  // Linear
    }
}

/**
 * Cardinal Spline Interpolation
 */
function cardinalSpline(points, tension, numOfSegments) {
    const interpolated = [];
    
    for (let i = 0; i < points.length - 1; i++) {
        const p0 = points[Math.max(0, i - 1)];
        const p1 = points[i];
        const p2 = points[i + 1];
        const p3 = points[Math.min(points.length - 1, i + 2)];
        
        for (let t = 0; t <= numOfSegments; t++) {
            const u = t / numOfSegments;
            const x = interpolate(p0[0], p1[0], p2[0], p3[0], u, tension);
            const y = interpolate(p0[1], p1[1], p2[1], p3[1], u, tension);
            interpolated.push([x, y]);
        }
    }
    
    return interpolated;
}

function interpolate(p0, p1, p2, p3, t, tension) {
    const v0 = (p2 - p0) * tension;
    const v1 = (p3 - p1) * tension;
    const t2 = t * t;
    const t3 = t * t2;
    
    return (2 * p1 - 2 * p2 + v0 + v1) * t3 +
           (-3 * p1 + 3 * p2 - 2 * v0 - v1) * t2 +
           v0 * t +
           p1;
}

/**
 * Downsampling - Largest Triangle Three Buckets (LTTB)
 */
function downsampleData(data, threshold) {
    if (data.length <= threshold) {
        return data;
    }
    
    const sampled = [];
    const bucketSize = (data.length - 2) / (threshold - 2);
    
    sampled[0] = data[0];  // Primeiro ponto
    
    let a = 0;
    
    for (let i = 0; i < threshold - 2; i++) {
        const avgRangeStart = Math.floor((i + 1) * bucketSize) + 1;
        const avgRangeEnd = Math.floor((i + 2) * bucketSize) + 1;
        
        const avgRangeLength = avgRangeEnd - avgRangeStart;
        
        let avgX = 0;
        let avgY = 0;
        
        for (let j = avgRangeStart; j < avgRangeEnd; j++) {
            avgX += data[j][0];
            avgY += data[j][1];
        }
        
        avgX /= avgRangeLength;
        avgY /= avgRangeLength;
        
        const rangeStart = Math.floor(i * bucketSize) + 1;
        const rangeEnd = Math.floor((i + 1) * bucketSize) + 1;
        
        const pointA = data[a];
        
        let maxArea = -1;
        let maxAreaPoint = null;
        
        for (let j = rangeStart; j < rangeEnd; j++) {
            const area = Math.abs(
                (pointA[0] - avgX) * (data[j][1] - pointA[1]) -
                (pointA[0] - data[j][0]) * (avgY - pointA[1])
            ) * 0.5;
            
            if (area > maxArea) {
                maxArea = area;
                maxAreaPoint = data[j];
                a = j;
            }
        }
        
        sampled.push(maxAreaPoint);
    }
    
    sampled.push(data[data.length - 1]);  // Último ponto
    
    return sampled;
}

/**
 * Calcula estatísticas
 */
function calculateStats(points) {
    if (points.length === 0) {
        return {
            count: 0,
            min: 0,
            max: 0,
            mean: 0,
            median: 0,
            stdDev: 0
        };
    }
    
    const values = points.map(p => p[1]);
    
    const min = Math.min(...values);
    const max = Math.max(...values);
    const sum = values.reduce((a, b) => a + b, 0);
    const mean = sum / values.length;
    
    const sorted = [...values].sort((a, b) => a - b);
    const median = sorted[Math.floor(sorted.length / 2)];
    
    const variance = values.reduce((acc, val) => {
        return acc + Math.pow(val - mean, 2);
    }, 0) / values.length;
    
    const stdDev = Math.sqrt(variance);
    
    return {
        count: points.length,
        min,
        max,
        mean,
        median,
        stdDev
    };
}
```

---

#### Uso do Orchestrator na Main Thread

**`services/WorkerService.js`:**
```javascript
/**
 * Service: Gerenciamento de Workers
 * 
 * Encapsula o WorkerOrchestrator e fornece API simplificada
 */
import { WorkerOrchestrator } from '../workers/WorkerOrchestrator.js';
import { EventBus } from '../core/EventBus.js';

export class WorkerService {
    constructor() {
        this.orchestrator = new WorkerOrchestrator();
        this.eventBus = EventBus.getInstance();
        this.isInitialized = false;
    }
    
    async init() {
        if (this.isInitialized) {
            return;
        }
        
        await this.orchestrator.init();
        this._setupEventRouting();
        this.isInitialized = true;
    }
    
    /**
     * Conecta ao WebSocket
     */
    connectWebSocket(url) {
        this.orchestrator.postMessage('websocket', {
            action: 'connect',
            data: { url }
        });
    }
    
    /**
     * Envia comando para o servidor
     */
    sendCommand(command) {
        this.orchestrator.postMessage('websocket', {
            action: 'send',
            data: command
        });
    }
    
    /**
     * Processa dados recebidos
     */
    processData(rawData) {
        this.orchestrator.postMessage('dataProcessing', {
            action: 'process_data',
            data: rawData
        });
    }
    
    /**
     * Atualiza configuração de processamento
     */
    updateConfig(config) {
        this.orchestrator.postMessage('dataProcessing', {
            action: 'set_config',
            data: config
        });
    }
    
    /**
     * Interpola curva do gráfico
     */
    async interpolateCurve(points, method) {
        return new Promise((resolve) => {
            this.orchestrator.on('chart', 'interpolated', (data) => {
                resolve(data);
            });
            
            this.orchestrator.postMessage('chart', {
                action: 'interpolate',
                data: { points, method }
            });
        });
    }
    
    /**
     * Exporta sessão para PDF
     */
    async exportToPDF(session) {
        return new Promise((resolve) => {
            this.orchestrator.on('export', 'pdf_ready', (data) => {
                resolve(data);
            });
            
            this.orchestrator.postMessage('export', {
                action: 'generate_pdf',
                data: session
            });
        });
    }
    
    /**
     * Termina todos os workers
     */
    shutdown() {
        this.orchestrator.terminateAll();
        this.isInitialized = false;
    }
    
    // =========== Privado ===========
    
    _setupEventRouting() {
        // WebSocket → EventBus
        this.orchestrator.on('websocket', 'connected', () => {
            this.eventBus.emit('ws:connected');
        });
        
        this.orchestrator.on('websocket', 'disconnected', () => {
            this.eventBus.emit('ws:disconnected');
        });
        
        this.orchestrator.on('websocket', 'data', (payload) => {
            // Redireciona para DataProcessingWorker
            this.processData(payload);
        });
        
        // DataProcessing → EventBus
        this.orchestrator.on('dataProcessing', 'processed_data', (payload) => {
            this.eventBus.emit('data:processed', payload);
        });
        
        this.orchestrator.on('dataProcessing', 'config_updated', (config) => {
            this.eventBus.emit('config:updated', config);
        });
    }
}
```

---

#### Benefícios da Arquitetura Multi-Worker

| Aspecto | Antes (Worker Único) | Depois (Orquestrador) |
|---------|---------------------|----------------------|
| **Responsabilidades** | Tudo em 1 worker | 5 workers especializados |
| **Escalabilidade** | ❌ Limitada | ✅ Pool de workers |
| **Performance** | ⚠️ Thread bloqueada | ✅ Processamento paralelo |
| **Manutenibilidade** | ❌ Difícil | ✅ Módulos isolados |
| **Testabilidade** | ❌ Monolítico | ✅ Testável unitariamente |
| **Resiliência** | ❌ Falha = tudo para | ✅ Restart individual |
| **Load Balancing** | ❌ Não existe | ✅ Automático |
| **Isolamento** | ❌ Estado compartilhado | ✅ Estado isolado |

**Ganhos de Performance Esperados:**
- 📈 **+40%** throughput de leituras (pool de processamento)
- 📉 **-60%** latência no gráfico (worker dedicado)
- 🚀 **+200%** velocidade de export PDF (worker sob demanda)
- ⚡ **-80%** bloqueios na UI (processamento paralelo)

---

#### `main.js` (Entry Point)
```javascript
/**
 * Entry Point da aplicação
 */
import { DependencyContainer } from './core/DependencyContainer.js';
import { EventBus } from './core/EventBus.js';
import { WebSocketService } from './services/WebSocketService.js';
import { APIService } from './services/APIService.js';
import { StorageService } from './services/StorageService.js';
import { SessaoRepository } from './repositories/SessaoRepository.js';
import { GraficoController } from './controllers/GraficoController.js';
import { SessaoController } from './controllers/SessaoController.js';
import { ConfigController } from './controllers/ConfigController.js';
import { GraficoView } from './views/components/GraficoView.js';
import { StatusBar } from './views/components/StatusBar.js';

class Application {
    constructor() {
        this.container = new DependencyContainer();
        this.eventBus = EventBus.getInstance();
    }
    
    async init() {
        // 1. Register Services
        this.container.register('websocketService', () => {
            const url = StorageService.get('wsUrl') || this._buildDefaultWsUrl();
            return new WebSocketService(url);
        });
        
        this.container.register('apiService', () => {
            return new APIService('/api');
        });
        
        this.container.register('storageService', () => {
            return new StorageService();
        });
        
        // 2. Register Repositories
        this.container.register('sessaoRepository', () => {
            return new SessaoRepository(
                this.container.resolve('apiService')
            );
        });
        
        // 3. Register Views
        this.container.register('graficoView', () => {
            return new GraficoView('grafico', chartOptions);
        });
        
        this.container.register('statusBar', () => {
            return new StatusBar('footer-atalhos');
        });
        
        // 4. Register Controllers
        this.container.register('graficoController', () => {
            return new GraficoController(
                this.container.resolve('graficoView')
            );
        });
        
        this.container.register('sessaoController', () => {
            return new SessaoController(
                this.container.resolve('sessaoRepository')
            );
        });
        
        this.container.register('configController', () => {
            return new ConfigController(
                this.container.resolve('websocketService'),
                this.container.resolve('storageService')
            );
        });
        
        // 5. Initialize
        this._setupGlobalEventListeners();
        this._connectWebSocket();
        this._initializeControllers();
    }
    
    _setupGlobalEventListeners() {
        // Keyboard shortcuts
        document.addEventListener('keydown', (e) => {
            if (e.shiftKey && e.key === 'T') {
                this.eventBus.emit('command:tara');
            }
            // ... outros atalhos
        });
    }
    
    _connectWebSocket() {
        const wsService = this.container.resolve('websocketService');
        wsService.connect();
    }
    
    _initializeControllers() {
        // Controllers já se auto-registram via EventBus
        this.container.resolve('graficoController');
        this.container.resolve('sessaoController');
        this.container.resolve('configController');
    }
    
    _buildDefaultWsUrl() {
        const protocol = location.protocol === 'https:' ? 'wss' : 'ws';
        const host = location.hostname || 'localhost';
        return `${protocol}://${host}:81`;
    }
}

// Bootstrap
document.addEventListener('DOMContentLoaded', () => {
    const app = new Application();
    app.init();
});
```

---

## 🔧 ESP32 (Firmware)

### Nova Estrutura

```
src/
├── main.cpp                    # Entry point
│
├── core/
│   ├── Application.h/cpp       # Main application logic
│   └── TaskScheduler.h/cpp     # Non-blocking task scheduler
│
├── hardware/
│   ├── HX711Driver.h/cpp       # HX711 interface
│   ├── DisplayDriver.h/cpp     # OLED display
│   └── EEPROMManager.h/cpp     # EEPROM storage
│
├── domain/
│   ├── models/
│   │   ├── Reading.h           # Leitura struct
│   │   └── Config.h            # Configuração struct
│   │
│   └── services/
│       ├── CalibrationService.h/cpp
│       ├── FilterService.h/cpp
│       └── StabilityService.h/cpp
│
├── communication/
│   ├── protocol/
│   │   ├── BinaryProtocol.h/cpp
│   │   ├── PacketBuilder.h/cpp
│   │   ├── PacketParser.h/cpp
│   │   └── CRC16.h/cpp
│   │
│   └── SerialHandler.h/cpp
│
└── utils/
    ├── Math.h/cpp
    └── Filters.h/cpp
```

### Exemplo de Implementação

#### `domain/models/Reading.h`
```cpp
/**
 * Reading Value Object
 */
#ifndef READING_H
#define READING_H

#include <Arduino.h>

struct Reading {
    uint32_t timestamp_ms;
    float force_n;
    uint8_t status;
    
    Reading() : timestamp_ms(0), force_n(0.0f), status(0) {}
    
    Reading(uint32_t ts, float force, uint8_t stat) 
        : timestamp_ms(ts), force_n(force), status(stat) {}
    
    bool isValid() const {
        return !isnan(force_n) && !isinf(force_n);
    }
    
    float getForceGf(float gravity = 9.80665f) const {
        return force_n * 101.97162129779f;
    }
    
    float getForceKgf(float gravity = 9.80665f) const {
        return force_n / gravity;
    }
};

#endif
```

#### `domain/services/CalibrationService.h`
```cpp
/**
 * Calibration Service
 */
#ifndef CALIBRATION_SERVICE_H
#define CALIBRATION_SERVICE_H

#include "hardware/HX711Driver.h"
#include "domain/models/Config.h"

class CalibrationService {
public:
    CalibrationService(HX711Driver& hx711, EEPROMManager& eeprom);
    
    bool calibrate(float known_mass_g, uint16_t timeout_ms);
    bool tare();
    
    float getConversionFactor() const;
    int32_t getTareOffset() const;
    
private:
    HX711Driver& _hx711;
    EEPROMManager& _eeprom;
    
    bool waitForStability(uint16_t timeout_ms);
    float calculateConversionFactor(int32_t raw_value, float known_mass_g);
};

#endif
```

#### `communication/protocol/BinaryProtocol.h`
```cpp
/**
 * Binary Protocol Handler
 */
#ifndef BINARY_PROTOCOL_H
#define BINARY_PROTOCOL_H

#include <Arduino.h>
#include "domain/models/Reading.h"
#include "domain/models/Config.h"
#include "PacketBuilder.h"
#include "PacketParser.h"

class BinaryProtocol {
public:
    BinaryProtocol();
    
    // Building packets (ESP → Host)
    size_t buildDataPacket(const Reading& reading, uint8_t* buffer);
    size_t buildConfigPacket(const Config& config, uint8_t* buffer);
    size_t buildStatusPacket(uint8_t statusCode, uint8_t msgCode, uint8_t* buffer);
    
    // Parsing commands (Host → ESP)
    bool parseCommand(const uint8_t* buffer, size_t length, Command& cmd);
    
private:
    PacketBuilder _builder;
    PacketParser _parser;
};

#endif
```

#### `main.cpp` (Refatorado)
```cpp
/**
 * Main Entry Point
 */
#include <Arduino.h>
#include "core/Application.h"

Application* app = nullptr;

void setup() {
    Serial.begin(921600);
    
    // Dependency Injection
    auto hx711 = new HX711Driver(DOUT_PIN, SCK_PIN);
    auto display = new DisplayDriver(SCREEN_WIDTH, SCREEN_HEIGHT);
    auto eeprom = new EEPROMManager();
    auto protocol = new BinaryProtocol();
    auto serialHandler = new SerialHandler(Serial, *protocol);
    
    // Services
    auto calibService = new CalibrationService(*hx711, *eeprom);
    auto filterService = new FilterService();
    auto stabilityService = new StabilityService();
    
    // Application
    app = new Application(
        hx711,
        display,
        eeprom,
        serialHandler,
        calibService,
        filterService,
        stabilityService
    );
    
    app->init();
}

void loop() {
    app->run();
}
```

---

## 📋 Plano de Migração

### Fase 1: Preparação (Semana 1)
1. ✅ Setup de ambiente de desenvolvimento
2. ✅ Criar estrutura de diretórios
3. ✅ Configurar testes unitários
4. ✅ Documentar APIs atuais

### Fase 2: Backend - Infrastructure Layer (Semana 2-3)
1. Implementar `infrastructure/protocol/`
   - BinaryProtocol
   - CRC16
   - Packet types
2. Implementar `infrastructure/database/`
   - Connection pool
   - Base repository
3. Implementar `infrastructure/serial/`
   - Serial adapter
   - Auto-detect

### Fase 3: Backend - Domain Layer (Semana 4)
1. Criar models (Leitura, Sessao, Configuracao)
2. Definir interfaces de repositories
3. Implementar domain services

### Fase 4: Backend - Application Layer (Semana 5)
1. Implementar use cases
2. Criar DTOs
3. Testes unitários

### Fase 5: Backend - Presentation Layer (Semana 6)
1. Implementar HTTP controllers
2. Criar routes
3. Integrar com use cases

### Fase 6: Frontend - Core & Models (Semana 7)
1. EventBus
2. Router
3. Models

### Fase 7: Frontend - Services (Semana 8)
1. WebSocketService
2. APIService
3. StorageService

### Fase 8: Frontend - Views & Controllers (Semana 9-10)
1. Componentes de UI
2. Controllers
3. Integração

### Fase 9: ESP32 Refactor (Semana 11)
1. Separar protocolo
2. Criar drivers
3. Services

### Fase 10: Testes & Deploy (Semana 12)
1. Testes end-to-end
2. Performance testing
3. Deploy gradual

---

## ✨ Benefícios Esperados

### Manutenibilidade
- ✅ Código modular e testável
- ✅ Responsabilidades claras
- ✅ Fácil de entender e modificar
- ✅ Redução de bugs

### Escalabilidade
- ✅ Fácil adicionar novos tipos de pacotes
- ✅ Fácil adicionar novos endpoints
- ✅ Suporte a múltiplos bancos de dados
- ✅ Suporte a múltiplos protocolos

### Testabilidade
- ✅ Testes unitários isolados
- ✅ Mocks facilitados (DI)
- ✅ Testes de integração
- ✅ Cobertura de código >80%

### Performance
- ✅ Separation of concerns = otimização focada
- ✅ Lazy loading de componentes
- ✅ Connection pooling
- ✅ Batch operations

### Developer Experience
- ✅ Código autodocumentado
- ✅ IntelliSense melhorado
- ✅ Debugging facilitado
- ✅ Onboarding mais rápido

---

**Versão:** 1.0  
**Data:** 31/10/2025  
**Autor:** Sistema de Balança GFIG
