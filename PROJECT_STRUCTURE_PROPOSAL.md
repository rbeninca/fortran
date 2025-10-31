# 📁 Proposta de Organização da Estrutura do Projeto

## 🎯 Objetivo

Reorganizar a estrutura de arquivos do projeto **Balança Digital Web** para:
- ✅ Melhorar manutenibilidade
- ✅ Facilitar navegação e entendimento
- ✅ Separar responsabilidades (ESP, Servidor, Web, Docker)
- ✅ Seguir convenções de projetos modernos
- ✅ Facilitar versionamento e colaboração

---

## 📊 Estrutura Atual vs Proposta

### ❌ Estrutura Atual (Problemas)

```
balanca_nodemcu/
├── Dockerfile                    # ⚠️ Config Docker na raiz
├── docker-compose.yml           # ⚠️ Config Docker na raiz
├── server.py                    # ⚠️ Servidor Python na raiz
├── entrypoint.sh               # ⚠️ Scripts Docker na raiz
├── init.sql                    # ⚠️ Scripts SQL na raiz
├── platformio.ini              # ⚠️ Config ESP na raiz
├── requirements.txt            # ⚠️ Deps Python na raiz
├── setup_wifi.sh               # ⚠️ Scripts rede na raiz
├── hotspot.sh                  # ⚠️ Scripts rede na raiz
├── *.md                        # ⚠️ Muitos docs na raiz
├── src/                        # ✅ Código ESP (OK)
│   ├── main.cpp
│   ├── HX711.cpp
│   └── HX711.h
├── data/                       # ⚠️ Frontend misturado
│   ├── index.html
│   ├── script.js
│   ├── dataWorker.js
│   └── estilo.css
├── docs/                       # ⚠️ Duplicado de data/
└── include/                    # ✅ Headers ESP (OK)
```

**Problemas:**
- 🔴 Raiz poluída com 15+ arquivos de diferentes responsabilidades
- 🔴 `data/` e `docs/` duplicados (confuso)
- 🔴 Arquivos Docker, Python, Shell, Markdown misturados
- 🔴 Difícil encontrar o que precisa
- 🔴 Scripts de rede sem organização

---

## ✅ Estrutura Proposta (Organizada)

```
balanca-digital-web/
│
├── 📄 README.md                          # Documentação principal
├── 📄 LICENSE                            # Licença do projeto
├── 📄 .gitignore                         # Ignorar arquivos
│
├── 📁 docs/                              # 📚 TODA A DOCUMENTAÇÃO
│   ├── README.md                         # Índice da documentação
│   ├── ARCHITECTURE.md                   # Arquitetura do sistema
│   ├── REFACTORING_PROPOSAL.md          # Proposta de refatoração
│   ├── PROJECT_STRUCTURE_PROPOSAL.md    # Este documento
│   ├── API.md                           # Endpoints e protocolos
│   ├── DEPLOYMENT.md                    # Como fazer deploy
│   ├── DEVELOPMENT.md                   # Setup dev local
│   ├── ETHERNET_DHCP_CONFIG.md          # Config de rede
│   ├── IPV6_IMPLEMENTATION_SUMMARY.md   # Resumo IPv6
│   ├── MYSQL-HEALTHCHECK-README.md      # MySQL health
│   ├── PERFORMANCE_OPTIMIZATION.md      # Otimizações
│   └── REPOSITORY_ANALYSIS.md           # Análise repositório
│
├── 📁 firmware/                          # 🔧 FIRMWARE ESP32
│   ├── platformio.ini                   # Config PlatformIO
│   ├── src/                             # Código fonte ESP
│   │   ├── main.cpp                     # Programa principal
│   │   ├── HX711.cpp                    # Driver célula carga
│   │   └── HX711.h
│   ├── include/                         # Headers
│   │   └── README
│   ├── lib/                             # Bibliotecas externas
│   │   └── README
│   └── test/                            # Testes unitários
│       └── README
│
├── 📁 server/                            # 🐍 SERVIDOR PYTHON
│   ├── server.py                        # Servidor principal
│   ├── requirements.txt                 # Dependências Python
│   ├── config/                          # Configurações
│   │   └── settings.py                  # Settings do servidor
│   ├── routes/                          # Rotas API (proposta)
│   │   ├── api_routes.py
│   │   └── websocket_routes.py
│   ├── services/                        # Lógica de negócio
│   │   ├── serial_service.py
│   │   ├── database_service.py
│   │   └── session_service.py
│   └── utils/                           # Utilitários
│       └── helpers.py
│
├── 📁 web/                               # 🌐 FRONTEND WEB
│   ├── public/                          # Arquivos estáticos
│   │   ├── index.html                   # Página principal
│   │   ├── rede.html                    # Config de rede
│   │   └── assets/                      # Recursos estáticos
│   │       ├── css/
│   │       │   └── estilo.css
│   │       ├── js/
│   │       │   ├── app.js               # Inicialização
│   │       │   ├── models/              # Camada Model
│   │       │   │   ├── DataModel.js
│   │       │   │   ├── ConfigModel.js
│   │       │   │   └── SessionModel.js
│   │       │   ├── views/               # Camada View
│   │       │   │   ├── ChartView.js
│   │       │   │   ├── TableView.js
│   │       │   │   ├── ControlsView.js
│   │       │   │   └── NotificationView.js
│   │       │   ├── controllers/         # Camada Controller
│   │       │   │   ├── ChartController.js
│   │       │   │   ├── SessionController.js
│   │       │   │   └── ConfigController.js
│   │       │   ├── services/            # Serviços
│   │       │   │   ├── WebSocketService.js
│   │       │   │   ├── ApiService.js
│   │       │   │   └── StorageService.js
│   │       │   ├── workers/             # Web Workers
│   │       │   │   └── dataWorker.js
│   │       │   └── utils/               # Utilitários
│   │       │       ├── constants.js
│   │       │       ├── formatters.js
│   │       │       └── validators.js
│   │       └── libs/                    # Bibliotecas externas
│   │           ├── apexcharts.js
│   │           ├── chartist.min.js
│   │           └── chartist.min.css
│   └── README.md                        # Doc do frontend
│
├── 📁 docker/                            # 🐳 DOCKER CONFIG
│   ├── Dockerfile                       # Imagem principal
│   ├── docker-compose.yml              # Orquestração
│   ├── entrypoint.sh                   # Script inicialização
│   ├── .dockerignore                   # Ignorar no build
│   └── mysql/                          # Config MySQL
│       ├── init.sql                    # Schema inicial
│       └── mysql-healthcheck.sh        # Healthcheck
│
├── 📁 scripts/                           # 📜 SCRIPTS AUXILIARES
│   ├── network/                         # Scripts de rede
│   │   ├── setup_wifi.sh               # Config WiFi/AP
│   │   ├── hotspot.sh                  # Criar hotspot
│   │   ├── configure_ethernet_dhcp.sh  # Config ethernet
│   │   └── README.md
│   ├── monitoring/                      # Scripts monitoramento
│   │   ├── install-monitoring.sh
│   │   └── check-mysql.sh
│   ├── deployment/                      # Scripts deploy
│   │   └── deploy.sh
│   └── development/                     # Scripts dev
│       ├── debug_serial.py
│       ├── testProtocol.py
│       └── testSerial.py
│
├── 📁 database/                          # 🗄️ BANCO DE DADOS
│   ├── migrations/                      # Migrações
│   │   └── 001_initial_schema.sql
│   ├── seeds/                           # Dados iniciais
│   │   └── test_data.sql
│   └── backups/                         # Backups
│       └── .gitkeep
│
├── 📁 tests/                             # 🧪 TESTES
│   ├── unit/                            # Testes unitários
│   │   ├── test_server.py
│   │   └── test_services.py
│   ├── integration/                     # Testes integração
│   │   └── test_api.py
│   └── e2e/                             # Testes end-to-end
│       └── test_workflow.py
│
├── 📁 .github/                           # ⚙️ GITHUB CONFIG
│   ├── workflows/                       # CI/CD
│   │   ├── docker-build.yml
│   │   └── tests.yml
│   └── ISSUE_TEMPLATE/
│
└── 📁 .vscode/                           # 🔧 VSCODE CONFIG
    ├── settings.json
    ├── launch.json
    └── extensions.json
```

---

## 🔀 Mapeamento de Migração

### Arquivos da Raiz

| Arquivo Atual | Novo Local | Motivo |
|---------------|------------|--------|
| `Dockerfile` | `docker/Dockerfile` | Centralizar Docker |
| `docker-compose.yml` | `docker/docker-compose.yml` | Centralizar Docker |
| `entrypoint.sh` | `docker/entrypoint.sh` | Script Docker |
| `init.sql` | `docker/mysql/init.sql` | Config MySQL |
| `mysql-healthcheck.sh` | `docker/mysql/mysql-healthcheck.sh` | Script MySQL |
| `server.py` | `server/server.py` | Centralizar servidor |
| `requirements.txt` | `server/requirements.txt` | Deps Python |
| `platformio.ini` | `firmware/platformio.ini` | Config firmware |
| `setup_wifi.sh` | `scripts/network/setup_wifi.sh` | Script rede |
| `hotspot.sh` | `scripts/network/hotspot.sh` | Script rede |
| `configure_ethernet_dhcp.sh` | `scripts/network/configure_ethernet_dhcp.sh` | Script rede |
| `install-monitoring.sh` | `scripts/monitoring/install-monitoring.sh` | Script monitor |
| `check-mysql.sh` | `scripts/monitoring/check-mysql.sh` | Script monitor |
| `debug_serial.py` | `scripts/development/debug_serial.py` | Script dev |
| `testProtocol.py` | `scripts/development/testProtocol.py` | Script dev |
| `testSerial.py` | `scripts/development/testSerial.py` | Script dev |
| `*.md` | `docs/*.md` | Documentação |

### Código ESP32

| Arquivo Atual | Novo Local |
|---------------|------------|
| `src/main.cpp` | `firmware/src/main.cpp` |
| `src/HX711.cpp` | `firmware/src/HX711.cpp` |
| `src/HX711.h` | `firmware/src/HX711.h` |
| `include/*` | `firmware/include/*` |
| `lib/*` | `firmware/lib/*` |
| `test/*` | `firmware/test/*` |

### Frontend Web

| Arquivo Atual | Novo Local | Refatoração |
|---------------|------------|-------------|
| `data/index.html` | `web/public/index.html` | Limpar duplicação |
| `data/rede.html` | `web/public/rede.html` | - |
| `data/script.js` | `web/public/assets/js/` | **Dividir em módulos MVC** |
| `data/dataWorker.js` | `web/public/assets/js/workers/dataWorker.js` | - |
| `data/script_grafico_sessao.js` | `web/public/assets/js/views/ChartView.js` | Refatorar |
| `data/funcoespdf.js` | `web/public/assets/js/utils/pdfExport.js` | Refatorar |
| `data/estilo.css` | `web/public/assets/css/estilo.css` | - |
| `data/apexcharts` | `web/public/assets/libs/apexcharts.js` | - |
| `data/chartist.min.*` | `web/public/assets/libs/chartist.*` | - |
| `docs/*` | **REMOVER** | Duplicação de `data/` |

---

## 📝 Vantagens da Nova Estrutura

### 🎯 Separação de Responsabilidades

```
firmware/     → ESP32 (C++, PlatformIO)
server/       → Backend (Python, WebSocket, API)
web/          → Frontend (HTML, CSS, JS)
docker/       → Containerização
scripts/      → Automação
docs/         → Documentação
```

### 🔍 Fácil Navegação

**Antes:**
- ❓ "Onde está o servidor?" → Raiz poluída
- ❓ "Qual o script de rede?" → Vários na raiz
- ❓ "Documentação?" → Misturada com código

**Depois:**
- ✅ "Servidor?" → `server/`
- ✅ "Script de rede?" → `scripts/network/`
- ✅ "Documentação?" → `docs/`

### 🚀 Deploy Facilitado

```bash
# Build apenas do firmware
cd firmware && pio run

# Build apenas do servidor
cd server && docker build .

# Build apenas do frontend
cd web && npm run build  # (futuro)
```

### 🧪 Testes Organizados

```
tests/
├── unit/           # Testes rápidos
├── integration/    # Testes médios
└── e2e/           # Testes completos
```

---

## 🔄 Plano de Migração

### Fase 1: Preparação (Sem Breaking Changes)

```bash
# 1. Criar nova estrutura de diretórios
mkdir -p firmware/{src,include,lib,test}
mkdir -p server/{config,routes,services,utils}
mkdir -p web/public/assets/{css,js,libs}
mkdir -p web/public/assets/js/{models,views,controllers,services,workers,utils}
mkdir -p docker/mysql
mkdir -p scripts/{network,monitoring,deployment,development}
mkdir -p docs
mkdir -p database/{migrations,seeds,backups}
mkdir -p tests/{unit,integration,e2e}

# 2. Copiar arquivos (manter originais)
# ... (comandos de cópia)
```

### Fase 2: Atualização de Referências

**`platformio.ini`** → Ajustar paths:
```ini
[env:nodemcu]
data_dir = ../web/public  # Novo path
```

**`docker-compose.yml`** → Ajustar volumes:
```yaml
services:
  balanca:
    build: ./docker
    volumes:
      - ./server:/app
      - ./web/public:/app/static
```

**`server.py`** → Ajustar paths:
```python
STATIC_DIR = Path(__file__).parent.parent / 'web' / 'public'
```

### Fase 3: Refatoração do Frontend

**Dividir `script.js` (3000+ linhas) em módulos:**

```javascript
// app.js (Entry Point)
import { ChartController } from './controllers/ChartController.js';
import { SessionController } from './controllers/SessionController.js';
import { WebSocketService } from './services/WebSocketService.js';

// Inicialização modular
const app = new Application();
app.init();
```

### Fase 4: Validação

```bash
# Testar build firmware
cd firmware && pio run

# Testar servidor
cd server && python server.py

# Testar Docker
cd docker && docker-compose up
```

### Fase 5: Remover Arquivos Antigos

```bash
# Após validação completa, remover da raiz
rm Dockerfile docker-compose.yml server.py
rm setup_wifi.sh hotspot.sh *.md
# ... etc
```

---

## 📦 Configurações Necessárias

### 1. `.gitignore` Atualizado

```gitignore
# Firmware
firmware/.pio/
firmware/.vscode/

# Server
server/__pycache__/
server/*.pyc
server/.env

# Web
web/node_modules/
web/dist/

# Docker
docker/volumes/
docker/.env

# Database
database/backups/*.sql
!database/backups/.gitkeep

# IDE
.vscode/
.idea/
*.swp

# OS
.DS_Store
Thumbs.db
```

### 2. `docker-compose.yml` Atualizado

```yaml
version: '3.8'

services:
  balanca:
    build:
      context: ../
      dockerfile: docker/Dockerfile
    volumes:
      - ../server:/app
      - ../web/public:/app/static
    environment:
      - STATIC_DIR=/app/static
    networks:
      - balanca_network
    
  balanca_mysql:
    image: mariadb:11
    volumes:
      - ../docker/mysql/init.sql:/docker-entrypoint-initdb.d/init.sql
      - mysql_data:/var/lib/mysql
```

### 3. `platformio.ini` Atualizado

```ini
[platformio]
src_dir = src
include_dir = include
lib_dir = lib
test_dir = test
data_dir = ../web/public  # ← Atualizado

[env:nodemcu]
platform = espressif8266
board = nodemcuv2
framework = arduino
```

---

## 🎓 Convenções e Boas Práticas

### Nomenclatura de Arquivos

```
✅ USAR:
snake_case.py       # Python
camelCase.js        # JavaScript
PascalCase.js       # Classes JS
kebab-case.css      # CSS
UPPER_CASE.md       # Docs importantes

❌ EVITAR:
MisturadoCamelCase_snake.py
arquivo sem espacos.js
```

### Estrutura de Módulos JS

```javascript
// Cada arquivo JS deve:
// 1. Importar dependências
import { dependency } from './dependency.js';

// 2. Definir classe/função
export class ModuleName {
    constructor() {
        // ...
    }
}

// 3. Exportar
export default ModuleName;
```

### README em Cada Pasta

Cada pasta principal deve ter um `README.md`:

```
firmware/README.md       # Como compilar firmware
server/README.md         # Como rodar servidor
web/README.md           # Como buildar frontend
scripts/README.md       # Como usar scripts
```

---

## 🚀 Benefícios da Reorganização

### Para Desenvolvimento

- ✅ **Onboarding mais rápido**: Estrutura clara
- ✅ **Menos conflitos Git**: Arquivos separados
- ✅ **Testes isolados**: Cada camada testável
- ✅ **Hot reload**: Só recarrega o que mudou

### Para Manutenção

- ✅ **Bug tracking**: Fácil localizar código
- ✅ **Code review**: Mudanças bem delimitadas
- ✅ **Refactoring**: Pode refatorar por camadas
- ✅ **Versionamento**: Histórico limpo

### Para Deployment

- ✅ **Build otimizado**: Só compila o necessário
- ✅ **Cache Docker**: Layers bem definidas
- ✅ **CI/CD**: Pipelines por componente
- ✅ **Rollback**: Pode reverter só uma parte

---

## 📊 Comparação Final

| Aspecto | Antes | Depois |
|---------|-------|--------|
| **Arquivos na raiz** | 15+ | 3-5 |
| **Navegação** | Confusa | Clara |
| **Duplicação** | `data/` = `docs/` | Eliminada |
| **Responsabilidades** | Misturadas | Separadas |
| **Testabilidade** | Difícil | Modular |
| **Onboarding** | 2-3 dias | 2-3 horas |
| **Build time** | Build tudo | Build seletivo |
| **Manutenibilidade** | ⭐⭐ | ⭐⭐⭐⭐⭐ |

---

## 📖 Estratégia GitHub Pages

### Problema: Duplicação `data/` vs `docs/`

Atualmente, o código web está **duplicado**:
- `data/` - Usado pelo ESP32/PlatformIO
- `docs/` - Servido pelo GitHub Pages (cópia idêntica)

### Solução: Build Automatizado

Manter **apenas `data/`** como fonte única:

```
balanca-web/
├── data/                     # ← Fonte única (edite aqui)
│   ├── index.html
│   ├── script.js
│   └── ...
├── docs/                     # ← Gerado automaticamente
│   └── (cópia de data/)
└── scripts/
    └── sync-docs.sh          # Script de sincronização
```

### Automação

**GitHub Actions** (`.github/workflows/sync-docs.yml`):
- Detecta mudanças em `data/`
- Sincroniza para `docs/` automaticamente
- Faz commit e push

**Hook Pre-commit** (`.git/hooks/pre-commit`):
- Sincroniza localmente antes do commit
- Garante que `docs/` está sempre atualizado

### Uso

```bash
# Edite apenas em data/
vim data/script.js

# Commit (hook sincroniza automaticamente)
git add data/script.js
git commit -m "feat: novo recurso"

# Ou sincronize manualmente
./scripts/sync-docs.sh
```

**Detalhes:** Veja `GITHUB_PAGES_STRATEGY.md`

---

## 🎯 Próximos Passos

1. ✅ **Revisar esta proposta** com equipe
2. ⏳ **Criar branch** `feature/restructure`
3. ⏳ **Migração Fase 1**: Criar estrutura nova
4. ⏳ **Migração Fase 2**: Ajustar referências
5. ⏳ **Migração Fase 3**: Refatorar frontend MVC
6. ⏳ **Migração Fase 4**: Implementar sync-docs
7. ⏳ **Validação**: Testar tudo
8. ⏳ **Merge**: Para branch principal
9. ⏳ **Cleanup**: Remover arquivos antigos

---

## 📚 Referências

- [PlatformIO Project Structure](https://docs.platformio.org/en/latest/projectconf/index.html)
- [Docker Best Practices](https://docs.docker.com/develop/dev-best-practices/)
- [Python Project Structure](https://docs.python-guide.org/writing/structure/)
- [JavaScript Module Patterns](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)

---

**Documento criado em:** 31 de outubro de 2025  
**Autor:** GitHub Copilot  
**Status:** 📋 Proposta (Aguardando Aprovação)
