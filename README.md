# Projeto Balança para Foguetemodelismo

## Descrição

Esta é uma aplicação web moderna e orientada a objetos para capturar, visualizar e analisar dados de uma balança de empuxo para foguetes de pequena escala. O projeto foi reescrito a partir de uma versão procedural para adotar boas práticas de engenharia de software, como separação de responsabilidades, modularidade e testabilidade.

## Funcionalidades Principais

- **Conexão em Tempo Real:** Conecta-se a um dispositivo (ESP8266/ESP32) via WebSocket para receber dados de força em tempo real.
- **Visualização de Dados:** Plota os dados de empuxo em um gráfico interativo usando Chartist.js.
- **Análise de Propulsão:** Calcula o impulso total, força máxima, tempo de queima e classifica o motor de acordo com a tabela padrão.
- **Gerenciamento de Sessões:** Permite salvar, carregar e exportar sessões de teste completas (dados brutos e análise).
- **Arquitetura Modular:** Código organizado em classes com responsabilidades únicas, facilitando a manutenção e a expansão.
- **Testes Unitários:** Módulos críticos do núcleo da aplicação são cobertos por testes unitários com Jest.

## Estrutura do Projeto

```
/projeto-balanca
├── /src
│   ├── index.html
│   ├── estilo.css
│   ├── main.js
│   ├── /core
│   │   ├── WebSocketManager.js
│   │   ├── DataProcessor.js
│   │   ├── EmaCalculator.js
│   │   ├── SessionManager.js
│   │   ├── PropulsionAnalyzer.js
│   │   └── UnitConverter.js
│   ├── /ui
│   │   ├── GraphManager.js
│   │   ├── Tooltip.js
│   │   └── UIController.js
│   └── /worker
│       └── dataWorker.js
├── /tests
│   ├── DataProcessor.test.js
│   ├── EmaCalculator.test.js
│   ├── PropulsionAnalyzer.test.js
│   └── UnitConverter.test.js
├── /assets
│   ├── logo.png
│   └── chartist.min.css
├── package.json
└── README.md
```

## Instalação e Setup

Você precisará do [Node.js](https://nodejs.org/) (que inclui o npm) instalado em sua máquina.

1.  **Clone o repositório:**
    ```bash
    git clone <url-do-seu-repositorio>
    cd projeto-balanca
    ```

2.  **Instale as dependências de desenvolvimento:**
    ```bash
    npm install
    ```

## Como Executar

1.  **Iniciar o servidor local:**
    O comando abaixo irá iniciar um servidor web simples e abrir a aplicação no seu navegador padrão.
    ```bash
    npm start
    ```

2.  **Executar os testes:**
    Para rodar a suíte de testes unitários, execute:
    ```bash
    npm test
    