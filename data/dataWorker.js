// --- Variáveis Globais do Worker ---
let socket;
let dataBuffer = []; // Buffer para acumular dados antes de enviar para a UI
let gravity = 9.80665; // Valor padrão, será atualizado pela config
let emaAlpha = 0.2; // Fator de suavização para EMA
let emaValue = 0;
let emaInitialized = false;
let maxForce = -Infinity;

// --- Variáveis para cálculo de Leituras por Segundo (RPS) ---
let readingsCounter = 0;
let lastRpsUpdateTime = Date.now();
let lastCalculatedRps = 0;

/**
 * Conecta ao servidor WebSocket do ESP32.
 */
function connectWebSocket() {

    // Evita criar múltiplas conexões se uma já estiver ativa ou tentando conectar.
    if (socket && socket.readyState !== WebSocket.CLOSED) {
        return;
    }
    // Tenta conectar usando o hostname da página. Altere para IP fixo se necessário.
    //const wsURL = `ws://${self.location.hostname}:81`;
     const wsURL = `ws://192.168.1.2:81`; // Exemplo com IP fixo

    socket = new WebSocket(wsURL);
    socket.timeout = 500; // 500 milissegundos de timeout para conexão

    socket.onopen = () => {
        self.postMessage({ type: 'status', status: 'connected', message: 'Conectado ao dispositivo' });
    };

    socket.onclose = () => {
        self.postMessage({ type: 'status', status: 'disconnected', message: 'Desconectado. Tentando reconectar...' });
        socket = null; // Limpa a referência do socket
    };

    socket.onerror = (error) => {
        self.postMessage({ type: 'status', status: 'error', message: 'Erro na conexão WebSocket.' });
        console.error("WebSocket Error:", error);
        socket = null; // Limpa a referência do socket
      
    };

    socket.onmessage = (event) => {
        // Verifica se o dado recebido é uma string e se parece com um objeto JSON.
        if (typeof event.data === 'string' && event.data.startsWith('{')) {
            try {
                const data = JSON.parse(event.data);

                switch (data.type) {
                    // Mensagem com dados de medição
                    case "data":
                        processDataPoint(data);
                        break;
                    
                    // Mensagem com os parâmetros de configuração do ESP32
                    case "config":
                        if (data.gravity) {
                            gravity = parseFloat(data.gravity);
                        }
                        // Retransmite a configuração para a UI
                        self.postMessage({ type: 'config', payload: data });
                        break;

                    // Mensagem de status (ex: "Calibrando...", "Tara OK")
                    case "status":
                        self.postMessage({ type: 'status', status: data.status, message: data.message });
                        break;
                }
            } catch (e) {
                // Se o parse falhar, trata como uma mensagem de status de texto.
                console.warn("Worker: JSON malformado, tratando como status.", event.data);
                self.postMessage({ type: 'status', status: 'info', message: event.data });
            }
        } else {
            // Se não for um JSON, trata como uma mensagem de status de texto puro.
            self.postMessage({ type: 'status', status: 'info', message: event.data });
        }
    };
}

/**
 * Processa um único ponto de dado recebido do ESP32.
 * @param {object} data - O objeto de dados com {tempo, forca}.
 */
function processDataPoint(data) {
    const forceN = data.forca;
    
    // Atualiza a força máxima
    if (forceN > maxForce) {
        maxForce = forceN;
    }

    // Calcula a Média Móvel Exponencial (EMA)
    const ema = getEmaValue(forceN);
    // Calcula a massa em kg
    const massaKg = gravity > 0 ? forceN / gravity : 0;

    // Adiciona o dado processado ao buffer
    dataBuffer.push({
        tempo: data.tempo,
        forca: forceN,
        ema: ema,
        maxForce: maxForce,
        massaKg: massaKg
    });

    // Incrementa o contador para o cálculo de RPS
    readingsCounter++;
}

/**
 * Calcula a Média Móvel Exponencial (EMA).
 * @param {number} newValue - O novo valor de força.
 * @returns {number} O valor EMA calculado.
 */
function getEmaValue(newValue) {
    if (!emaInitialized) {
        emaValue = newValue;
        emaInitialized = true;
    } else {
        emaValue = (emaAlpha * newValue) + ((1 - emaAlpha) * emaValue);
    }
    return emaValue;
}

/**
 * Manipulador de mensagens vindas da thread principal (script.js).
 */
self.onmessage = (e) => {
    const { type, payload } = e.data;

    switch (type) {
        // A UI está pedindo os dados acumulados
        case 'solicitarDados':
            if (dataBuffer.length > 0) {
                self.postMessage({ type: 'dadosDisponiveis', payload: dataBuffer });
                dataBuffer = []; // Limpa o buffer após o envio
            }
            break;

        // A UI está pedindo as leituras por segundo
        case 'getRPS':
            const now = Date.now();
            if (now - lastRpsUpdateTime >= 1000) {
                lastCalculatedRps = readingsCounter;
                readingsCounter = 0;
                lastRpsUpdateTime = now;
            }
            self.postMessage({ type: 'rps', payload: lastCalculatedRps });
            break;

        // A UI está enviando um comando para o ESP32
        case 'sendCommand':
            if (socket && socket.readyState === WebSocket.OPEN) {
                socket.send(payload);
            }
            break;
    }
};


/**
 * Inicia o Gerenciador de Conexão.
 * Este loop verifica o estado da conexão a cada 2 segundos e tenta conectar se necessário.
 * Isso garante uma reconexão rápida e evita o backoff exponencial do navegador.
 */
setInterval(() => {
    if (socket==null || socket.readyState === WebSocket.CLOSED) {
        connectWebSocket();
        console.log("Tentando reconectar ao WebSocket...");
    }
}, 2000); 
// Inicia a conexão WebSocket assim que o worker é carregado.

