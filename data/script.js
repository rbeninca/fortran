// --- Variáveis Globais da UI ---
let chart;
let dataWorker;
const MAX_DATA_POINTS = 120; // Pontos no gráfico deslizante
let chartMode = 'deslizante';
let displayUnit = 'kgf';
let maxForceInN = -Infinity;
let minForceInN = Infinity;
let chartData = { labels: [], series: [[]] };
let rawDataN = []; // Armazena os dados brutos em Newtons para recalcular o gráfico
let connectionTimeout; // Temporizador para detectar perda de conexão
let antiNoisingAtivo = false;
let isSessionActive = false; // <<< NOVO: Controla se a gravação na tabela está ativa

// --- Funções de Inicialização ---
window.onload = () => {
  // Abre a aba padrão
  abrirAba(document.getElementById("padrao"), 'abaGrafico');

  // Inicializa o gráfico Chartist
  const chartOptions = {
    showPoint: true,
    lineSmooth: Chartist.Interpolation.cardinal({ tension: 0 }),
    axisX: { showGrid: true, showLabel: true ,
             labelInterpolationFnc: (value) => value +"s"},
    axisY: { showGrid: true, showLabel: true },
    fullWidth: true,
    chartPadding: { right: 40 }
  };
  chart = new Chartist.Line('#grafico', chartData, chartOptions);

  // Configura estado inicial dos botões
  setDisplayUnit('kgf');
  setChartMode('deslizante');

  // Carrega gravações salvas no LocalStorage
  carregarGravacoes();

  // Inicia e conecta ao Web Worker
  conectarWorker();

  // Inicia o loop para pedir o RPS (Leituras por Segundo)
  setInterval(updateReadingsPerSecond, 1000);
};

function conectarWorker() {
  if (window.Worker) {
    if (!dataWorker) {
      dataWorker = new Worker('dataWorker.js');
      dataWorker.onmessage = handleWorkerMessage;

      // Solicita os dados da balança a cada 200ms
      setInterval(() => {
        dataWorker.postMessage({ type: 'solicitarDados' });
      }, 200);
    }
  } else {
    showNotification('error', 'Seu navegador não suporta Web Workers.');
  }
}

/**
 * Reinicia o temporizador de timeout da conexão.
 */
function resetConnectionTimeout() {
  clearTimeout(connectionTimeout);
  connectionTimeout = setTimeout(() => {
    updateConnectionStatus(false);
    document.getElementById('balanca-status').textContent = 'Dispositivo não responde.';
  }, 1000); // 1 segundo de timeout
}

// --- Manipulador de Mensagens do Worker ---
function handleWorkerMessage(event) {
  const { type, payload, status, message } = event.data;
  resetConnectionTimeout();
  switch (type) {
    case 'dadosDisponiveis':
      payload.forEach(updateUIFromData);
      break;

    case 'rps':
      document.getElementById('leituras-por-segundo').textContent = payload;
      break;

    case 'config':
      updateConfigForm(payload);
      break;

    case 'status':
      document.getElementById('balanca-status').textContent = message || status;
      if (message) {
        const notificationType = (status === 'error' || status === 'disconnected') ? 'erro' : 'info';
        showNotification(notificationType, message);
      }
      if (status === 'connected') {
        updateConnectionStatus(true);
      } else if (status === 'disconnected' || status === 'error') {
        clearTimeout(connectionTimeout);
        updateConnectionStatus(false);
      }
      break;

    case 'error':
      showNotification("erro", message || "Erro desconhecido no worker");
      break;

    default:
      console.warn("Mensagem desconhecida do worker:", event.data);
  }
}

// --- Funções de Atualização da UI ---
function updateUIFromData(dado) {
  const { tempo, forca, ema, maxForce, massaKg } = dado;

  // Atualiza a força máxima e mínima global se necessário
  if (forca > maxForceInN) maxForceInN = forca;
  if (forca < minForceInN) minForceInN = forca;

  let forcaFiltrada = forca;
  if (antiNoisingAtivo) {
    const toleranciaBruta = parseFloat(document.getElementById("param-tolerancia").value);
    const fatorConversao = parseFloat(document.getElementById("param-conversao").value);
    const toleranciaN = (!isNaN(toleranciaBruta) && !isNaN(fatorConversao) && fatorConversao !== 0)
      ? toleranciaBruta / fatorConversao
      : 0;

    if (Math.abs(forca) <= toleranciaN) {
      forcaFiltrada = 0;
    } else {
      forcaFiltrada = forca - Math.sign(forca) * toleranciaN;
    }
  }
  const displayForce = convertForce(forcaFiltrada, displayUnit);
  const maxDisplayForce = convertForce(maxForceInN, displayUnit);
  const emaDisplay = convertForce(ema, displayUnit);
  const minDisplayForce = convertForce(minForceInN, displayUnit);

  // Atualiza os painéis de leitura (SEMPRE ATUALIZA)
  document.getElementById('forca-atual').textContent = `${formatForce(displayForce, displayUnit)} ${displayUnit}`;
  document.getElementById('forca-maxima').textContent = `${formatForce(maxDisplayForce, displayUnit)} ${displayUnit}`;
  document.getElementById('forca-minima').textContent = `mín: ${formatForce(minDisplayForce, displayUnit)} ${displayUnit}`;
  document.getElementById('forca-ems').textContent = `${formatForce(emaDisplay, displayUnit)} ${displayUnit}`;

  // Atualiza o gráfico se não estiver pausado.
  if (chartMode !== 'pausado') {
    rawDataN.push(forca); // Adiciona ao array bruto para recalcular unidades
    chartData.labels.push(tempo.toFixed(1));
    chartData.series[0].push(parseFloat(formatForce(displayForce, displayUnit)));

    if (chartMode === 'deslizante' && chartData.labels.length > MAX_DATA_POINTS) {
      chartData.labels.shift();
      chartData.series[0].shift();
      rawDataN.shift(); 
    }
    chart.update(chartData);
  }

  // Adiciona a nova leitura à tabela APENAS se uma sessão estiver ativa.
  // A gravação na tabela continua mesmo se o gráfico estiver pausado.
  if (isSessionActive) {
    const tbody = document.getElementById("tabela").querySelector("tbody");
    const linha = tbody.insertRow(0); // Insere no topo
    const agora = new Date();
    const timestamp = `${agora.toLocaleDateString('pt-BR')} ${agora.toLocaleTimeString('pt-BR')}.${String(agora.getMilliseconds()).padStart(3, '0')}`;

    linha.insertCell(0).innerText = timestamp;
    linha.insertCell(1).innerText = tempo.toFixed(1);
    linha.insertCell(2).innerText = forca.toFixed(3);
    linha.insertCell(3).innerText = (massaKg * 1000).toFixed(1);
    linha.insertCell(4).innerText = massaKg.toFixed(4);

    if (tbody.rows.length > 5000) {
      tbody.deleteRow(tbody.rows.length - 1);
    }
  }
}

// --- NOVAS FUNÇÕES DE CONTROLE DE SESSÃO ---

function iniciarSessao() {
    const nomeSessaoInput = document.getElementById('nome-sessao');
    const nomeSessao = nomeSessaoInput.value.trim();
    if (!nomeSessao) {
        showNotification('error', 'Por favor, insira um nome para a sessão.');
        nomeSessaoInput.focus();
        return;
    }

    // Limpa a tabela e o gráfico para a nova sessão
    clearChart(); 
    document.getElementById("tabela").querySelector("tbody").innerHTML = '';


    isSessionActive = true;
    showNotification('success', `Sessão "${nomeSessao}" iniciada. Gravando dados...`);

    // Gerencia o estado dos botões
    document.getElementById('btn-iniciar-sessao').disabled = true;
    nomeSessaoInput.disabled = true;
    document.getElementById('btn-encerrar-sessao').disabled = false;
}

function encerrarSessao() {
    if (!isSessionActive) return;

    const nomeSessao = document.getElementById('nome-sessao').value.trim();
    const tabela = document.getElementById("tabela").querySelector("tbody");

    if (tabela.rows.length === 0) {
        showNotification('info', 'Nenhum dado foi gravado nesta sessão. Nada foi salvo.');
    } else {
        // Chama a função para salvar os dados da tabela no Local Storage
        salvarDadosDaSessao(nomeSessao, tabela);
    }

    isSessionActive = false;

    // Reseta o estado dos botões para permitir uma nova sessão
    const nomeSessaoInput = document.getElementById('nome-sessao');
    document.getElementById('btn-iniciar-sessao').disabled = false;
    nomeSessaoInput.disabled = false;
    nomeSessaoInput.value = ''; 
    document.getElementById('btn-encerrar-sessao').disabled = true;
}

function salvarDadosDaSessao(nome, tabela) {
    const dadosTabela = [];
    // Itera pelas linhas da tabela para montar o objeto de gravação
    for (const linha of tabela.rows) {
        dadosTabela.push({
            timestamp: linha.cells[0].innerText,
            tempo_esp: linha.cells[1].innerText,
            newtons: linha.cells[2].innerText,
            grama_forca: linha.cells[3].innerText,
            quilo_forca: linha.cells[4].innerText
        });
    }

    const gravacao = {
        id: Date.now(),
        nome: nome,
        timestamp: new Date().toISOString(),
        dadosTabela: dadosTabela.reverse() // Salva na ordem cronológica correta
    };

    try {
        let gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
        gravacoes.push(gravacao);
        localStorage.setItem('balancaGravacoes', JSON.stringify(gravacoes));
        showNotification('success', `Sessão "${nome}" salva com sucesso!`);
        carregarGravacoes(); // Atualiza a lista de gravações salvas na UI
    } catch (e) {
        showNotification('error', 'Erro ao salvar. O Local Storage pode estar cheio.');
        console.error("Erro ao salvar no LocalStorage:", e);
    }
}


// --- Funções de UI e Utilitários (demais funções sem alteração) ---

function updateConfigForm(config) {
  const getValue = (val) => (val !== null && val !== undefined) ? val : '';
  document.getElementById("ssid").value = getValue(config.ssid);
  document.getElementById("senha").value = getValue(config.password);
  document.getElementById("param-conversao").value = getValue(config.conversionFactor);
  document.getElementById("param-gravidade").value = getValue(config.gravity);
  document.getElementById("param-offset").value = getValue(config.tareOffset);
  document.getElementById("param-leituras-estaveis").value = getValue(config.leiturasEstaveis);
  document.getElementById("param-tolerancia").value = getValue(config.toleranciaEstabilidade);
  atualizarToleranciaEmGramas();
  document.getElementById("param-num-amostras").value = getValue(config.numAmostrasMedia);
  document.getElementById("param-timeout").value = getValue(config.timeoutCalibracao);
}

function updateConnectionStatus(isConnected) {
  const indicator = document.getElementById('ws-indicator');
  indicator.classList.toggle('conectado', isConnected);
  document.getElementById('ws-text').textContent = isConnected ? "Conectado" : "Desconectado";
  if (!isConnected) {
    tocarBip();
  }
}

function updateReadingsPerSecond() {
  if (dataWorker) {
    dataWorker.postMessage({ type: 'getRPS' });
  }
}

function sendCommandToWorker(command, value = null) {
  if (dataWorker) {
    const message = value !== null ? `${command}:${value}` : command;
    dataWorker.postMessage({ type: 'sendCommand', payload: message });
  } else {
    showNotification("error", "Worker não está conectado.");
  }
}

function tare() {
  sendCommandToWorker("t");
  showNotification('info', 'Comando de Tara enviado.');
}

function calibrar() {
  const massa = parseFloat(document.getElementById("massaCalibracao").value);
  if (!isNaN(massa) && massa > 0) {
    sendCommandToWorker("c", massa);
    showNotification('info', `Comando de calibração com ${massa}g enviado.`);
  } else {
    showNotification("error", "Informe uma massa de calibração válida.");
  }
}

function salvarParametros() {
  const params = {
    conversionFactor: "param-conversao",
    gravity: "param-gravidade",
    tareOffset: "param-offset",
    leiturasEstaveis: "param-leituras-estaveis",
    toleranciaEstabilidade: "param-tolerancia",
    numAmostrasMedia: "param-num-amostras",
    timeoutCalibracao: "param-timeout",
  };

  for (const [key, id] of Object.entries(params)) {
    const value = document.getElementById(id).value;
    if (value !== '') {
      sendCommandToWorker(`set_param`, `${key}:${value}`);
    }
  }
  showNotification('success', 'Parâmetros enviados para salvamento no ESP32.');
}

function formatForce(value, unit) {
  if (unit === 'N') return value.toFixed(4);
  if (unit === 'gf') return value.toFixed(0);
  if (unit === 'kgf') return value.toFixed(3);
  return value.toFixed(3);
}

function convertForce(valueN, unit) {
  const g_force_conversion = 101.9716; // 1 N = 101.9716 gf
  if (unit === 'gf') return valueN * g_force_conversion;
  if (unit === 'kgf') return valueN * (g_force_conversion / 1000);
  return valueN; // Retorna em Newtons (N)
}

function setDisplayUnit(unit) {
  displayUnit = unit;
  document.querySelectorAll('#btn-unit-n, #btn-unit-gf, #btn-unit-kgf').forEach(b => b.classList.remove('ativo'));
  document.getElementById(`btn-unit-${unit.toLowerCase()}`).classList.add('ativo');

  chartData.series[0] = rawDataN.map(forceN => parseFloat(formatForce(convertForce(forceN, unit), unit)));
  chart.update(chartData);

  const currentForceN = rawDataN.length > 0 ? rawDataN[rawDataN.length - 1] : 0;
  const currentDisplayForce = convertForce(currentForceN, displayUnit);
  const maxDisplayForce = convertForce(maxForceInN, displayUnit);
  document.getElementById('forca-atual').textContent = `${formatForce(currentDisplayForce, displayUnit)} ${displayUnit}`;
  document.getElementById('forca-maxima').textContent = `${formatForce(maxDisplayForce, displayUnit)} ${displayUnit}`;
}

function setChartMode(mode) {
  chartMode = mode;
  document.querySelectorAll('#btn-deslizante, #btn-acumulado, #btn-pausado').forEach(b => b.classList.remove('ativo'));
  document.getElementById(`btn-${mode}`).classList.add('ativo');
}

function clearChart() {
  chartData.labels = [];
  chartData.series = [[]];
  rawDataN = [];
  maxForceInN = -Infinity;
  minForceInN = Infinity;
  document.getElementById('forca-atual').textContent = `--- ${displayUnit}`;
  document.getElementById('forca-maxima').textContent = `--- ${displayUnit}`;
  document.getElementById('forca-minima').textContent = `--- ${displayUnit}`;
  chart.update(chartData);
  showNotification("info", "Gráfico limpo.", 3000);
}

function abrirAba(element, abaID) {
  document.querySelectorAll('.tabcontent').forEach(tab => { tab.style.display = "none"; tab.classList.remove('active'); });
  document.querySelectorAll('.tablink').forEach(link => link.classList.remove('active'));
  const el = document.getElementById(abaID);
  el.style.display = "block";
  el.classList.add('active');
  element.classList.add('active');
}

function showNotification(type, message, duration = 5000) {
  const area = document.getElementById('notification-area');
  const notification = document.createElement('div');
  notification.className = `notificacao ${type}`;
  notification.innerHTML = `<p class="titulo">${type.charAt(0).toUpperCase() + type.slice(1)}</p><p>${message}</p>`;
  area.prepend(notification);
  setTimeout(() => {
    notification.style.transition = 'opacity 0.5s';
    notification.style.opacity = '0';
    setTimeout(() => notification.remove(), 500);
  }, duration);
}

function salvarRede(event) {
  event.preventDefault();
  const form = new FormData(event.target);
  fetch("/salvarRede", { method: "POST", body: new URLSearchParams(form) })
    .then(r => r.text())
    .then(text => showNotification("success", text))
    .catch(err => showNotification("error", "Falha ao salvar a rede: " + err));
}

function carregarGravacoes() {
  const container = document.getElementById('lista-gravacoes');
  container.innerHTML = '';
  const gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
  if (gravacoes.length === 0) {
    container.innerHTML = '<p>Nenhuma gravação encontrada.</p>';
    return;
  }
  gravacoes.sort((a, b) => b.id - a.id); 
  gravacoes.forEach(gravacao => {
    const dataFormatada = new Date(gravacao.timestamp).toLocaleString('pt-BR');
    const card = document.createElement('div');
    card.className = 'card-gravacao';
    card.innerHTML = `
      <div>
        <p style="font-weight: 600;">${gravacao.nome}</p> 
        <p style="font-size: 0.875rem; color: var(--cor-texto-secundario);">Salvo em: ${dataFormatada} (${gravacao.dadosTabela.length} leituras)</p>
      </div>
      <div class="botoes">
        <button onclick="exportarCSV(${gravacao.id})" class="btn btn-sucesso">Exportar CSV</button>
        <button onclick="deletarGravacao(${gravacao.id})" class="btn btn-perigo">Deletar</button>
      </div>`;
    container.appendChild(card);
  });
}

function exportarCSV(id) {
  const gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
  const gravacao = gravacoes.find(g => g.id === id);
  if (!gravacao) return;
  const cabecalho = Object.keys(gravacao.dadosTabela[0]).join(';');
  const linhas = gravacao.dadosTabela.map(linha => Object.values(linha).join(';'));
  const conteudoCSV = `\uFEFF${cabecalho}\n${linhas.join('\n')}`;
  const blob = new Blob([conteudoCSV], { type: 'text/csv;charset=utf-8;' });
  const link = document.createElement('a');
  link.href = URL.createObjectURL(blob);
  link.setAttribute('download', `gravacao_balanca_${gravacao.nome.replace(/\s/g, "_")}_${id}.csv`);
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
}

function deletarGravacao(id) {
  if (!confirm('Tem certeza que deseja deletar esta gravação?')) return;
  let gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
  const novasGravacoes = gravacoes.filter(g => g.id !== id);
  localStorage.setItem('balancaGravacoes', JSON.stringify(novasGravacoes));
  showNotification('info', 'Gravação deletada.');
  carregarGravacoes();
}

function tocarBip() {
  const contexto = new (window.AudioContext || window.webkitAudioContext)();
  const oscilador = contexto.createOscillator();
  oscilador.type = 'square';
  oscilador.frequency.setValueAtTime(880, contexto.currentTime);
  oscilador.connect(contexto.destination);
  oscilador.start();
  oscilador.stop(contexto.currentTime + 0.2);
}

function atualizarToleranciaEmGramas() {
  const toleranciaBruta = parseFloat(document.getElementById("param-tolerancia").value);
  const fatorConversao = parseFloat(document.getElementById("param-conversao").value);
  if (!isNaN(toleranciaBruta) && !isNaN(fatorConversao) && fatorConversao !== 0) {
    const toleranciaN = toleranciaBruta / fatorConversao;
    const toleranciaGf = toleranciaN;
    document.getElementById("tolerancia-em-gramas").textContent = `≈ ${toleranciaGf.toFixed(3)} gf`;
  } else {
    document.getElementById("tolerancia-em-gramas").textContent = '';
  }
}

function toggleAntiNoising() {
  antiNoisingAtivo = !antiNoisingAtivo;
  const btn = document.getElementById('btn-anti-noising');
  btn.textContent = antiNoisingAtivo ? 'Anti-Noising: On (ΔT compensado)' : 'Anti-Noising: Off';
  btn.classList.toggle('btn-aviso', antiNoisingAtivo);
}
