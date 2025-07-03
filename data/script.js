let chart;
let socket;
const MAX_DATA_POINTS = 120;
let chartMode = 'deslizante';
let displayUnit = 'kgf'; // Default unit
let maxForceInN = -Infinity;
let chartData = { labels: [], series: [[]] };
let rawDataN = [];
let gravity = 9.80665;

let readingsCounter = 0;
let lastUpdateTime = Date.now();

let emaAlpha = 0.2; // quanto menor, mais suave
let emaValue = 0;
let emaInitialized = false;
let emaBuffer = [];

function formatForce(value, unit) { if (unit === 'N') return value.toFixed(2); if (unit === 'gf') return value.toFixed(0); if (unit === 'kgf') return value.toFixed(3); return value.toFixed(3); }
function convertForce(valueN, unit) { if (unit === 'gf') return valueN * 101.97; if (unit === 'kgf') return valueN * 0.10197; return valueN; }

window.onload = () => {
  abrirAba(document.getElementById("padrao"), 'abaGrafico');
  
  // --- INICIALIZAÇÃO CORRIGIDA PARA CHARTIST.JS ---
  const chartOptions = {
    showPoint: false,
    lineSmooth: Chartist.Interpolation.cardinal({ tension: 0 }),
    axisX: { showGrid: true, showLabel: true },
    axisY: { showGrid: true, showLabel: true },
    fullWidth: true,
    chartPadding: { right: 40 }
  };
  chart = new Chartist.Line('#grafico', chartData, chartOptions);

  setDisplayUnit('kgf');
  setChartMode('deslizante');
  carregarGravacoes();
  conectarWebSocket();
  
  setInterval(updateReadingsPerSecond, 1000);
};

function conectarWebSocket() {
  const wsURL = "ws://" + location.hostname + ":81";
  socket = new WebSocket(wsURL);
  socket.onopen = () => updateConnectionStatus(true);
  socket.onclose = () => { updateConnectionStatus(false); setTimeout(conectarWebSocket, 3000); };
  socket.onerror = () => updateConnectionStatus(false);
  socket.onmessage = (event) => {
    try {
      const data = JSON.parse(event.data);
      switch (data.type) {
        case "data": 
              readingsCounter++;
              updateUI(data);
          break;
        case "status": showNotification(data.status, data.message, (data.status === 'info') ? 2000 : 7000); break;
        case "config": updateConfigForm(data); break;
      }
    } catch (e) { console.error("Erro ao processar JSON:", e, event.data); }
  };
}

function updateUI(dado) {
  document.getElementById('balanca-status').textContent = dado.status; //modificado para contador fazer
  const forceN = dado.forca;
  rawDataN.push(forceN);
  if (forceN > maxForceInN) { maxForceInN = forceN; }
  
  const displayForce = convertForce(forceN, displayUnit);
  const maxDisplayForce = convertForce(maxForceInN, displayUnit);
  document.getElementById('forca-atual').textContent = `${formatForce(displayForce, displayUnit)} ${displayUnit}`;
  document.getElementById('forca-maxima').textContent = `${formatForce(maxDisplayForce, displayUnit)} ${displayUnit}`;

  if (chartMode === 'pausado') return;

  const espTime = dado.tempo.toFixed(1);
  chartData.labels.push(espTime);
  chartData.series[0].push(formatForce(displayForce, displayUnit));

  if (chartData.labels.length > MAX_DATA_POINTS && chartMode === 'deslizante') {
    chartData.labels.shift();
    chartData.series[0].shift();
    rawDataN.shift();
    const emaForcaN = getEmaValue(forceN);
    const emaDisplay = convertForce(emaForcaN, displayUnit);
    document.getElementById('forca-ems').textContent = `${formatForce(emaDisplay, displayUnit)} ${displayUnit}`;
  }
  chart.update(chartData);

  const tbody = document.getElementById("tabela").querySelector("tbody");
  const linha = tbody.insertRow(0);
  const agora = new Date();
  const timestamp = `${agora.getFullYear()}-${String(agora.getMonth() + 1).padStart(2, '0')}-${String(agora.getDate()).padStart(2, '0')} ${agora.toLocaleTimeString('pt-BR')}.${String(agora.getMilliseconds()).padStart(3, '0')}`;
  linha.insertCell(0).innerText = timestamp;
  linha.insertCell(1).innerText = espTime;
  linha.insertCell(2).innerText = forceN.toFixed(2);
  let massaKg = forceN > 0 && gravity > 0 ? forceN / gravity : 0;
  linha.insertCell(3).innerText = (massaKg * 1000).toFixed(0);
  linha.insertCell(4).innerText = massaKg.toFixed(3);
  if (tbody.rows.length > 200000) { tbody.deleteRow(200000); }
}

function updateConfigForm(config) {
  const getValue = (val) => val != null ? val : '';
  document.getElementById("ssid").value = getValue(config.ssid);
  document.getElementById("senha").value = getValue(config.password);
  document.getElementById("param-conversao").value = getValue(config.conversionFactor);
  document.getElementById("param-gravidade").value = getValue(config.gravity);
  document.getElementById("param-leituras-estaveis").value = getValue(config.leiturasEstaveis);
  document.getElementById("param-tolerancia").value = getValue(config.toleranciaEstabilidade);
  document.getElementById("param-num-amostras").value = getValue(config.numAmostrasMedia);
  document.getElementById("param-timeout").value = getValue(config.timeoutCalibracao);
  document.getElementById("param-offset").value = getValue(config.tareOffset);
  if(config.gravity) { gravity = parseFloat(config.gravity); }
}

function sendCommand(cmd) { if (socket && socket.readyState === WebSocket.OPEN) { socket.send(cmd); } else { showNotification("error", "Não conectado."); } }
function tare() { sendCommand("t"); }
function calibrar() { const massa = parseFloat(document.getElementById("massaCalibracao").value); if (massa > 0) { sendCommand("c:" + massa); } else { showNotification("error", "Informe uma massa válida."); } }
function salvarParametros() { 
  const params = { conversionFactor: "param-conversao",
                    gravity: "param-gravidade",
                    leiturasEstaveis: "param-leituras-estaveis", 
                    toleranciaEstabilidade: "param-tolerancia", 
                    numAmostrasMedia: "param-num-amostras", 
                    timeoutCalibracao: "param-timeout",
                    tareOffset: "param-offset"
                  }; 
    for (const [key, id] of Object.entries(params)) 
      { const value = document.getElementById(id).value; 
        if (value) { 
          sendCommand(`set_param:${key}:${value}`); 
        } 
      }
}

function salvarRede(event) { event.preventDefault(); const form = new FormData(event.target); fetch("/salvarRede", { method: "POST", body: new URLSearchParams(form) }).then(r => r.text()).then(text => showNotification("success", text)); }

function setDisplayUnit(unit) {
    displayUnit = unit;
    document.querySelectorAll('.btn-grupo .btn').forEach(b => b.classList.remove('ativo'));
    
    
    // Converte a unidade para minúsculas ao procurar o ID, para corresponder ao HTML ('n' em vez de 'N')
    const activeButton = document.getElementById(`btn-unit-${unit.toLowerCase()}`);
    if (activeButton) {
        activeButton.classList.add('ativo');
    }

    chartData.series[0] = rawDataN.map(forceN => formatForce(convertForce(forceN, unit), unit));
    chart.update(chartData);
    const maxDisplayForce = convertForce(maxForceInN, displayUnit);
    document.getElementById('forca-maxima').textContent = `${formatForce(maxDisplayForce, displayUnit)} ${displayUnit}`;
    const currentForceN = rawDataN.length > 0 ? rawDataN[rawDataN.length-1] : 0;
    const currentDisplayForce = convertForce(currentForceN, displayUnit);
    document.getElementById('forca-atual').textContent = `${formatForce(currentDisplayForce, displayUnit)} ${displayUnit}`;
}

function setChartMode(mode) {
    chartMode = mode;
    document.querySelectorAll('.btn-grupo .btn').forEach(b => {
        if (b.id.includes('deslizante') || b.id.includes('acumulado') || b.id.includes('pausado')) {
            b.classList.remove('ativo');
        }
    });
    const activeButton = document.getElementById(`btn-${mode}`);
    if (activeButton) {
      activeButton.classList.add('ativo');
    }
}

function clearChart() {
    chartData.labels = []; chartData.series = [[]]; rawDataN = [];
    maxForceInN = -Infinity;
    document.getElementById('forca-atual').textContent = `--- ${displayUnit}`;
    document.getElementById('forca-maxima').textContent = `--- ${displayUnit}`;
    document.getElementById("tabela").querySelector("tbody").innerHTML = '';
    chart.update(chartData);
    showNotification("info", "Sessão atual limpa.", 2000);
}

function abrirAba(element, abaID) {
  document.querySelectorAll('.tabcontent').forEach(tab => { tab.style.display = "none"; tab.classList.remove('active'); });
  document.querySelectorAll('.tablink').forEach(link => link.classList.remove('active'));
  const el = document.getElementById(abaID);
  el.style.display = "block"; el.classList.add('active');
  element.classList.add('active');
}

function updateConnectionStatus(isConnected) {
  const indicator = document.getElementById('ws-indicator');
  indicator.classList.toggle('conectado', isConnected);
  document.getElementById('ws-text').textContent = isConnected ? "Conectado" : "Desconectado";
}

function showNotification(type, message, duration = 7000) {
    const area = document.getElementById('notification-area');
    const notification = document.createElement('div');
    notification.className = `notificacao ${type}`;
    notification.innerHTML = `<p class="titulo">${type.charAt(0).toUpperCase() + type.slice(1)}</p><p>${message}</p>`;
    area.prepend(notification);
    if(duration > 0) setTimeout(() => { notification.style.transition = 'opacity 0.3s';
       notification.style.opacity = '0';
        setTimeout(() => notification.remove(), 5000);
       }, duration);
}

function salvarGravacao() {
    const nomeInput = document.getElementById('nome-gravacao');
    const nome = nomeInput.value.trim();
    if (!nome) { showNotification('error', 'Digite um nome para a gravação.', 3000); nomeInput.focus(); return; }
    const tabela = document.getElementById("tabela").querySelector("tbody");
    if (tabela.rows.length === 0) { showNotification('error', 'Não há dados para salvar.', 3000); return; }
    const dadosTabela = [];
    for (const linha of tabela.rows) {
        dadosTabela.push({ timestamp: linha.cells[0].innerText, tempo_esp: linha.cells[1].innerText, newtons: linha.cells[2].innerText, grama_forca: linha.cells[3].innerText, quilo_forca: linha.cells[4].innerText });
    }
    const gravacao = { id: Date.now(), nome: nome, timestamp: new Date().toISOString(), dadosTabela: dadosTabela.reverse() };
    try {
        let gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
        gravacoes.push(gravacao);
        localStorage.setItem('balancaGravacoes', JSON.stringify(gravacoes));
        showNotification('success', `Gravação "${nome}" salva!`);
        carregarGravacoes();
        nomeInput.value = '';
    } catch (e) { showNotification('error', 'Erro ao salvar. Local Storage pode estar cheio.'); }
}

function carregarGravacoes() {
    const container = document.getElementById('lista-gravacoes');
    const gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
    container.innerHTML = '';
    if (gravacoes.length === 0) { container.innerHTML = '<p>Nenhuma gravação encontrada.</p>'; return; }
    gravacoes.reverse().forEach(gravacao => {
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


function updateReadingsPerSecond() {
    const now = Date.now();
    const elapsedTime = (now - lastUpdateTime) / 1000; // Tempo em segundos
    if (elapsedTime > 0) {
        const rps = (readingsCounter / elapsedTime).toFixed(1);
        document.getElementById('leituras-por-segundo').textContent = rps;
    }
    // Reseta para o próximo intervalo
    readingsCounter = 0;
    lastUpdateTime = now;
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
    link.setAttribute('download', `gravacao_balanca_${gravacao.nome.replace(/ /g,"_")}_${id}.csv`);
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

function getEmaValue(newValue) {
    if (!emaInitialized) {
        emaValue = newValue;
        emaInitialized = true;
    } else {
        emaValue = emaAlpha * newValue + (1 - emaAlpha) * emaValue;
    }

    // Armazena no buffer circular para histórico de 100
    emaBuffer.push(emaValue);
    if (emaBuffer.length > readingsCounter*2) {
        emaBuffer.shift();
    }

    return emaValue;
}