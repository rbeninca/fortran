let chart;
let socket;
const MAX_DATA_POINTS = 100;
let chartMode = 'deslizante'; // 'deslizante', 'acumulado', 'pausado'
let displayUnit = 'N'; // Unidade de exibição padrão: 'N', 'gf', 'kgf'
let maxForceInN = -Infinity; // Guarda sempre a força máxima na unidade base (Newtons)
let rawDataN = []; // Guarda os dados brutos em Newtons para conversão
let gravity = 9.80665; // Valor padrão, será atualizado ao conectar

// --- NOVA FUNÇÃO PARA FORMATAR A PRECISÃO ---
// Centraliza a lógica de arredondamento conforme a unidade.
function formatForce(value, unit) {
    if (unit === 'N') {
        // Newtons: 2 casas decimais
        return value.toFixed(2);
    }
    if (unit === 'gf') {
        // Grama-força: nenhuma casa decimal (número inteiro)
        return value.toFixed(0);
    }
    if (unit === 'kgf') {
        // Quilograma-força: 3 casas decimais (representando as gramas)
        return value.toFixed(3);
    }
    // Fallback padrão, caso uma nova unidade seja adicionada
    return value.toFixed(3);
}

window.onload = () => {
  abrirAba(document.getElementById("padrao"), 'abaGrafico');
  const ctx = document.getElementById("grafico").getContext("2d");
  chart = new Chart(ctx, {
    type: 'line',
    data: { labels: [], datasets: [{ label: 'Força', data: [], borderColor: '#3b82f6', backgroundColor: 'rgba(59, 130, 246, 0.1)', borderWidth: 2, fill: true, tension: 0.4 }] },
    options: { 
      animation: false, 
      responsive: true, 
      scales: { 
        x: { title: { display: true, text: 'Tempo (s)' } }, 
        y: { title: { display: true, text: 'Força (N)' } } 
      },
      plugins: {
        zoom: {
          pan: { enabled: true, mode: 'x' },
          zoom: { wheel: { enabled: true }, pinch: { enabled: true }, mode: 'x' }
        }
      }
    }
  });
  setChartMode('deslizante');
  carregarGravacoes(); // <-- NOVO: Carrega a lista de gravações ao iniciar
  conectarWebSocket();
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
          updateUI(data);
          break;
        case "status":
          document.getElementById('notification-area').innerHTML = '';
          const duration = (data.status === 'info') ? 0 : 7000;
          showNotification(data.status, data.message, duration);
          break;
        case "config":
          updateConfigForm(data);
          break;
      }
    } catch (e) {
      console.error("Erro ao processar JSON:", e, event.data);
    }
  };
}

function convertForce(valueN, unit) {
    if (unit === 'gf') return valueN * 101.97;
    if (unit === 'kgf') return valueN * 0.10197;
    return valueN;
}

function updateUI(dado) {
  document.getElementById('balanca-status').textContent = dado.status;
  
  const forceN = dado.forca;
  rawDataN.push(forceN);
  
  if (forceN > maxForceInN) {
      maxForceInN = forceN;
  }
  
  const displayForce = convertForce(forceN, displayUnit);
  const maxDisplayForce = convertForce(maxForceInN, displayUnit);

  // --- ALTERADO: Usa a nova função de formatação ---
  document.getElementById('forca-atual').textContent = `${formatForce(displayForce, displayUnit)} ${displayUnit}`;
  document.getElementById('forca-maxima').textContent = `${formatForce(maxDisplayForce, displayUnit)} ${displayUnit}`;

  if (chartMode === 'pausado') return;

  const { labels, datasets } = chart.data;
  const espTime = dado.tempo.toFixed(2);
  labels.push(espTime);
  // --- ALTERADO: Usa a nova função de formatação ---
  datasets[0].data.push(formatForce(displayForce, displayUnit));

  if (rawDataN.length > MAX_DATA_POINTS && chartMode === 'deslizante') {
    rawDataN.shift();
    labels.shift();
    datasets[0].data.shift();
  }
  
  chart.update(chartMode === 'acumulado' ? 'none' : undefined);

   const tbody = document.getElementById("tabela").querySelector("tbody");
  const linha = tbody.insertRow(0);

  // Coluna 1: Data e Hora completa do navegador
  const agora = new Date();
  const timestamp = `${agora.getFullYear()}-${String(agora.getMonth() + 1).padStart(2, '0')}-${String(agora.getDate()).padStart(2, '0')} ${agora.toLocaleTimeString('pt-BR')}.${String(agora.getMilliseconds()).padStart(3, '0')}`;
  linha.insertCell(0).innerText = timestamp;

  // Coluna 2: Tempo do microcontrolador
  linha.insertCell(1).innerText = espTime;

  // Coluna 3: Força em Newtons (o dado que recebemos)
  linha.insertCell(2).innerText = forceN.toFixed(2);

  // Coluna 4 e 5: Cálculos feitos a partir dos Newtons
  let massaKg = 0;
  if (gravity && gravity > 0) { // Cálculo seguro para evitar divisão por zero
    massaKg = forceN / gravity;
  }
  linha.insertCell(3).innerText = (massaKg * 1000).toFixed(0); // Grama-força
  linha.insertCell(4).innerText = massaKg.toFixed(3);        // Quilo-força

  // Limita o número de linhas na tabela
  if (tbody.rows.length > 200) {
    tbody.deleteRow(200);
  }
}

function updateConfigForm(config) {
  const getValue = (val) => (val != null ? val : '');
  document.getElementById("ssid").value = getValue(config.ssid);
  document.getElementById("senha").value = getValue(config.password);
  document.getElementById("param-conversao").value = getValue(config.conversionFactor);
  document.getElementById("param-gravidade").value = getValue(config.gravity);
  document.getElementById("param-leituras-estaveis").value = getValue(config.leiturasEstaveis);
  document.getElementById("param-tolerancia").value = getValue(config.toleranciaEstabilidade);
  document.getElementById("param-num-amostras").value = getValue(config.numAmostrasMedia);
  document.getElementById("param-timeout").value = getValue(config.timeoutCalibracao);
  document.getElementById("param-offset").textContent = getValue(config.tareOffset);
  // LINHA A SER ADICIONADA
  if(config.gravity) { gravity = parseFloat(config.gravity); }
}

function sendCommand(cmd) {
  if (socket && socket.readyState === WebSocket.OPEN) {
    socket.send(cmd);
  } else {
    showNotification("error", "Não conectado. Impossível enviar comando.");
  }
}

function tare() { sendCommand("t"); }

function calibrar() {
  const massaInput = document.getElementById("massaCalibracao");
  const massa = parseFloat(massaInput.value);
  if (massa > 0) {
    sendCommand("c:" + massa);
  } else {
    showNotification("error", "Por favor, informe uma massa válida e positiva.");
  }
}

function salvarParametros() {
  const params = {
    conversionFactor: document.getElementById("param-conversao").value,
    gravity: document.getElementById("param-gravidade").value,
    leiturasEstaveis: document.getElementById("param-leituras-estaveis").value,
    toleranciaEstabilidade: document.getElementById("param-tolerancia").value,
    numAmostrasMedia: document.getElementById("param-num-amostras").value,
    timeoutCalibracao: document.getElementById("param-timeout").value,
  };
  for (const [key, value] of Object.entries(params)) {
    if (value) {
      sendCommand(`set_param:${key}:${value}`);
    }
  }
}

function salvarRede(event) {
  event.preventDefault();
  const form = new FormData(event.target);
  fetch("/salvarRede", { method: "POST", body: new URLSearchParams(form) })
    .then(r => r.text()).then(text => showNotification("success", text));
}

function setDisplayUnit(unit) {
    displayUnit = unit;
    
    const unitButtons = document.querySelectorAll('#abaGrafico .btn-group:first-of-type button');
    unitButtons.forEach(b => {
        b.classList.remove('bg-blue-500', 'text-white');
        b.classList.add('bg-white', 'text-gray-900');
    });
    const activeButton = document.getElementById(`btn-unit-${unit}`);
    if(activeButton) {
        activeButton.classList.remove('bg-white', 'text-gray-900');
        activeButton.classList.add('bg-blue-500', 'text-white');
    }

    chart.options.scales.y.title.text = `Força (${unit})`;
    chart.data.datasets[0].label = `Força (${unit})`;
    document.getElementById('tabela-forca-header').textContent = `Força (${unit})`;

    // Reconverte e reformata todos os dados existentes no gráfico
    chart.data.datasets[0].data = rawDataN.map(forceN => formatForce(convertForce(forceN, unit), unit));
    chart.update();

    // Atualiza os painéis com a nova formatação
    const maxDisplayForce = convertForce(maxForceInN, displayUnit);
    document.getElementById('forca-maxima').textContent = `${formatForce(maxDisplayForce, displayUnit)} ${displayUnit}`;
    const currentForceN = rawDataN.length > 0 ? rawDataN[rawDataN.length-1] : 0;
    const currentDisplayForce = convertForce(currentForceN, displayUnit);
    document.getElementById('forca-atual').textContent = `${formatForce(currentDisplayForce, displayUnit)} ${displayUnit}`;
}

function setChartMode(mode) {
    chartMode = mode;
    const modeButtons = document.querySelectorAll('#abaGrafico .flex-col .btn-group button');
    modeButtons.forEach(b => {
        b.classList.remove('bg-blue-500', 'text-white');
        b.classList.add('bg-white', 'text-gray-900');
    });
    const activeButton = document.getElementById(`btn-${mode}`);
    if (activeButton) {
        activeButton.classList.remove('bg-white', 'text-gray-900');
        activeButton.classList.add('bg-blue-500', 'text-white');
    }
}
// Função clearChart modificada para não limpar a tabela, apenas o gráfico.
function clearChart() {
    chart.data.labels = [];
    chart.data.datasets[0].data = [];
    rawDataN = [];
    maxForceInN = -Infinity;
    document.getElementById('forca-atual').textContent = `--- ${displayUnit}`;
    document.getElementById('forca-maxima').textContent = `--- ${displayUnit}`;
    chart.resetZoom();
    chart.update();
    showNotification("info", "Dados da sessão atual limpos.", 2000);
}


function resetChartZoom() {
    chart.resetZoom();
}

function abrirAba(element, abaID) {
  document.querySelectorAll('.tabcontent').forEach(tab => tab.style.display = "none");
  document.querySelectorAll('.tablink').forEach(link => link.classList.remove('active', 'text-blue-600', 'border-blue-600'));
  document.getElementById(abaID).style.display = "block";
  element.classList.add('active', 'text-blue-600', 'border-blue-600');
}

function updateConnectionStatus(isConnected) {
  const indicator = document.getElementById('ws-indicator');
  const text = document.getElementById('ws-text');
  if (isConnected) {
    indicator.classList.replace('bg-red-500', 'bg-green-500');
    text.textContent = "Conectado";
  } else {
    indicator.classList.replace('bg-green-500', 'bg-red-500');
    text.textContent = "Desconectado";
  }
}

function showNotification(type, message, duration = 7000) {
    const area = document.getElementById('notification-area');
    const color = { success: 'green', error: 'red', info: 'blue' }[type] || 'blue';
    const notification = document.createElement('div');
    notification.className = `bg-${color}-100 border-l-4 border-${color}-500 text-${color}-700 p-4 mb-2 rounded-md shadow`;
    notification.innerHTML = `<p class="font-bold">${type.charAt(0).toUpperCase() + type.slice(1)}</p><p>${message}</p>`;
    area.prepend(notification);
    if(duration > 0) setTimeout(() => {
        notification.style.transition = 'opacity 0.5s';
        notification.style.opacity = '0';
        setTimeout(() => notification.remove(), 500);
    }, duration);
}


// =======================================================
// --- NOVAS FUNÇÕES PARA GRAVAÇÃO ---
// =======================================================


/**
 * Salva a sessão atual (gráfico e tabela) no Local Storage do navegador.
 */
/**
 * Salva a sessão atual (gráfico e tabela) no Local Storage do navegador.
 */
function salvarGravacao() {
    // Pega o nome do campo de input na página
    const nomeInput = document.getElementById('nome-gravacao');
    const nome = nomeInput.value.trim(); // Pega o valor e remove espaços em branco

    // Se o usuário não digitou nada, avisa e para a função.
    if (!nome) {
        showNotification('error', 'Por favor, digite um nome para a gravação.', 3000);
        nomeInput.focus(); // Coloca o cursor no campo de nome
        return;
    }

    const dadosTabela = [];
    const tabela = document.getElementById("tabela").querySelector("tbody");
    
    if (tabela.rows.length === 0) {
        showNotification('error', 'Não há dados na tabela para salvar.', 3000);
        return;
    }

    for (const linha of tabela.rows) {
        dadosTabela.push({
            timestamp: linha.cells[0].innerText,
            tempo_esp: linha.cells[1].innerText,
            newtons: linha.cells[2].innerText,
            grama_forca: linha.cells[3].innerText,
            quilo_forca: linha.cells[4].innerText,
        });
    }

    const imagemGrafico = chart.toBase64Image();

    // Adiciona o nome ao objeto da gravação
    const gravacao = {
        id: Date.now(),
        nome: nome,
        timestamp: new Date().toISOString(),
        dadosTabela: dadosTabela.reverse(),
        imagemGrafico: imagemGrafico
    };

    try {
        let gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
        gravacoes.push(gravacao);
        localStorage.setItem('balancaGravacoes', JSON.stringify(gravacoes));
        showNotification('success', `Gravação "${nome}" salva com sucesso!`);
        carregarGravacoes();
        nomeInput.value = ''; // Limpa o campo de nome após salvar
    } catch (e) {
        console.error("Erro ao salvar no Local Storage:", e);
        showNotification('error', 'Erro ao salvar. O Local Storage pode estar cheio.');
    }
}

/**
 * Lê as gravações do Local Storage e as exibe na aba "Gravações".
 */
function carregarGravacoes() {
    const container = document.getElementById('lista-gravacoes');
    const gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
    container.innerHTML = '';

    if (gravacoes.length === 0) {
        container.innerHTML = '<p class="text-gray-500">Nenhuma gravação encontrada.</p>';
        return;
    }
    
    gravacoes.reverse().forEach(gravacao => {
        const dataFormatada = new Date(gravacao.timestamp).toLocaleString('pt-BR');
        const card = document.createElement('div');
        card.className = 'flex flex-col sm:flex-row items-center justify-between p-4 border rounded-lg';
        // Usa o nome salvo na gravação
        card.innerHTML = `
            <div class="flex-grow mb-2 sm:mb-0">
                <p class="font-semibold">${gravacao.nome}</p> 
                <p class="text-sm text-gray-600">Salvo em: ${dataFormatada} (${gravacao.dadosTabela.length} leituras)</p>
            </div>
            <div class="flex-shrink-0 flex gap-2">
                <button onclick="exportarCSV(${gravacao.id})" class="px-3 py-2 text-sm font-medium text-white bg-green-500 rounded-md hover:bg-green-600">Exportar CSV</button>
                <button onclick="salvarImagemGrafico(${gravacao.id})" class="px-3 py-2 text-sm font-medium text-white bg-blue-500 rounded-md hover:bg-blue-600">Salvar Gráfico</button>
                <button onclick="deletarGravacao(${gravacao.id})" class="px-3 py-2 text-sm font-medium text-white bg-red-500 rounded-md hover:bg-red-600">Deletar</button>
            </div>
        `;
        container.appendChild(card);
    });
}
/**
 * Exporta os dados de uma gravação específica para um arquivo CSV.
 * @param {number} id O ID da gravação a ser exportada.
 */
function exportarCSV(id) {
    const gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
    const gravacao = gravacoes.find(g => g.id === id);

    if (!gravacao) {
        showNotification('error', 'Gravação não encontrada.');
        return;
    }

    // Cria o cabeçalho e as linhas do CSV
    const cabecalho = Object.keys(gravacao.dadosTabela[0]).join(',');
    const linhas = gravacao.dadosTabela.map(linha => Object.values(linha).join(','));
    const conteudoCSV = `${cabecalho}\n${linhas.join('\n')}`;

    // Cria e dispara o download
    const blob = new Blob([conteudoCSV], { type: 'text/csv;charset=utf-8;' });
    const link = document.createElement('a');
    link.href = URL.createObjectURL(blob);
    link.setAttribute('download', `gravacao_balanca_${id}.csv`);
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
}

/**
 * Inicia o download da imagem do gráfico de uma gravação.
 * @param {number} id O ID da gravação.
 */
function salvarImagemGrafico(id) {
    const gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
    const gravacao = gravacoes.find(g => g.id === id);

    if (!gravacao) {
        showNotification('error', 'Gravação não encontrada.');
        return;
    }

    const link = document.createElement('a');
    link.href = gravacao.imagemGrafico;
    link.setAttribute('download', `grafico_balanca_${id}.png`);
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
}

/**
 * Deleta uma gravação específica do Local Storage.
 * @param {number} id O ID da gravação a ser deletada.
 */
function deletarGravacao(id) {
    if (!confirm('Tem certeza que deseja deletar esta gravação? Esta ação não pode ser desfeita.')) {
        return;
    }
    
    let gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
    const novasGravacoes = gravacoes.filter(g => g.id !== id);
    localStorage.setItem('balancaGravacoes', JSON.stringify(novasGravacoes));
    
    showNotification('info', 'Gravação deletada.');
    carregarGravacoes(); // Atualiza a lista
}
