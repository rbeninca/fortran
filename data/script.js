let chart;
let socket;
const MAX_DATA_POINTS = 100;
let chartMode = 'deslizante'; // 'deslizante', 'acumulado', 'pausado'
let displayUnit = 'N'; // Unidade de exibição padrão: 'N', 'gf', 'kgf'
let maxForceInN = -Infinity; // Guarda sempre a força máxima na unidade base (Newtons)
let rawDataN = []; // Guarda os dados brutos em Newtons para conversão

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
  setChartMode('deslizante'); // Garante que o estado inicial do botão corresponda à variável
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

// --- FUNÇÃO PARA CONVERTER VALORES DE FORÇA ---
function convertForce(valueN, unit) {
    if (unit === 'gf') return valueN * 101.97;
    if (unit === 'kgf') return valueN * 0.10197;
    return valueN; // Retorna em Newtons por padrão
}

function updateUI(dado) {
  document.getElementById('balanca-status').textContent = dado.status;
  
  const forceN = dado.forca;
  rawDataN.push(forceN); // Armazena o dado bruto em Newtons
  
  // Atualiza a força máxima (sempre comparando em N)
  if (forceN > maxForceInN) {
      maxForceInN = forceN;
  }
  
  // Converte os valores para a unidade de exibição selecionada
  const displayForce = convertForce(forceN, displayUnit);
  const maxDisplayForce = convertForce(maxForceInN, displayUnit);

  document.getElementById('forca-atual').textContent = `${displayForce.toFixed(4)} ${displayUnit}`;
  document.getElementById('forca-maxima').textContent = `${maxDisplayForce.toFixed(4)} ${displayUnit}`;

  if (chartMode === 'pausado') return;

  const { labels, datasets } = chart.data;
  
  const espTime = dado.tempo.toFixed(2);
  labels.push(espTime);
  datasets[0].data.push(displayForce.toFixed(4));

  // Remove dados antigos
  if (rawDataN.length > MAX_DATA_POINTS && chartMode === 'deslizante') {
    rawDataN.shift();
    labels.shift();
    datasets[0].data.shift();
  }
  
  if (chartMode === 'acumulado') {
    chart.update('none'); 
  } else {
    chart.update();
  }

  const tbody = document.getElementById("tabela").querySelector("tbody");
  const linha = tbody.insertRow(0);

  const timestamp = new Date().toLocaleTimeString('pt-BR');

  linha.insertCell(0).innerText = timestamp;
  linha.insertCell(1).innerText = espTime;
  linha.insertCell(2).innerText = displayForce.toFixed(4);

  if (tbody.rows.length > 200) tbody.deleteRow(200);
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
  const massa = parseFloat(massaInput.value).toFixed(5);
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
    
    // Atualiza os estilos dos botões de unidade
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

    // Atualiza o título do eixo Y e da tabela
    chart.options.scales.y.title.text = `Força (${unit})`;
    chart.data.datasets[0].label = `Força (${unit})`;
    document.getElementById('tabela-forca-header').textContent = `Força (${unit})`;

    // Reconverte todos os dados existentes no gráfico
    chart.data.datasets[0].data = rawDataN.map(forceN => convertForce(forceN, unit));
    chart.update();

    // Atualiza os painéis
    const maxDisplayForce = convertForce(maxForceInN, displayUnit);
    document.getElementById('forca-maxima').textContent = `${maxDisplayForce.toFixed(4)} ${displayUnit}`;
    const currentForceN = rawDataN.length > 0 ? rawDataN[rawDataN.length-1] : 0;
    const currentDisplayForce = convertForce(currentForceN, displayUnit);
    document.getElementById('forca-atual').textContent = `${currentDisplayForce.toFixed(4)} ${displayUnit}`;
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

function clearChart() {
    chart.data.labels = [];
    chart.data.datasets[0].data = [];
    rawDataN = [];
    chart.update();

    maxForceInN = -Infinity;
    document.getElementById('forca-atual').textContent = `--- ${displayUnit}`;
    document.getElementById('forca-maxima').textContent = `--- ${displayUnit}`;

    showNotification("info", "Gráfico limpo.", 2000);
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
