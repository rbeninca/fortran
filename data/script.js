let chart;
let socket;
const MAX_DATA_POINTS = 100;
let chartMode = 'sliding'; // 'sliding', 'continuous', 'paused'

window.onload = () => {
  abrirAba(document.getElementById("padrao"), 'abaGrafico');
  const ctx = document.getElementById("grafico").getContext("2d");
  chart = new Chart(ctx, {
    type: 'line',
    data: { labels: [], datasets: [{ label: 'Força (g)', data: [], borderColor: '#3b82f6', backgroundColor: 'rgba(59, 130, 246, 0.1)', borderWidth: 2, fill: true, tension: 0.4 }] },
    options: { animation: false, responsive: true, scales: { x: { title: { display: true, text: 'Tempo (s)' } }, y: { title: { display: true, text: 'Força (g)' } } } }
  });
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
      showNotification("error", "Falha ao ler dados do dispositivo. Formato inválido.", 10000);
    }
  };
}

function updateUI(dado) {
  document.getElementById('balanca-status').textContent = dado.status;
  if (chartMode === 'paused') return;
  const { labels, datasets } = chart.data;
  labels.push(dado.tempo.toFixed(2));
  datasets[0].data.push(dado.forca.toFixed(3));
  if (chartMode === 'sliding' && labels.length > MAX_DATA_POINTS) {
    labels.shift();
    datasets[0].data.shift();
  }
  chart.update();
  const tbody = document.getElementById("tabela").querySelector("tbody");
  const linha = tbody.insertRow(0);
  linha.insertCell(0).innerText = dado.tempo.toFixed(2);
  linha.insertCell(1).innerText = dado.forca.toFixed(3);
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

function setChartMode(mode) {
    chartMode = mode;
    document.querySelectorAll('.btn-group button.active').forEach(b => b.classList.remove('active'));
    document.getElementById(`btn-${mode}`).classList.add('active');
    if(mode === 'sliding' && chart.data.labels.length > MAX_DATA_POINTS) {
        chart.data.labels.splice(0, chart.data.labels.length - MAX_DATA_POINTS);
        chart.data.datasets[0].data.splice(0, chart.data.datasets[0].data.length - MAX_DATA_POINTS);
        chart.update();
    }
}

function clearChart() {
    chart.data.labels = [];
    chart.data.datasets[0].data = [];
    chart.update();
    showNotification("info", "Gráfico limpo.", 2000);
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
