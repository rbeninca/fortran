let chart;
let socket;
const MAX_DATA_POINTS = 100; // Limita os pontos no gráfico para melhor performance

/**
 * Função de inicialização, chamada quando a página carrega.
 */
window.onload = () => {
  // Ativa a primeira aba por padrão
  const defaultTab = document.getElementById("padrao");
  abrirAba(defaultTab, 'abaGrafico');

  // Configura o gráfico
  const ctx = document.getElementById("grafico").getContext("2d");
  chart = new Chart(ctx, {
    type: 'line',
    data: {
      labels: [],
      datasets: [{
        label: 'Força (g)',
        data: [],
        borderColor: '#3b82f6', // blue-500
        backgroundColor: 'rgba(59, 130, 246, 0.1)',
        borderWidth: 2,
        fill: true,
        tension: 0.4 // Deixa a linha mais suave
      }]
    },
    options: {
      animation: false,
      responsive: true,
      maintainAspectRatio: true,
      scales: {
        x: {
          title: { display: true, text: 'Tempo (s)' }
        },
        y: {
          title: { display: true, text: 'Força (g)' }
        }
      }
    }
  });

  // Inicia a conexão WebSocket
  conectarWebSocket();
};

/**
 * Gerencia a conexão WebSocket, incluindo reconexão automática.
 */
function conectarWebSocket() {
  // Constrói a URL do WebSocket a partir da localização da página
  const wsURL = "ws://" + location.hostname + ":81";
  console.log(`Conectando a ${wsURL}`);
  socket = new WebSocket(wsURL);

  socket.onopen = () => {
    console.log("WebSocket conectado!");
    updateStatus(true);
  };

  socket.onclose = () => {
    console.log("WebSocket desconectado. Tentando reconectar em 3 segundos...");
    updateStatus(false);
    // Tenta reconectar após um breve período
    setTimeout(conectarWebSocket, 3000);
  };

  socket.onerror = (error) => {
    console.error("Erro no WebSocket:", error);
    updateStatus(false);
  };

  // Processa as mensagens recebidas do ESP8266
  socket.onmessage = (event) => {
    try {
      const data = JSON.parse(event.data);

      // Verifica o tipo de mensagem: dados da balança ou status
      if (data.type === "data") {
        updateUI(data);
      } else if (data.type === "status") {
        showNotification(data.status, data.message);
      }
    } catch (e) {
      console.error("Erro ao processar JSON:", e, event.data);
    }
  };
}

/**
 * Atualiza o gráfico e a tabela com novos dados da balança.
 * @param {object} dado - O objeto de dados com 'tempo' e 'forca'.
 */
function updateUI(dado) {
  const { labels, datasets } = chart.data;
  
  // Adiciona novos dados
  labels.push(dado.tempo.toFixed(2));
  datasets[0].data.push(dado.forca.toFixed(3));

  // Remove dados antigos para manter o gráfico legível
  if (labels.length > MAX_DATA_POINTS) {
    labels.shift();
    datasets[0].data.shift();
  }
  chart.update();

  // Adiciona a nova leitura no topo da tabela
  const tbody = document.getElementById("tabela").querySelector("tbody");
  let linha = tbody.insertRow(0); // Insere no topo
  linha.insertCell(0).innerText = dado.tempo.toFixed(2);
  linha.insertCell(1).innerText = dado.forca.toFixed(3);

  // Limita o tamanho da tabela
  if (tbody.rows.length > MAX_DATA_POINTS) {
    tbody.deleteRow(MAX_DATA_POINTS);
  }
}

/**
 * Envia o comando de tara (zerar) para o WebSocket.
 */
function tare() {
  if (socket && socket.readyState === WebSocket.OPEN) {
    socket.send("t");
  } else {
    showNotification("error", "Não conectado. Impossível enviar comando.");
  }
}

/**
 * Envia o comando de calibração para o WebSocket.
 */
function calibrar() {
  if (socket && socket.readyState === WebSocket.OPEN) {
    const massaInput = document.getElementById("massaCalibracao");
    const massa = parseFloat(massaInput.value);
    if (massa > 0) {
      socket.send("c:" + massa);
    } else {
      showNotification("error", "Por favor, informe uma massa válida e positiva.");
    }
  } else {
    showNotification("error", "Não conectado. Impossível enviar comando.");
  }
}

/**
 * Envia as novas credenciais de Wi-Fi para o ESP8266.
 * @param {Event} event - O evento do formulário.
 */
function salvarRede(event) {
  event.preventDefault();
  const ssid = document.getElementById("ssid").value;
  const senha = document.getElementById("senha").value;

  fetch("/salvarRede", {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: `ssid=${encodeURIComponent(ssid)}&senha=${encodeURIComponent(senha)}`
  })
  .then(response => response.text())
  .then(text => {
    showNotification("success", text);
  })
  .catch(error => {
    console.error("Erro ao salvar rede:", error);
    showNotification("error", "Falha ao enviar configuração de rede.");
  });
}

/**
 * Controla a exibição das abas.
 * @param {HTMLElement} element - O elemento do botão da aba clicado.
 * @param {string} abaID - O ID do conteúdo da aba a ser exibida.
 */
function abrirAba(element, abaID) {
  // Esconde todos os conteúdos
  document.querySelectorAll('.tabcontent').forEach(tab => tab.style.display = "none");
  // Remove a classe 'active' de todos os links
  document.querySelectorAll('.tablink').forEach(link => link.classList.remove('active'));
  // Mostra o conteúdo da aba selecionada e ativa o link
  document.getElementById(abaID).style.display = "block";
  element.classList.add('active');
}

/**
 * Atualiza o indicador de status da conexão visual.
 * @param {boolean} isConnected - True se conectado, false caso contrário.
 */
function updateStatus(isConnected) {
  const indicator = document.getElementById('status-indicator');
  const text = document.getElementById('status-text');
  if (isConnected) {
    indicator.classList.remove('bg-red-500');
    indicator.classList.add('bg-green-500');
    indicator.title = "Conectado";
    text.textContent = "Conectado";
  } else {
    indicator.classList.remove('bg-green-500');
    indicator.classList.add('bg-red-500');
    indicator.title = "Desconectado";
    text.textContent = "Desconectado";
  }
}

/**
 * Exibe uma notificação na tela.
 * @param {string} type - Tipo de notificação ('success', 'error', 'info').
 * @param {string} message - A mensagem a ser exibida.
 */
function showNotification(type, message) {
    const area = document.getElementById('notification-area');
    const colorClasses = {
        success: 'bg-green-100 border-green-500 text-green-700',
        error: 'bg-red-100 border-red-500 text-red-700',
        info: 'bg-blue-100 border-blue-500 text-blue-700'
    };
    
    const notification = document.createElement('div');
    notification.className = `border-l-4 p-4 mb-2 rounded-md shadow ${colorClasses[type] || colorClasses['info']}`;
    notification.setAttribute('role', 'alert');
    notification.innerHTML = `<p class="font-bold">${type.charAt(0).toUpperCase() + type.slice(1)}</p><p>${message}</p>`;

    area.prepend(notification);

    // Remove a notificação após 7 segundos
    setTimeout(() => {
        notification.style.transition = 'opacity 0.5s ease';
        notification.style.opacity = '0';
        setTimeout(() => notification.remove(), 500);
    }, 7000);
}
