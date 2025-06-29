const socket = new WebSocket("ws://" + location.hostname + ":81/");
let chart = null;
let tempo = [], forca = [];

function tare() {
  socket.send("t");
}

function calibrar() {
  const massa = prompt("Coloque uma massa conhecida na balança e informe o valor em gramas:");
  if (massa && !isNaN(massa) && parseFloat(massa) > 0) {
    socket.send("c:" + massa);
  } else {
    alert("Valor inválido.");
  }
}

function criarGrafico() {
  const ctx = document.getElementById("grafico").getContext("2d");
  chart = new Chart(ctx, {
    type: 'line',
    data: {
      labels: tempo,
      datasets: [{
        label: 'Força (g)',
        data: forca,
        borderWidth: 2,
        borderColor: 'blue',
        tension: 0.1
      }]
    },
    options: {
      animation: false,
      responsive: true,
      scales: {
        x: { title: { display: true, text: "Tempo (s)" } },
        y: { title: { display: true, text: "Força (g)" } }
      }
    }
  });
}

socket.onmessage = function (event) {
  try {
    const dados = JSON.parse(event.data);
    tempo.push(dados.tempo);
    forca.push(dados.forca);

    if (tempo.length > 100) {
      tempo.shift();
      forca.shift();
    }

    chart.update();
  } catch (e) {
    console.warn("Erro ao processar dados:", e);
  }
};

window.onload = criarGrafico;
