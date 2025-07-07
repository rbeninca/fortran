// --- VERSÃO SIMPLES E ROBUSTA PARA EXPORTAÇÃO DE SESSÃO ---

function exportarImagemSessao(sessionId) {
  try {
    // Busca a sessão
    const gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
    const sessao = gravacoes.find(g => g.id === sessionId);
    
    if (!sessao || !sessao.dadosTabela || sessao.dadosTabela.length === 0) {
      showNotification('error', 'Sessão não encontrada ou sem dados');
      return;
    }
    
    showNotification('info', `Gerando relatório de "${sessao.nome}"...`, 2000);
    
    // Processa dados
    const dados = processarDadosSimples(sessao.dadosTabela);
    
    // Gera imagem
    criarRelatorioSimples(sessao, dados);
    
  } catch (e) {
    console.error('Erro:', e);
    showNotification('error', 'Erro ao gerar relatório: ' + e.message);
  }
}

function processarDadosSimples(dadosTabela) {
  const tempos = [];
  const newtons = [];
  const kgf = [];
  
  dadosTabela.forEach(linha => {
    tempos.push(parseFloat(linha.tempo_esp) || 0);
    newtons.push(parseFloat(linha.newtons) || 0);
    kgf.push(parseFloat(linha.quilo_forca) || 0);
  });
  
  // Estatísticas básicas
  const calcStats = (arr) => {
    if (arr.length === 0) return null;
    const soma = arr.reduce((a, b) => a + b, 0);
    const media = soma / arr.length;
    const max = Math.max(...arr);
    const min = Math.min(...arr);
    const ordenado = [...arr].sort((a, b) => a - b);
    const mediana = arr.length % 2 === 0 
      ? (ordenado[Math.floor(arr.length/2) - 1] + ordenado[Math.floor(arr.length/2)]) / 2
      : ordenado[Math.floor(arr.length/2)];
    
    const variancia = arr.reduce((acc, val) => acc + Math.pow(val - media, 2), 0) / arr.length;
    const desvio = Math.sqrt(variancia);
    const cv = media !== 0 ? (desvio / Math.abs(media)) * 100 : 0;
    
    return { media, max, min, mediana, desvio, cv, amplitude: max - min };
  };
  
  return {
    tempos,
    newtons,
    kgf,
    stats: calcStats(kgf),
    duracao: tempos.length > 0 ? Math.max(...tempos) - Math.min(...tempos) : 0,
    pontos: tempos.length
  };
}

function criarRelatorioSimples(sessao, dados) {
  const canvas = document.createElement('canvas');
  const ctx = canvas.getContext('2d');
  
  // Configurações
  const w = 1400;
  const h = 1000;
  canvas.width = w;
  canvas.height = h;
  
  // Cores
  const cor = {
    fundo: '#ffffff',
    titulo: '#2c3e50',
    subtitulo: '#7f8c8d',
    azul: '#3498db',
    verde: '#27ae60',
    vermelho: '#e74c3c',
    cinza: '#95a5a6',
    fundo2: '#f8f9fa'
  };
  
  // 1. FUNDO
  ctx.fillStyle = cor.fundo;
  ctx.fillRect(0, 0, w, h);
  
  // 2. CABEÇALHO
  ctx.fillStyle = cor.titulo;
  ctx.font = 'bold 32px Arial';
  ctx.textAlign = 'center';
  ctx.fillText('📊 RELATÓRIO DA SESSÃO', w/2, 50);
  
  ctx.fillStyle = cor.azul;
  ctx.font = 'bold 22px Arial';
  ctx.fillText(`"${sessao.nome}"`, w/2, 85);
  
  const dataSessao = new Date(sessao.timestamp).toLocaleString('pt-BR');
  ctx.fillStyle = cor.subtitulo;
  ctx.font = '16px Arial';
  ctx.fillText(`Gravada em: ${dataSessao}`, w/2, 115);
  
  // Info básica
  ctx.font = 'bold 14px Arial';
  ctx.fillText(`${dados.pontos} pontos • ${dados.duracao.toFixed(1)}s de duração`, w/2, 140);
  
  // Linha
  ctx.strokeStyle = cor.cinza;
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.moveTo(w*0.2, 160);
  ctx.lineTo(w*0.8, 160);
  ctx.stroke();
  
  // 3. GRÁFICO
  if (dados.kgf.length > 0) {
    desenharGraficoSimples(ctx, dados, cor, w, h);
  }
  
  // 4. ESTATÍSTICAS
  if (dados.stats) {
    desenharEstatisticasSimples(ctx, dados.stats, cor, w, h);
  }
  
  // 5. RODAPÉ
  ctx.fillStyle = cor.subtitulo;
  ctx.font = '12px Arial';
  ctx.textAlign = 'left';
  ctx.fillText('Sistema de Balança Digital', 50, h-20);
  ctx.textAlign = 'right';
  ctx.fillText(`Gerado em: ${new Date().toLocaleString('pt-BR')}`, w-50, h-20);
  
  // 6. DOWNLOAD
  baixarRelatorio(canvas, sessao.nome);
}

function desenharGraficoSimples(ctx, dados, cor, w, h) {
  // Área do gráfico
  const gx = 100;
  const gy = 200;
  const gw = w - 200;
  const gh = 350;
  
  // Fundo
  ctx.fillStyle = cor.fundo2;
  ctx.fillRect(gx, gy, gw, gh);
  
  // Borda
  ctx.strokeStyle = cor.cinza;
  ctx.lineWidth = 2;
  ctx.strokeRect(gx, gy, gw, gh);
  
  const valores = dados.kgf;
  const tempos = dados.tempos;
  
  if (valores.length === 0) return;
  
  // Limites
  const maxVal = Math.max(...valores);
  const minVal = Math.min(...valores);
  const range = maxVal - minVal || 0.001;
  const padding = range * 0.1;
  
  const yMin = minVal - padding;
  const yMax = maxVal + padding;
  const yRange = yMax - yMin;
  
  // Grid horizontal
  ctx.strokeStyle = '#ecf0f1';
  ctx.lineWidth = 1;
  ctx.setLineDash([3, 3]);
  
  for (let i = 0; i <= 5; i++) {
    const y = gy + (gh/5) * i;
    const valor = yMax - (yRange/5) * i;
    
    ctx.beginPath();
    ctx.moveTo(gx, y);
    ctx.lineTo(gx + gw, y);
    ctx.stroke();
    
    // Label
    ctx.fillStyle = cor.cinza;
    ctx.font = '11px Arial';
    ctx.textAlign = 'right';
    ctx.fillText(valor.toFixed(3), gx - 10, y + 3);
  }
  
  ctx.setLineDash([]);
  
  // Linha dos dados
  if (valores.length > 1) {
    ctx.strokeStyle = cor.azul;
    ctx.lineWidth = 3;
    ctx.beginPath();
    
    for (let i = 0; i < valores.length; i++) {
      const x = gx + (gw / (valores.length - 1)) * i;
      const y = gy + gh - ((valores[i] - yMin) / yRange) * gh;
      
      if (i === 0) {
        ctx.moveTo(x, y);
      } else {
        ctx.lineTo(x, y);
      }
    }
    ctx.stroke();
  }
  
  // Pontos
  ctx.fillStyle = cor.azul;
  for (let i = 0; i < valores.length; i++) {
    const x = gx + (gw / Math.max(1, valores.length - 1)) * i;
    const y = gy + gh - ((valores[i] - yMin) / yRange) * gh;
    
    ctx.beginPath();
    ctx.arc(x, y, 2, 0, 2 * Math.PI);
    ctx.fill();
  }
  
  // Labels dos eixos
  ctx.fillStyle = cor.titulo;
  ctx.font = 'bold 14px Arial';
  ctx.textAlign = 'center';
  
  // Eixo Y (sem rotação para simplicidade)
  ctx.textAlign = 'center';
  ctx.fillText('Força (kgf)', 50, gy + gh/2);
  
  // Eixo X
  ctx.fillText('Tempo (s)', gx + gw/2, gy + gh + 40);
  
  // Labels do tempo (alguns pontos)
  ctx.fillStyle = cor.cinza;
  ctx.font = '10px Arial';
  const step = Math.max(1, Math.floor(valores.length / 8));
  for (let i = 0; i < valores.length; i += step) {
    const x = gx + (gw / Math.max(1, valores.length - 1)) * i;
    ctx.fillText(tempos[i].toFixed(1), x, gy + gh + 15);
  }
}

function desenharEstatisticasSimples(ctx, stats, cor, w, h) {
  const sy = 600;
  
  // Título
  ctx.fillStyle = cor.titulo;
  ctx.font = 'bold 18px Arial';
  ctx.textAlign = 'left';
  ctx.fillText('📈 ESTATÍSTICAS', 100, sy);
  
  // Caixa
  const bx = 100;
  const by = sy + 20;
  const bw = w - 200;
  const bh = 120;
  
  ctx.fillStyle = cor.fundo2;
  ctx.fillRect(bx, by, bw, bh);
  
  ctx.strokeStyle = cor.azul;
  ctx.lineWidth = 2;
  ctx.strokeRect(bx, by, bw, bh);
  
  // Estatísticas em colunas
  ctx.fillStyle = cor.titulo;
  ctx.font = '14px Arial';
  
  const estatisticas = [
    [`Média: ${stats.media.toFixed(4)} kgf`, `Máximo: ${stats.max.toFixed(4)} kgf`],
    [`Mínimo: ${stats.min.toFixed(4)} kgf`, `Mediana: ${stats.mediana.toFixed(4)} kgf`],
    [`Desvio Padrão: ${stats.desvio.toFixed(4)} kgf`, `Coef. Variação: ${stats.cv.toFixed(1)}%`],
    [`Amplitude: ${stats.amplitude.toFixed(4)} kgf`, `Qualidade: ${getQualidadeSinal(stats.cv)}`]
  ];
  
  estatisticas.forEach((linha, i) => {
    ctx.fillText(linha[0], bx + 20, by + 25 + i * 20);
    ctx.fillText(linha[1], bx + bw/2 + 20, by + 25 + i * 20);
  });
}

function getQualidadeSinal(cv) {
  if (cv < 5) return 'Excelente';
  if (cv < 15) return 'Boa';
  if (cv < 30) return 'Regular';
  return 'Ruidosa';
}

function baixarRelatorio(canvas, nomeSessao) {
  try {
    canvas.toBlob(function(blob) {
      if (!blob) {
        showNotification('error', 'Erro ao criar arquivo');
        return;
      }
      
      const url = URL.createObjectURL(blob);
      const link = document.createElement('a');
      
      // Nome sanitizado
      const nome = nomeSessao.replace(/[^a-zA-Z0-9\s]/g, '').replace(/\s+/g, '_');
      link.download = `relatorio_${nome}_${Date.now()}.png`;
      link.href = url;
      
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
      
      setTimeout(() => URL.revokeObjectURL(url), 1000);
      showNotification('success', `Relatório de "${nomeSessao}" exportado!`);
      
    }, 'image/png', 1.0);
    
  } catch (e) {
    console.error('Erro no download:', e);
    showNotification('error', 'Erro ao baixar: ' + e.message);
  }
}

// VERSÃO ALTERNATIVA AINDA MAIS SIMPLES
function exportarResumoSessao(sessionId) {
  try {
    const gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
    const sessao = gravacoes.find(g => g.id === sessionId);
    
    if (!sessao) {
      showNotification('error', 'Sessão não encontrada');
      return;
    }
    
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');
    
    canvas.width = 800;
    canvas.height = 600;
    
    // Fundo branco
    ctx.fillStyle = '#ffffff';
    ctx.fillRect(0, 0, 800, 600);
    
    // Título
    ctx.fillStyle = '#2c3e50';
    ctx.font = 'bold 24px Arial';
    ctx.textAlign = 'center';
    ctx.fillText('Relatório da Sessão', 400, 50);
    
    ctx.font = 'bold 18px Arial';
    ctx.fillStyle = '#3498db';
    ctx.fillText(sessao.nome, 400, 80);
    
    // Data
    ctx.font = '14px Arial';
    ctx.fillStyle = '#7f8c8d';
    const data = new Date(sessao.timestamp).toLocaleString('pt-BR');
    ctx.fillText(`Gravada em: ${data}`, 400, 110);
    
    // Info básica
    ctx.font = '16px Arial';
    ctx.fillStyle = '#2c3e50';
    ctx.textAlign = 'left';
    
    const info = [
      `Total de pontos: ${sessao.dadosTabela.length}`,
      `Primeira leitura: ${sessao.dadosTabela[0]?.tempo_esp || '0'}s`,
      `Última leitura: ${sessao.dadosTabela[sessao.dadosTabela.length-1]?.tempo_esp || '0'}s`,
      `Maior força: ${Math.max(...sessao.dadosTabela.map(d => parseFloat(d.quilo_forca) || 0)).toFixed(3)} kgf`,
      `Menor força: ${Math.min(...sessao.dadosTabela.map(d => parseFloat(d.quilo_forca) || 0)).toFixed(3)} kgf`
    ];
    
    info.forEach((linha, i) => {
      ctx.fillText(linha, 50, 180 + i * 30);
    });
    
    // Download
    baixarRelatorio(canvas, sessao.nome + '_resumo');
    
  } catch (e) {
    console.error('Erro:', e);
    showNotification('error', 'Erro: ' + e.message);
  }
}

// ATUALIZA A FUNÇÃO DE CARREGAR GRAVAÇÕES COM BOTÕES CORRETOS
function carregarGravacoes() {
  const container = document.getElementById('lista-gravacoes');
  if (!container) return;
  
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
    card.style.cssText = `
      display: flex;
      justify-content: space-between;
      align-items: center;
      background: white;
      padding: 15px;
      border-radius: 8px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      margin-bottom: 10px;
    `;
    
    card.innerHTML = `
      <div>
        <p style="font-weight: 600; margin-bottom: 5px;">${gravacao.nome}</p> 
        <p style="font-size: 0.875rem; color: #7f8c8d;">
          ${dataFormatada} • ${gravacao.dadosTabela.length} leituras
        </p>
      </div>
      <div style="display: flex; gap: 8px; flex-wrap: wrap;">
        <button onclick="exportarCSV(${gravacao.id})" 
                style="background: #27ae60; color: white; border: none; padding: 8px 12px; border-radius: 4px; cursor: pointer; font-size: 12px;">
          📄 CSV
        </button>
        <button onclick="exportarImagemSessao(${gravacao.id})" 
                style="background: #3498db; color: white; border: none; padding: 8px 12px; border-radius: 4px; cursor: pointer; font-size: 12px;">
          🖼️ PNG
        </button>
        <button onclick="visualizarSessao(${gravacao.id})" 
                style="background: #9b59b6; color: white; border: none; padding: 8px 12px; border-radius: 4px; cursor: pointer; font-size: 12px;">
          👁️ Ver
        </button>
        <button onclick="deletarGravacao(${gravacao.id})" 
                style="background: #e74c3c; color: white; border: none; padding: 8px 12px; border-radius: 4px; cursor: pointer; font-size: 12px;">
          🗑️ Del
        </button>
      </div>
    `;
    
    container.appendChild(card);
  });
}

// FUNÇÃO PARA VISUALIZAR SESSÃO (mantém a mesma do artifact anterior)
function visualizarSessao(sessionId) {
  try {
    const gravacoes = JSON.parse(localStorage.getItem('balancaGravacoes')) || [];
    const sessao = gravacoes.find(g => g.id === sessionId);
    
    if (!sessao) {
      showNotification('error', 'Sessão não encontrada');
      return;
    }
    
    // Limpa gráfico atual
    clearChart();
    
    // Carrega dados no gráfico
    chartData.labels = [];
    chartData.series = [[]];
    rawDataN = [];
    
    sessao.dadosTabela.forEach(dado => {
      const tempo = parseFloat(dado.tempo_esp) || 0;
      const newtons = parseFloat(dado.newtons) || 0;
      const displayForce = convertForce(newtons, displayUnit);
      
      chartData.labels.push(tempo.toFixed(1));
      chartData.series[0].push(parseFloat(formatForce(displayForce, displayUnit)));
      rawDataN.push(newtons);
    });
    
    // Atualiza estatísticas nos painéis
    if (rawDataN.length > 0) {
      maxForceInN = Math.max(...rawDataN);
      minForceInN = Math.min(...rawDataN);
      
      const currentDisplayForce = convertForce(rawDataN[rawDataN.length - 1], displayUnit);
      const maxDisplayForce = convertForce(maxForceInN, displayUnit);
      const minDisplayForce = convertForce(minForceInN, displayUnit);
      const avgForce = rawDataN.reduce((a, b) => a + b, 0) / rawDataN.length;
      const avgDisplayForce = convertForce(avgForce, displayUnit);
      
      document.getElementById('forca-atual').textContent = `${formatForce(currentDisplayForce, displayUnit)} ${displayUnit}`;
      document.getElementById('forca-maxima').textContent = `${formatForce(maxDisplayForce, displayUnit)} ${displayUnit}`;
      document.getElementById('forca-minima').textContent = `mín: ${formatForce(minDisplayForce, displayUnit)} ${displayUnit}`;
      document.getElementById('forca-ems').textContent = `${formatForce(avgDisplayForce, displayUnit)} ${displayUnit}`;
    }
    
    // Atualiza gráfico
    chart.update(chartData);
    
    // Vai para aba do gráfico
    abrirAba(document.getElementById("padrao"), 'abaGrafico');
    
    showNotification('success', `Sessão "${sessao.nome}" carregada!`);
    
  } catch (e) {
    console.error('Erro ao visualizar:', e);
    showNotification('error', 'Erro ao carregar sessão');
  }
}