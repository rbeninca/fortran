#include <Arduino.h>
#include <Wire.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>
#include <HX711.h>
#include <EEPROM.h>
#include <ESP8266WiFi.h>
#include <ESP8266WebServer.h>
#include <WebSocketsServer.h>
#include <FS.h>
#include <ArduinoJson.h>
#include <Ticker.h>

// --- CONFIGURAÇÃO CORRETA DO DISPLAY ---
#define SCREEN_WIDTH 128
#define SCREEN_HEIGHT 64
#define OLED_RESET -1
#define SCREEN_ADDRESS 0x3C
#define OLED_SDA 14 // GPIO14 -> D5 na placa
#define OLED_SCL 12 // GPIO12 -> D6 na placa
Adafruit_SSD1306 display(SCREEN_WIDTH, SCREEN_HEIGHT, &Wire, OLED_RESET);

// --- CONFIGURAÇÃO DO BUFFER AINDA MAIS REDUZIDA ---
const int NUM_READINGS_IN_BUFFER = 20; // Reduzido de 8 para 10
StaticJsonDocument<JSON_ARRAY_SIZE(NUM_READINGS_IN_BUFFER) + NUM_READINGS_IN_BUFFER * (JSON_OBJECT_SIZE(4)+10)> bufferDoc;
unsigned long lastBroadcastTime = 0;
char jsonOutputBuffer[1250]; // Reduzido de 1024
// --- ESTRUTURA DE CONFIGURAÇÃO ---
struct Config {
  unsigned long magic_number = 123456789;
  char staSSID[32] = "BenincaGaspar";
  char staPassword[32] = "aabbccddee";
  float conversionFactor = 21000.0;
  float gravity = 9.80665;
  int leiturasEstaveis = 10; // Reduzido de 10 para 8
  float toleranciaEstabilidade = 100.0;
  int numAmostrasMedia = 3; // Reduzido de 5 para 3
  unsigned long timeoutCalibracao = 20000; // Reduzido de 30000 para 20000
  long tareOffset = 0;
};
Config config;

// --- PINOS DO SENSOR DE PESO ---
const uint8_t pinData = D7;
const uint8_t pinClock = D8;

// --- OBJETOS E VARIÁVEIS GLOBAIS ---
HX711 loadcell;
ESP8266WebServer server(80);
WebSocketsServer webSocket(81);
Ticker networkWatchdog;
Ticker memoryWatchdog;

String balancaStatus = "Iniciando...";
float pesoAtual_g = 0.0;
unsigned long lastReadTime = 0;
unsigned long lastDisplayUpdateTime = 0;
unsigned long lastNetworkActivity = 0;
bool wifiConnected = false;
bool networkStuck = false;

// --- CONTROLE DE REDE MAIS RIGOROSO ---
unsigned long wifiReconnectAttempts = 0;
const unsigned long MAX_WIFI_RECONNECT_ATTEMPTS = 2;
const unsigned long WIFI_RECONNECT_DELAY = 3000;
const unsigned long NETWORK_TIMEOUT = 60000; // 1 minuto sem atividade = reset
const unsigned long MAX_HEAP_FRAGMENTATION = 50; // % máxima de fragmentação

// --- CONTROLE DE CLIENTES WEBSOCKET ---
unsigned long lastClientActivity[10] = {0}; // Para até 10 clientes
const unsigned long CLIENT_TIMEOUT = 30000; // 30 segundos sem atividade

// --- PROTÓTIPOS DE FUNÇÕES ---
void atualizarDisplay(String status, float peso_em_gramas);
void saveConfig();
void loadConfig();
void broadcastStatus(const char *type, const String &message);
bool aguardarEstabilidade(const String &proposito);
void handleFileRequest();
void onWebSocketEvent(uint8_t client_num, WStype_t type, uint8_t *payload, size_t length);
void verificarClientesWebSocket();
void forceNetworkReset();
void networkWatchdogCallback();
void memoryWatchdogCallback();
void emergencyRestart();
void limpezaProfundaMemoria();
bool isNetworkHealthy();

// =======================================================
// SETUP (CONFIGURAÇÃO INICIAL)
// =======================================================
void setup() {
  Serial.begin(115200);
  Serial.println("\n\nBalança GFIG ESP8266 - Versão Ultra Robusta");

  // Desabilita WiFi inicialmente para configuração limpa
  WiFi.mode(WIFI_OFF);
  delay(100);

  // Configuração inicial do WiFi com parâmetros otimizados
  WiFi.mode(WIFI_STA);
  WiFi.setSleepMode(WIFI_NONE_SLEEP);
  WiFi.setAutoReconnect(false); // Controle manual
  WiFi.persistent(false); // Evita writes desnecessários na flash
  WiFi.setPhyMode(WIFI_PHY_MODE_11N);

  // Inicialização do display
  Wire.begin(OLED_SDA, OLED_SCL);
  if (!display.begin(SSD1306_SWITCHCAPVCC, SCREEN_ADDRESS)) {
    Serial.println(F("Falha ao iniciar display SSD1306."));
    // Não trava, continua sem display
  }
  atualizarDisplay("Iniciando...", 0);

  // Inicialização da EEPROM e SPIFFS
  EEPROM.begin(sizeof(Config));
  if (!SPIFFS.begin()) {
    Serial.println("SPIFFS falhou, continuando sem arquivos");
  }
  loadConfig();

  // Inicialização do HX711
  Serial.println("Iniciando HX711...");
  loadcell.begin(pinData, pinClock);
  loadcell.set_scale(config.conversionFactor);
  loadcell.set_offset(config.tareOffset);

  // Tara inicial apenas se necessário
  if (config.tareOffset == 0) {
    atualizarDisplay("Tarando...", 0);
    if (aguardarEstabilidade("Tara Inicial")) {
      loadcell.tare(config.numAmostrasMedia);
      config.tareOffset = loadcell.get_offset();
      saveConfig();
    }
  }

  // Configuração do AP primeiro
  atualizarDisplay("Config AP...", 0);
  WiFi.softAP("balancaGFIG", "aabbccddee");
  delay(100);
  Serial.print("AP IP: ");
  Serial.println(WiFi.softAPIP());

  // Configuração do servidor web com timeouts reduzidos
  server.on("/salvarRede", HTTP_POST, []() {
    Serial.println("Salvando nova rede...");
    if (server.hasArg("ssid") && server.hasArg("senha")) {
      String newSSID = server.arg("ssid");
      String newPassword = server.arg("senha");
      
      // Validação básica
      if (newSSID.length() > 0 && newSSID.length() < 32) {
        strncpy(config.staSSID, newSSID.c_str(), sizeof(config.staSSID) - 1);
        strncpy(config.staPassword, newPassword.c_str(), sizeof(config.staPassword) - 1);
        config.staSSID[sizeof(config.staSSID) - 1] = '\0';
        config.staPassword[sizeof(config.staPassword) - 1] = '\0';
        saveConfig();
        server.send(200, "text/plain", "Rede salva. Reiniciando em 3s...");
        delay(3000);
        ESP.restart();
      } else {
        server.send(400, "text/plain", "SSID inválido");
      }
    } else {
      server.send(400, "text/plain", "Parâmetros obrigatórios: ssid, senha");
    }
  });
  
  server.on("/status", HTTP_GET, []() {
    StaticJsonDocument<256> doc;
    doc["heap"] = ESP.getFreeHeap();
    doc["fragmentation"] = ESP.getHeapFragmentation();
    doc["uptime"] = millis();
    doc["wifi_status"] = WiFi.status();
    doc["clients"] = webSocket.connectedClients();
    String output;
    serializeJson(doc, output);
    server.send(200, "application/json", output);
  });

  server.on("/restart", HTTP_POST, []() {
    server.send(200, "text/plain", "Reiniciando...");
    delay(1000);
    ESP.restart();
  });
  
  server.onNotFound(handleFileRequest);
  server.begin();

  // Configuração do WebSocket com configurações otimizadas
  webSocket.onEvent(onWebSocketEvent);
  webSocket.begin();
  webSocket.enableHeartbeat(15000, 3000, 2); // Heartbeat agressivo

  // Inicialização do buffer JSON
  bufferDoc.to<JsonArray>();
  lastBroadcastTime = millis();
  lastNetworkActivity = millis();

  // Configuração dos watchdogs
  networkWatchdog.attach(30, networkWatchdogCallback); // Verifica rede a cada 30s
  memoryWatchdog.attach(10, memoryWatchdogCallback);   // Verifica memória a cada 10s

  // Tentativa inicial de conexão WiFi
  atualizarDisplay("Conectando WiFi...", 0);
  if (strlen(config.staSSID) > 0) {
    WiFi.begin(config.staSSID, config.staPassword);
    unsigned long startTime = millis();
    while (WiFi.status() != WL_CONNECTED && millis() - startTime < 10000) {
      delay(200);
      yield();
      ESP.wdtFeed();
    }
    
    if (WiFi.status() == WL_CONNECTED) {
      wifiConnected = true;
      Serial.println("WiFi conectado inicialmente!");
      Serial.print("IP: ");
      Serial.println(WiFi.localIP());
    }
  }

  balancaStatus = "Pronta";
  atualizarDisplay(balancaStatus, pesoAtual_g);
  Serial.println("Setup concluído. Sistema operacional.");
}

// =======================================================
// LOOP PRINCIPAL ULTRA OTIMIZADO
// =======================================================
void loop() {
  // Alimenta o watchdog do sistema
  ESP.wdtFeed();
  
  // Verifica se a rede está travada
  if (networkStuck) {
    Serial.println("[NETWORK] Rede travada detectada, forçando reset...");
    forceNetworkReset();
    return;
  }

  // Processa clientes rapidamente
  unsigned long startProcess = millis();
  webSocket.loop();
  server.handleClient();
  
  // Se demorou muito para processar, há problema
  if (millis() - startProcess > 100) {
    Serial.println("[WARNING] Processamento lento detectado");
    limpezaProfundaMemoria();
  }
  
  yield();

  // COLETOR DE DADOS - Frequência reduzida para 100ms
  if (millis() - lastReadTime >=25) {
    lastReadTime = millis();

    // Pula leituras durante processos especiais
    if (balancaStatus.indexOf("Tarar") != -1 || balancaStatus.indexOf("Calibrar") != -1) {
      return;
    }

    if (loadcell.is_ready()) {
      balancaStatus = "Pesando";
      pesoAtual_g = loadcell.get_units(config.numAmostrasMedia);
      if (config.conversionFactor < 0) {
        pesoAtual_g *= -1;
      }

      // Adiciona ao buffer apenas se há clientes conectados e rede saudável
      if (webSocket.connectedClients() > 0 && isNetworkHealthy()) {
        JsonArray readingsArray = bufferDoc.as<JsonArray>();
        
        // Limita rigorosamente o tamanho do buffer
        while (readingsArray.size() >= NUM_READINGS_IN_BUFFER) {
          readingsArray.remove(0);
        }
        
        JsonObject readingObj = readingsArray.createNestedObject();
        readingObj["type"] = "data";
        readingObj["tempo"] = millis() / 1000.0;
        readingObj["forca"] = (pesoAtual_g / 1000.0) * config.gravity;
        readingObj["status"] = balancaStatus;
        
        lastNetworkActivity = millis();
      }
    }
  }

  // ENVIADOR DE DADOS - Frequência aumentada para 300ms
  if (millis() - lastBroadcastTime >= 300) {
    lastBroadcastTime = millis();

    if (webSocket.connectedClients() > 0 && isNetworkHealthy()) {
      JsonArray readingsArray = bufferDoc.as<JsonArray>();
      
      if (readingsArray.size() > 0) {
        // Limita o tamanho máximo do JSON
        size_t jsonSize = measureJson(bufferDoc);
        if (jsonSize < sizeof(jsonOutputBuffer) - 50) { // Margem de segurança
          jsonSize = serializeJson(bufferDoc, jsonOutputBuffer, sizeof(jsonOutputBuffer));
          
          if (jsonSize > 0) {
            bool success = webSocket.broadcastTXT(jsonOutputBuffer, jsonSize);
            if (success) {
              lastNetworkActivity = millis();
            } else {
              Serial.println("[WebSocket] Falha no envio, limpando clientes");
              // Limpa clientes problemáticos
              for (uint8_t i = 0; i < webSocket.connectedClients(); i++) {
                webSocket.disconnect(i);
              }
            }
          }
        } else {
          Serial.printf("[WARNING] JSON muito grande: %u bytes\n", jsonSize);
        }
        
        // Sempre limpa o buffer após tentativa de envio
        bufferDoc.clear();
        bufferDoc.to<JsonArray>();
        memset(jsonOutputBuffer, 0, sizeof(jsonOutputBuffer));
      }
    }
  }

  // ATUALIZAÇÃO DE DISPLAY - Frequência reduzida para 1000ms
  if (millis() - lastDisplayUpdateTime >= 1000) {
    lastDisplayUpdateTime = millis();
    atualizarDisplay(balancaStatus, pesoAtual_g);
  }
}

// =======================================================
// FUNÇÕES DE MONITORAMENTO E RECUPERAÇÃO
// =======================================================

void networkWatchdogCallback() {
  // Verifica se há atividade de rede recente
  if (millis() - lastNetworkActivity > NETWORK_TIMEOUT) {
    Serial.println("[WATCHDOG] Rede inativa por muito tempo");
    networkStuck = true;
  }
  
  // Verifica status do WiFi
  if (WiFi.status() != WL_CONNECTED && wifiConnected) {
    Serial.println("[WATCHDOG] WiFi desconectado");
    wifiConnected = false;
    networkStuck = true;
  }
}

void memoryWatchdogCallback() {
  uint32_t freeHeap = ESP.getFreeHeap();
  uint8_t fragmentation = ESP.getHeapFragmentation();
  
  // Condições críticas de memória
  if (freeHeap < 3000 || fragmentation > MAX_HEAP_FRAGMENTATION) {
    Serial.printf("[WATCHDOG] Memória crítica: %u bytes, %u%% fragmentada\n", freeHeap, fragmentation);
    limpezaProfundaMemoria();
    
    // Se ainda crítico, força restart
    if (ESP.getFreeHeap() < 2000) {
      Serial.println("[EMERGENCY] Memória ainda crítica, reiniciando...");
      emergencyRestart();
    }
  }
}

void forceNetworkReset() {
  Serial.println("[RESET] Forçando reset da rede...");
  
  // Para todos os serviços de rede
  webSocket.disconnect();
  server.stop();
  WiFi.disconnect(true);
  WiFi.mode(WIFI_OFF);
  
  delay(1000);
  
  // Limpa tudo
  limpezaProfundaMemoria();
  
  // Reinicia rede
  WiFi.mode(WIFI_STA);
  WiFi.begin(config.staSSID, config.staPassword);
  
  // Reinicia serviços
  server.begin();
  webSocket.begin();
  
  networkStuck = false;
  lastNetworkActivity = millis();
  
  Serial.println("[RESET] Reset de rede concluído");
}

void limpezaProfundaMemoria() {
  // Limpa todos os buffers
  bufferDoc.clear();
  bufferDoc.to<JsonArray>();
  memset(jsonOutputBuffer, 0, sizeof(jsonOutputBuffer));
  
  // Força limpeza de strings
  balancaStatus.trim();
  if (balancaStatus.length() > 20) {
    balancaStatus = "Pronta";
  }
  
  // Limpa clientes WebSocket inativos
  verificarClientesWebSocket();
  
  delay(10);
  yield();
  ESP.wdtFeed();
  
  Serial.printf("[CLEANUP] Heap após limpeza: %u bytes\n", ESP.getFreeHeap());
}

void emergencyRestart() {
  Serial.println("[EMERGENCY] Reinício de emergência em 3 segundos...");
  atualizarDisplay("REINICIANDO...", 0);
  delay(3000);
  ESP.restart();
}

bool isNetworkHealthy() {
  return (WiFi.status() == WL_CONNECTED && 
          !networkStuck && 
          ESP.getFreeHeap() > 3000 &&
          ESP.getHeapFragmentation() < MAX_HEAP_FRAGMENTATION);
}

void verificarClientesWebSocket() {
  uint8_t clientCount = webSocket.connectedClients();
  unsigned long currentTime = millis();
  
  for (uint8_t i = 0; i < clientCount; i++) {
    // Desconecta clientes inativos há muito tempo
    if (currentTime - lastClientActivity[i] > CLIENT_TIMEOUT) {
      Serial.printf("[WebSocket] Cliente %u inativo, desconectando\n", i);
      webSocket.disconnect(i);
    }
  }
}

// =======================================================
// FUNÇÕES BÁSICAS (OTIMIZADAS)
// =======================================================

void atualizarDisplay(String status, float peso_em_gramas) {
  display.clearDisplay();
  display.setTextColor(SSD1306_WHITE);

  // Status (linha 1)
  display.setTextSize(1);
  display.setCursor(0, 0);
  display.print("Status: ");
  display.println(status.substring(0, 15)); // Limita tamanho

  // Peso (linha 2-3)
  display.setTextSize(2);
  display.setCursor(0, 16);
  display.print(peso_em_gramas / 1000.0, 3); // Reduzido para 2 decimais
  display.println(" kg");

  // Rede (linha 4)
  display.setTextSize(1);
  display.setCursor(0, 40);
  if (WiFi.status() == WL_CONNECTED) {
    display.print("IP: ");
    String ip = WiFi.localIP().toString();
    display.println(ip); 
  } else {
    display.println("WiFi: OFF");
  }
  
  // Status sistema (linha 5)
  display.setCursor(0, 50);
  display.print("H:");
  display.print(ESP.getFreeHeap() / 1024); // Em KB
  display.print("k C:");
  display.print(webSocket.connectedClients());

  display.display();
}

void saveConfig() {
  EEPROM.put(0, config);
  if (EEPROM.commit()) {
    Serial.println("Config salva OK");
  } else {
    Serial.println("Erro ao salvar config");
  }
}

void loadConfig() {
  Config tempConfig;
  EEPROM.get(0, tempConfig);
  if (tempConfig.magic_number == 123456789) {
    config = tempConfig;
    Serial.println("Config carregada da EEPROM");
  } else {
    Serial.println("Config inválida, usando padrões");
    config = Config();
    saveConfig();
  }
}

void broadcastStatus(const char *type, const String &message) {
  balancaStatus = message;
  
  if (webSocket.connectedClients() > 0 && isNetworkHealthy()) {
    StaticJsonDocument<256> doc;
    doc["type"] = "status";
    doc["status"] = type;
    doc["message"] = message; // Mantém mensagem original sem limitação
    
    String output;
    serializeJson(doc, output);
    
    if (output.length() < 240) { // Verifica tamanho mas mantém conteúdo
      webSocket.broadcastTXT(output);
      lastNetworkActivity = millis();
    }
  }
}

bool aguardarEstabilidade(const String &proposito) {
  const unsigned long timeout_ms = config.timeoutCalibracao;
   delay(12);
  unsigned long inicio = millis();
  int leiturasEstaveis = 0;
  long leituraAnterior = 0;
  // Pequeno delay para estabilizar o sistema
  if (!loadcell.is_ready()) {
    broadcastStatus("error", "Celula nao pronta");
    return false;
  }

  leituraAnterior = loadcell.read_average(config.numAmostrasMedia);

  while (leiturasEstaveis < config.leiturasEstaveis) {
    // Timeout mais rigoroso
    if (millis() - inicio > timeout_ms) {
      broadcastStatus("error", "Timeout estabilidade");
      balancaStatus = "Pronta";
      return false;
    }

    // Processa sistema
    ESP.wdtFeed();
    webSocket.loop();
    server.handleClient();
    yield();

    if (loadcell.is_ready()) {
      long leituraAtual = loadcell.read_average(config.numAmostrasMedia);
      
      String progresso = proposito + " " + String(leiturasEstaveis + 1) + "/" + String(config.leiturasEstaveis);
      broadcastStatus("info", progresso);

      if (abs(leituraAtual - leituraAnterior) < config.toleranciaEstabilidade) {
        leiturasEstaveis++;
      } else {
        leiturasEstaveis = 0;
      }

      leituraAnterior = leituraAtual;
    }

    delay(30); // Delay mínimo
  }

  broadcastStatus("success", "Estabilizado OK");
  return true;
}

void handleFileRequest() {
  String path = server.uri();
  if (path.endsWith("/")) path += "index.html";
  
  String contentType = "text/plain";
  if (path.endsWith(".html")) contentType = "text/html";
  else if (path.endsWith(".css")) contentType = "text/css";
  else if (path.endsWith(".js")) contentType = "application/javascript";

  if (SPIFFS.exists(path)) {
    File file = SPIFFS.open(path, "r");
    size_t sent = server.streamFile(file, contentType);
    file.close();
    if (sent > 0) {
      lastNetworkActivity = millis();
    }
  } else {
    server.send(404, "text/plain", "404: Not Found");
  }
}

void onWebSocketEvent(uint8_t client_num, WStype_t type, uint8_t *payload, size_t length) {
  lastClientActivity[client_num] = millis();
  lastNetworkActivity = millis();
  
  switch (type) {
    case WStype_DISCONNECTED:
      Serial.printf("[%u] Desconectado\n", client_num);
      lastClientActivity[client_num] = 0;
      break;
      
    case WStype_CONNECTED: {
      // Limite rigoroso de clientes
      if (webSocket.connectedClients() > 1) {
        Serial.printf("[WebSocket] Cliente %u rejeitado (limite)\n", client_num);
        webSocket.sendTXT(client_num, "{\"type\":\"error\",\"message\":\"Limite de clientes\"}");
        webSocket.disconnect(client_num);
        return;
      }

      IPAddress ip = webSocket.remoteIP(client_num);
      Serial.printf("[%u] Conectado: %s\n", client_num, ip.toString().c_str());
      
      // Envia configuração com strings originais
      StaticJsonDocument<512> doc;
      doc["type"] = "config";
      doc["conversionFactor"] = config.conversionFactor;
      doc["gravity"] = config.gravity;
      doc["leiturasEstaveis"] = config.leiturasEstaveis;
      doc["toleranciaEstabilidade"] = config.toleranciaEstabilidade;
      doc["numAmostrasMedia"] = config.numAmostrasMedia;
      doc["timeoutCalibracao"] = config.timeoutCalibracao;
      doc["tareOffset"] = config.tareOffset;
      
      String output;
      serializeJson(doc, output);
      webSocket.sendTXT(client_num, output);
      break;
    }
    
    case WStype_PONG:
      Serial.printf("[%u] PONG OK\n", client_num);
      break;
      
    case WStype_TEXT: {
      if (length > 100) { // Limita tamanho de comando
        Serial.println("Comando muito grande ignorado");
        return;
      }
      
      String msg = String((char *)payload);
      msg.trim();

      if (msg == "t") {
        if (aguardarEstabilidade("Tarar")) {
          loadcell.tare(config.numAmostrasMedia);
          config.tareOffset = loadcell.get_offset();
          saveConfig();
          broadcastStatus("success", "Tara OK");
        } else {
          broadcastStatus("error", "Falha tara");
        }
      }
      else if (msg.startsWith("c:")) {
        float massa_g = msg.substring(2).toFloat();
        if (massa_g > 0 && massa_g < 100000) { // Validação
          if (aguardarEstabilidade("Calibrar")) {
            long leituraRaw = loadcell.read_average(config.numAmostrasMedia);
            long offset = loadcell.get_offset();
            config.conversionFactor = (float)(leituraRaw - offset) / massa_g;
            loadcell.set_scale(config.conversionFactor);
            saveConfig();
            broadcastStatus("success", "Calibracao OK");
          } else {
            broadcastStatus("error", "Falha calibracao");
          }
        } else {
          broadcastStatus("error", "Massa invalida");
        }
      }
      else if (msg.startsWith("set_param:")) {
        // Comando para alterar parâmetros - mantendo compatibilidade
        int firstColon = msg.indexOf(':');
        int secondColon = msg.indexOf(':', firstColon + 1);
        if (firstColon != -1 && secondColon != -1) {
          String paramName = msg.substring(firstColon + 1, secondColon);
          String paramValue = msg.substring(secondColon + 1);
          bool changed = false;

          if (paramName == "conversionFactor") {
            config.conversionFactor = paramValue.toFloat();
            loadcell.set_scale(config.conversionFactor);
            changed = true;
          }
          else if (paramName == "gravity") {
            config.gravity = paramValue.toFloat();
            changed = true;
          }
          else if (paramName == "leiturasEstaveis") {
            config.leiturasEstaveis = constrain(paramValue.toInt(), 1, 50);
            changed = true;
          }
          else if (paramName == "toleranciaEstabilidade") {
            config.toleranciaEstabilidade = paramValue.toFloat();
            changed = true;
          }
          else if (paramName == "numAmostrasMedia") {
            config.numAmostrasMedia = constrain(paramValue.toInt(), 1, 20);
            changed = true;
          }
          else if (paramName == "timeoutCalibracao") {
            config.timeoutCalibracao = paramValue.toInt();
            changed = true;
          }
          else if (paramName == "tareOffset") {
            config.tareOffset = paramValue.toInt();
            loadcell.set_offset(config.tareOffset);
            changed = true;
          }

          if (changed) {
            saveConfig();
            broadcastStatus("success", "Parâmetro '" + paramName + "' atualizado!");
          } else {
            broadcastStatus("error", "Parâmetro desconhecido: " + paramName);
          }
        }
      }
      
      break;
    }
  }
}