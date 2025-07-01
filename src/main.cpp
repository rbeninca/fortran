#include <Arduino.h>
#include <HX711.h>
#include <EEPROM.h>
#include <ESP8266WiFi.h>
#include <ESP8266WebServer.h>
#include <WebSocketsServer.h>
#include <FS.h>
#include <ArduinoJson.h>

// Alimentação via GPIO, conforme solicitado.
// AVISO: Esta é a causa provável de instabilidade nas leituras.
#define PIN_VCC D0   // GPIO16
#define PIN_GND D3   // GPIO0

// ======== ESTRUTURA DE CONFIGURAÇÃO ========
struct Config {
  unsigned long magic_number = 123456789;
  char staSSID[32] = "BenincaGaspar";
  char staPassword[32] = "aabbccddee";
  float conversionFactor = 21000.0;
  float gravity = 9.80665;
  int leiturasEstaveis = 10;
  float toleranciaEstabilidade = 100.0;
  int numAmostrasMedia = 10;
  unsigned long timeoutCalibracao = 30000;
  long tareOffset = 0;
};

Config config;

// ======== CONSTANTES GLOBAIS ========
const char* apSSID = "balancaESP";
const char* apPassword = "";

const uint8_t pinData = D2;
const uint8_t pinClock = D1;
const unsigned long READ_INTERVAL = 100;

HX711 loadcell;
ESP8266WebServer server(80);
WebSocketsServer webSocket(81);

unsigned long lastReadTime = 0;
String balancaStatus = "Iniciando...";

// ======== FUNÇÕES DE SUPORTE ========
void saveConfig() {
  EEPROM.put(0, config);
  EEPROM.commit();
  Serial.println("DEBUG: Configuracao salva na EEPROM.");
}

void loadConfig() {
  EEPROM.get(0, config);
  if (config.magic_number != 123456789 || config.leiturasEstaveis < 1 || config.leiturasEstaveis > 100) {
    Serial.println("DEBUG: Configuracao invalida, carregando padroes.");
    config = Config();
    saveConfig();
  } else {
    Serial.println("DEBUG: Configuracao carregada da EEPROM.");
  }
}

void broadcastStatus(const char* type, const String& message) {
  StaticJsonDocument<256> doc;
  doc["type"] = "status";
  doc["status"] = type;
  doc["message"] = message;
  String output;
  serializeJson(doc, output);
  webSocket.broadcastTXT(output);
}

bool aguardarEstabilidade(const String& proposito) {
    balancaStatus = proposito;
    broadcastStatus("info", "Aguardando estabilidade para " + proposito + "...");
    Serial.println("DEBUG: Entrou em aguardarEstabilidade para: " + proposito);
    delay(500);

    if (!loadcell.is_ready()) {
        broadcastStatus("error", "Erro: Célula de carga não está pronta.");
        Serial.println("DEBUG: ERROR - loadcell.is_ready() retornou false.");
        return false;
    }

    unsigned long inicioTimeout = millis();
    long leituraAnterior = loadcell.read_average(config.numAmostrasMedia);
    int leiturasEstaveisCount = 0;

    while (leiturasEstaveisCount < config.leiturasEstaveis) {
        if (millis() - inicioTimeout > config.timeoutCalibracao) {
            broadcastStatus("error", "Erro de Timeout! A balança não estabilizou.");
            Serial.println("DEBUG: Timeout em aguardarEstabilidade.");
            return false;
        }

        if (loadcell.is_ready()) {
            long leituraAtual = loadcell.read_average(config.numAmostrasMedia);
            long diferenca = abs(leituraAtual - leituraAnterior);

            // Removido o broadcast de status daqui para não poluir a interface, focando no Serial
            Serial.printf("DEBUG: Estabilidade -> Leituras: %d/%d | Variação: %ld\n", leiturasEstaveisCount, config.leiturasEstaveis, diferenca);
            
            if (diferenca < config.toleranciaEstabilidade) {
                leiturasEstaveisCount++;
            } else {
                leiturasEstaveisCount = 0;
            }
            leituraAnterior = leituraAtual;
        }
        delay(200);
    }
    broadcastStatus("success", "Peso estabilizado para " + proposito + "!");
    Serial.println("DEBUG: Estabilidade alcancada para: " + proposito);
    return true;
}


// ======== WEBSOCKET ========
void onWebSocketEvent(uint8_t client_num, WStype_t type, uint8_t * payload, size_t length) {
  switch (type) {
    case WStype_DISCONNECTED:
      Serial.printf("[%u] Desconectado!\n", client_num);
      break;

    case WStype_CONNECTED: {
      IPAddress ip = webSocket.remoteIP(client_num);
      Serial.printf("[%u] Conectado de %d.%d.%d.%d\n", client_num, ip[0], ip[1], ip[2], ip[3]);

      StaticJsonDocument<512> doc;
      doc["type"] = "config";
      doc["ssid"] = config.staSSID;
      doc["password"] = config.staPassword;
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

    case WStype_TEXT: {
      String msg = String((char*)payload);
      balancaStatus = "Processando Comando";

      if (msg == "t") {
        if (aguardarEstabilidade("Tarar")) {
            loadcell.tare(20);
            config.tareOffset = loadcell.get_offset();
            saveConfig();
            broadcastStatus("success", "Tara concluída e salva!");
        } else {
            broadcastStatus("error", "Falha ao tarar. A balança não estabilizou.");
        }
      }
      else if (msg.startsWith("c:")) {
        float massa_g = msg.substring(2).toFloat();
        if (massa_g <= 0) {
          broadcastStatus("error", "Massa de calibração deve ser maior que zero.");
          return;
        }

        if (!aguardarEstabilidade("Calibrar com Peso")) {
             balancaStatus = "Pronta"; return;
        }

        broadcastStatus("info", "Peso estabilizado. Calculando o fator...");
        long leituraComPeso = loadcell.read_average(20);
        long offsetAtual = loadcell.get_offset();
        float diferenca = leituraComPeso - offsetAtual;

        if (diferenca == 0) {
           broadcastStatus("error", "Erro: Leitura de calibração resultou em zero.");
           balancaStatus = "Pronta"; return;
        }

        config.conversionFactor = diferenca / massa_g;
        loadcell.set_scale(config.conversionFactor);
        saveConfig();
        broadcastStatus("success", "Fator de conversão calculado: " + String(config.conversionFactor));

        broadcastStatus("info", "AGORA, REMOVA O PESO da balança para registrar o novo zero.");
        delay(5000);

        if (aguardarEstabilidade("Registrar Nova Tara")) {
            loadcell.tare(20);
            config.tareOffset = loadcell.get_offset();
            saveConfig();
            broadcastStatus("success", "Calibração finalizada com sucesso! Nova tara foi registrada.");
        } else {
            broadcastStatus("error", "Falha ao registrar a nova tara. A balança não estabilizou. Tente a calibração novamente.");
        }

        balancaStatus = "Pronta";
      }
      else if (msg.startsWith("set_param:")) {
        int firstColon = msg.indexOf(':');
        int secondColon = msg.indexOf(':', firstColon + 1);
        String paramName = msg.substring(firstColon + 1, secondColon);
        String paramValue = msg.substring(secondColon + 1);

        bool changed = false;
        if (paramName == "conversionFactor") { config.conversionFactor = paramValue.toFloat(); loadcell.set_scale(config.conversionFactor); changed = true; }
        else if (paramName == "gravity") { config.gravity = paramValue.toFloat(); changed = true; }
        else if (paramName == "leiturasEstaveis") { config.leiturasEstaveis = paramValue.toInt(); changed = true; }
        else if (paramName == "toleranciaEstabilidade") { config.toleranciaEstabilidade = paramValue.toFloat(); changed = true; }
        else if (paramName == "numAmostrasMedia") { config.numAmostrasMedia = paramValue.toInt(); changed = true; }
        else if (paramName == "timeoutCalibracao") { config.timeoutCalibracao = paramValue.toInt(); changed = true; }

        if (changed) { saveConfig(); broadcastStatus("success", "Parâmetro '" + paramName + "' salvo!"); }
        else { broadcastStatus("error", "Parâmetro desconhecido."); }
      }
      break;
    }
  }
}

// ======== SERVIDOR WEB ========
void handleFileRequest() {
  String path = server.uri();
  if (path.endsWith("/")) path += "index.html";
  String contentType = "text/plain";
  if (path.endsWith(".html")) contentType = "text/html";
  else if (path.endsWith(".css")) contentType = "text/css";
  else if (path.endsWith(".js")) contentType = "application/javascript";
  else if (path.endsWith(".gz")) contentType = "application/x-gzip";

  if (SPIFFS.exists(path)) {
    File file = SPIFFS.open(path, "r");
    server.streamFile(file, contentType);
    file.close();
  } else {
    server.send(404, "text/plain", "404: Not Found");
  }
}

// ======== SETUP ========
void setup() {
  Serial.begin(115200);
  Serial.println("\n\nDEBUG: Iniciando Balança ESP8266...");

  EEPROM.begin(sizeof(Config));
  SPIFFS.begin();
  
  Serial.println("DEBUG: Carregando configuracao...");
  loadConfig();

  Serial.println("DEBUG: Configurando alimentacao GPIO...");
  pinMode(PIN_VCC, OUTPUT);
  pinMode(PIN_GND, OUTPUT);
  digitalWrite(PIN_VCC, HIGH);
  digitalWrite(PIN_GND, LOW);
  delay(100); // Aumentei o delay para garantir que a alimentação "estabilize"
  
  Serial.println("DEBUG: Iniciando HX711...");
  loadcell.begin(pinData, pinClock);
  loadcell.set_scale(config.conversionFactor);

  Serial.println("DEBUG: Verificando Tara da EEPROM...");
  if (config.tareOffset != 0) {
    loadcell.set_offset(config.tareOffset);
    Serial.println("DEBUG: Tara carregada da EEPROM.");
  } else {
    Serial.println("DEBUG: EEPROM sem tara. Realizando tara inicial...");
    if(aguardarEstabilidade("Tara Inicial")) {
        loadcell.tare();
        config.tareOffset = loadcell.get_offset();
        saveConfig();
        Serial.println("DEBUG: Tara inicial realizada com sucesso.");
    } else {
        Serial.println("DEBUG: FALHA ao realizar tara inicial (Timeout).");
    }
  }

  balancaStatus = "Conectando WiFi...";
  Serial.println("DEBUG: Iniciando WiFi...");
  WiFi.mode(WIFI_AP_STA);
  WiFi.softAP(apSSID, apPassword);
  WiFi.begin(config.staSSID, config.staPassword);
  Serial.print("Conectando a rede ");
  Serial.print(config.staSSID);
  unsigned long start = millis();
  while (WiFi.status() != WL_CONNECTED && millis() - start < 10000) {
    delay(500);
    Serial.print(".");
  }
  if (WiFi.status() == WL_CONNECTED) { Serial.println("\nIP: " + WiFi.localIP().toString()); }
  else { Serial.println("\nFalha na conexao. Usando apenas Modo AP."); }

  Serial.println("DEBUG: Iniciando Servidor Web e WebSocket...");
  server.on("/salvarRede", HTTP_POST, []() {
    strncpy(config.staSSID, server.arg("ssid").c_str(), sizeof(config.staSSID) - 1);
    strncpy(config.staPassword, server.arg("senha").c_str(), sizeof(config.staPassword) - 1);
    saveConfig();
    server.send(200, "text/plain", "Rede salva. Reinicie o dispositivo para aplicar.");
  });
  server.onNotFound(handleFileRequest);
  server.begin();
  webSocket.begin();
  webSocket.onEvent(onWebSocketEvent);

  balancaStatus = "Pronta";
  Serial.println("\nDEBUG: Setup concluido. Entrando no loop principal...\n");
}

// ======== LOOP PRINCIPAL ========
// ======== LOOP PRINCIPAL (CORRIGIDO) ========
void loop() {
  webSocket.loop();
  server.handleClient();

  // A verificação de tempo continua igual
  if (millis() - lastReadTime >= READ_INTERVAL) {
    lastReadTime = millis();

    // LÓGICA CORRIGIDA:
    // Agora, a gente não depende mais do 'balancaStatus' para tentar ler.
    // A gente tenta ler toda vez. Se não conseguir, informa o usuário.
    
    // 1. A balança está em um processo comandado pelo usuário (Calibração ou Tara)?
    if (balancaStatus == "Calibrando" || balancaStatus == "Tarando") {
      // Se estiver, não faz nada aqui, pois a função aguardarEstabilidade está no controle.
      return; 
    }

    // 2. Se não estiver em um processo, tenta ler o peso.
    if (loadcell.is_ready()) {
      balancaStatus = "Pesando";

      float mass_g = loadcell.get_units(5);
      
      // O fator de conversão negativo está invertendo suas leituras.
      // A linha abaixo é uma gambiarra temporária para visualizar o peso correto.
      // O ideal é corrigir a fiação da célula de carga.
      if (config.conversionFactor < 0) {
        mass_g = mass_g * -1;
      }

      float force_N = (mass_g / 1000.0) * config.gravity;

      StaticJsonDocument<200> doc;
      doc["type"] = "data";
      doc["tempo"] = millis() / 1000.0;
      doc["forca"] = force_N; // Envia o valor em Newtons
      doc["status"] = balancaStatus;

      String output;
      serializeJson(doc, output);
      webSocket.broadcastTXT(output);

    } else {
      // 3. Se a célula não estiver pronta, informa o usuário e tenta de novo na próxima volta.
      balancaStatus = "Aguardando";
      broadcastStatus("info", "Aguardando célula de carga...");
      Serial.println("Loop: Célula de carga não está pronta.");
    }
  }
}