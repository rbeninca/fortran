#include <Arduino.h>
#include <HX711.h>
#include <EEPROM.h>
#include <ESP8266WiFi.h>
#include <ESP8266WebServer.h>
#include <WebSocketsServer.h>
#include <FS.h>
#include <ArduinoJson.h> // Usando a biblioteca para criar JSON de forma segura.

// ======== ESTRUTURA DE CONFIGURAÇÃO ========
struct Config {
  unsigned long magic_number = 123456789; // "Número mágico" para verificar a validade da EEPROM
  char staSSID[32] = "BenincaGaspar";
  char staPassword[32] = "aabbccddee";
  float conversionFactor = 21000.0;
  float gravity = 9.80665;
  int leiturasEstaveis = 10;
  float toleranciaEstabilidade = 100.0;
  int numAmostrasMedia = 5;
  unsigned long timeoutCalibracao = 20000;
  long tareOffset = 0; // Campo para guardar o valor da tara permanentemente
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
  Serial.println("Configuracao salva na EEPROM.");
}

void loadConfig() {
  EEPROM.get(0, config);
  if (config.magic_number != 123456789 || config.leiturasEstaveis < 1 || config.leiturasEstaveis > 100) {
    Serial.println("Configuracao invalida na EEPROM. Carregando valores padrao.");
    config.magic_number = 123456789;
    strcpy(config.staSSID, "BenincaGaspar");
    strcpy(config.staPassword, "aabbccddee");
    config.conversionFactor = 21000.0;
    config.gravity = 9.80665;
    config.leiturasEstaveis = 10;
    config.toleranciaEstabilidade = 100.0;
    config.numAmostrasMedia = 5;
    config.timeoutCalibracao = 20000;
    config.tareOffset = 0;
    saveConfig();
  } else {
    Serial.println("Configuracao carregada com sucesso da EEPROM.");
  }
}

void broadcastStatus(const char* status, const String& message) {
  StaticJsonDocument<200> doc;
  doc["type"] = "status";
  doc["status"] = status;
  doc["message"] = message;
  String output;
  serializeJson(doc, output);
  webSocket.broadcastTXT(output);
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
        loadcell.tare();
        config.tareOffset = loadcell.get_offset();
        saveConfig();
        broadcastStatus("success", "Tara concluída e salva!");
      } else if (msg.startsWith("c:")) {
        float massa_g = msg.substring(2).toFloat(); // Massa fornecida em gramas
        if (massa_g <= 0) {
          broadcastStatus("error", "Massa de calibração deve ser maior que zero.");
          return;
        }

        balancaStatus = "Calibrando";
        broadcastStatus("info", "Iniciando calibração... Por favor, aguarde o peso estabilizar.");
        delay(1000); // Pequena pausa para estabilizar a conexão
        unsigned long inicioTimeout = millis();
        if (!loadcell.is_ready()) {
            broadcastStatus("error", "Erro: Célula de carga não está pronta.");
            return;
        }
        long leituraAnterior = loadcell.read_average(config.numAmostrasMedia);
        int leiturasEstaveisCount = 0;

        while (leiturasEstaveisCount < config.leiturasEstaveis) {
          //informa status com numero de leituras feitas
          String statusMsg = "Lendo peso... Leituras feitas: " + String(leiturasEstaveisCount) + "/" + String(config.leiturasEstaveis);
          broadcastStatus("info", statusMsg);
          if (millis() - inicioTimeout > config.timeoutCalibracao) {
            broadcastStatus("error", "Erro de Timeout! A balança não estabilizou. Verifique o peso e tente novamente.");
            return;
          }
          if (loadcell.is_ready()) {
            long leituraAtual = loadcell.read_average(config.numAmostrasMedia);
            if (abs(leituraAtual - leituraAnterior) < config.toleranciaEstabilidade) leiturasEstaveisCount++;
            else leiturasEstaveisCount = 0;
            leituraAnterior = leituraAtual;
          }
          delay(100);
        }

        broadcastStatus("info", "Peso estabilizado. Calculando o fator...");
        float mediaBruta = loadcell.read_average(20);
        long offsetAtual = loadcell.get_offset();
        float diferenca = mediaBruta - offsetAtual;
        
        if (diferenca == 0) {
           broadcastStatus("error", "Erro: Leitura de calibração resultou em zero.");
           return;
        }

        // O fator de conversão é calculado para resultar em gramas
        config.conversionFactor = diferenca / massa_g;
        loadcell.set_scale(config.conversionFactor);

        broadcastStatus("info", "Fator calculado. Agora, REMOVA O PESO da balança para registrar o novo zero.");
        delay(5000);

        broadcastStatus("info", "A registrar nova tara... Por favor, aguarde.");
        balancaStatus = "Tarando";
        
        inicioTimeout = millis();
        leituraAnterior = loadcell.read_average(config.numAmostrasMedia);
        leiturasEstaveisCount = 0;
        while (leiturasEstaveisCount < config.leiturasEstaveis) {
          //informa status com numero de leituras feitas
          String statusMsg = "Registrando tara... Leituras feitas: " + String(leiturasEstaveisCount) + "/" + String(config.leiturasEstaveis);
          broadcastStatus("info", statusMsg);
          if (millis() - inicioTimeout > config.timeoutCalibracao) {
            broadcastStatus("error", "Erro de Timeout ao registrar a tara. Tente a calibração novamente.");
            return;
          }
          if (loadcell.is_ready()) {
            long leituraAtual = loadcell.read_average(config.numAmostrasMedia);
            if (abs(leituraAtual - leituraAnterior) < config.toleranciaEstabilidade) leiturasEstaveisCount++;
            else leiturasEstaveisCount = 0;
            leituraAnterior = leituraAtual;
          }
          delay(100);
        }

        config.tareOffset = loadcell.read_average(20);
        loadcell.set_offset(config.tareOffset);
        
        saveConfig();
        //informar que a nova tara foi salva e que a calibração foi concluída e os valores configurados para a balança
        String statusMsg = "Nova tara registrada com sucesso! Fator de conversão: " + String(config.conversionFactor, 2) + " g/N";
        statusMsg += ", Gravidade: " + String(config.gravity, 2) + " m/s²";
        statusMsg += ", Leituras estáveis: " + String(config.leiturasEstaveis);
        statusMsg += ", Tolerância de estabilidade: " + String(config.toleranciaEstabilidade, 2) + " unidades";
        statusMsg += ", Número de amostras para média: " + String(config.numAmostrasMedia);
        statusMsg += ", Timeout de calibração: " + String(config.timeoutCalibracao) + " ms";
        broadcastStatus("success", statusMsg);
        balancaStatus = "Pronta";
        Serial.println("Calibração e nova tara concluídas com sucesso!");
        //broadcastStatus("success", "Calibração e nova tara concluídas com sucesso!");

      } else if (msg.startsWith("set_param:")) {
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
  EEPROM.begin(sizeof(Config));
  SPIFFS.begin();
  loadConfig();
  loadcell.begin(pinData, pinClock);
  loadcell.set_scale(config.conversionFactor);
  
  if (config.tareOffset != 0) {
    loadcell.set_offset(config.tareOffset);
  } else {
    loadcell.tare();
    config.tareOffset = loadcell.get_offset();
    saveConfig();
  }

  balancaStatus = "Pronta";
  WiFi.mode(WIFI_AP_STA);
  WiFi.softAP(apSSID, apPassword);
  WiFi.begin(config.staSSID, config.staPassword);
  Serial.print("Conectando a rede...");
  unsigned long start = millis();
  while (WiFi.status() != WL_CONNECTED && millis() - start < 10000) {
    delay(500);
    Serial.print(".");
  }
  if (WiFi.status() == WL_CONNECTED) { Serial.println("\nIP: " + WiFi.localIP().toString()); }
  else { Serial.println("\nFalha na conexao."); }

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
}

// ======== LOOP ========
void loop() {
  webSocket.loop();
  server.handleClient();

  if (millis() - lastReadTime >= READ_INTERVAL) {
    if (balancaStatus != "Calibrando" && balancaStatus != "Tarando") {
        if (loadcell.is_ready()) {
          balancaStatus = "Pesando";
          
          // --- CONVERSÃO PARA NEWTONS ---
          float mass_g = loadcell.get_units(5); // Obtém a massa em gramas
          float force_N = (mass_g / 1000.0) * config.gravity; // Converte para Newtons (F = m * g)

          StaticJsonDocument<200> doc;
          doc["type"] = "data";
          doc["tempo"] = millis() / 1000.0;
          doc["forca"] = force_N; // Envia o valor em Newtons
          doc["status"] = balancaStatus;
          String output;
          serializeJson(doc, output);
          webSocket.broadcastTXT(output);
        } else {
           balancaStatus = "Aguardando";
        }
    }
    lastReadTime = millis();
  }
}
