#include <Arduino.h>
#include <HX711.h>
#include <EEPROM.h>
#include <ESP8266WiFi.h>
#include <ESP8266WebServer.h>
#include <WebSocketsServer.h>
#include <FS.h>

// ======== CONFIG Wi-Fi ========
const char* apSSID = "balancaESP";
const char* apPassword = "";
char staSSID[32] = "BenincaGaspar";
char staPassword[32] = "aabbccddee";

// ======== HX711 ========
const uint8_t pinData = D2;   // GPIO4
const uint8_t pinClock = D1;  // GPIO5
HX711 loadcell;

float conversionFactor;
const unsigned long READ_INTERVAL = 100; // Aumentado para 100ms para reduzir a carga
unsigned long lastReadTime = 0;
float currentForce = 0.0;

// ======== Servidor e WebSocket ========
ESP8266WebServer server(80);
WebSocketsServer webSocket(81);

// Função auxiliar para enviar mensagens de status via WebSocket
void broadcastStatus(const char* status, const char* message) {
  String msg = "{\"type\":\"status\",\"status\":\"";
  msg += status;
  msg += "\",\"message\":\"";
  msg += message;
  msg += "\"}";
  webSocket.broadcastTXT(msg);
}

// ======== WebSocket: comandos ========
void onWebSocketEvent(uint8_t client_num, WStype_t type, uint8_t * payload, size_t length) {
  if (type != WStype_TEXT) return;

  String msg = String((char*)payload);

  if (msg == "t") {
    loadcell.tare();
    Serial.println(F("<4,Tara via WebSocket>"));
    broadcastStatus("success", "Tara concluída!");
  } else if (msg.startsWith("c:")) {
    float massa = msg.substring(2).toFloat();
    if (massa <= 0) {
      broadcastStatus("error", "Massa de calibração deve ser maior que zero.");
      return;
    }

    // --- LÓGICA DE CALIBRAÇÃO COM ESTABILIDADE ---
    Serial.println(F("Iniciando processo de calibração..."));
    broadcastStatus("info", "Iniciando calibração... Por favor, aguarde o peso estabilizar.");

    const int LEITURAS_ESTAVEIS_NECESSARIAS = 20;
    const float TOLERANCIA_ESTABILIDADE = 5.0; 
    const int NUM_AMOSTRAS_CALIBRACAO = 50;
    const unsigned long TIMEOUT_CALIBRACAO = 15000;

    int leiturasEstaveis = 0;
    long leituraAnterior = 0;
    unsigned long inicioTimeout = millis();

    while (leiturasEstaveis < LEITURAS_ESTAVEIS_NECESSARIAS) {
      if (millis() - inicioTimeout > TIMEOUT_CALIBRACAO) {
        Serial.println(F("<E,Erro: Timeout! A balança não estabilizou.>"));
        broadcastStatus("error", "Erro: Timeout! A balança não estabilizou. Tente novamente.");
        return;
      }

      if (loadcell.is_ready()) {
        long leituraAtual = loadcell.read();
        if (abs(leituraAtual - leituraAnterior) < TOLERANCIA_ESTABILIDADE) {
          leiturasEstaveis++;
        } else {
          leiturasEstaveis = 0;
        }
        leituraAnterior = leituraAtual;
      }
      delay(50);
    }

    Serial.println(F("\nPeso estabilizado! Realizando leitura final..."));
    broadcastStatus("info", "Peso estabilizado. Calculando fator...");

    long somaLeituras = 0;
    int leiturasValidas = 0;
    for (int i = 0; i < NUM_AMOSTRAS_CALIBRACAO; i++) {
      if (loadcell.is_ready()) {
        somaLeituras += loadcell.read();
        leiturasValidas++;
      }
      delay(10);
    }
    
    if (leiturasValidas > 0) {
      float mediaBruta = (float)somaLeituras / leiturasValidas;
      float novoFator = (mediaBruta - loadcell.get_offset()) / massa;
      
      loadcell.set_scale(novoFator);
      conversionFactor = novoFator;
      
      EEPROM.put(0, conversionFactor);
      EEPROM.commit();
      
      String successMsg = "Calibração concluída! Novo fator: " + String(novoFator, 4);
      Serial.println(successMsg);
      broadcastStatus("success", successMsg.c_str());
    } else {
      Serial.println(F("<E,Erro: Não foi possível ler para calibrar.>"));
      broadcastStatus("error", "Erro: Não foi possível realizar a leitura para calibração.");
    }
  }
}

// ======== Servir arquivos do SPIFFS ========
void handleFileRequest() {
  String path = server.uri();
  if (path.endsWith("/")) path += "index.html";

  String contentType = "text/plain";
  if (path.endsWith(".html")) contentType = "text/html";
  else if (path.endsWith(".js")) contentType = "application/javascript";
  else if (path.endsWith(".css")) contentType = "text/css";

  if (SPIFFS.exists(path)) {
    File file = SPIFFS.open(path, "r");
    server.streamFile(file, contentType);
    file.close();
  } else {
    server.send(404, "text/plain", "404: Arquivo Nao Encontrado");
  }
}

// ======== Setup principal ========
void setup() {
  Serial.begin(115200);
  EEPROM.begin(512);
  
  if(!SPIFFS.begin()){
    Serial.println("Erro ao montar SPIFFS");
    return;
  }

  loadcell.begin(pinData, pinClock);
  while (!loadcell.is_ready()) delay(100);
  
  EEPROM.get(0, conversionFactor);
  if (isnan(conversionFactor) || conversionFactor == 0) {
    conversionFactor = 1.0;
  }
  loadcell.set_scale(conversionFactor);
  loadcell.tare(); // Tara inicial

  WiFi.mode(WIFI_AP_STA);
  WiFi.softAP(apSSID, apPassword);
  WiFi.begin(staSSID, staPassword);

  Serial.println("AP IP: " + WiFi.softAPIP().toString());
  unsigned long start = millis();
  while (WiFi.status() != WL_CONNECTED && millis() - start < 10000) {
    delay(500);
    Serial.print(".");
  }
  if (WiFi.status() == WL_CONNECTED) {
    Serial.println("\nConectado a rede STA. IP: " + WiFi.localIP().toString());
  } else {
    Serial.println("\nFalha na conexao STA.");
  }

  // Roteamento
  server.on("/salvarRede", HTTP_POST, []() {
    String ssid = server.arg("ssid");
    String senha = server.arg("senha");
    
    // Para persistir, você deveria salvar ssid e senha na EEPROM aqui.
    // E depois ler no setup().
    
    server.send(200, "text/plain", "Configuracao de rede recebida. O dispositivo sera reiniciado para aplicar.");
    delay(1000);
    ESP.restart();
  });

  server.onNotFound(handleFileRequest);
  server.begin();
  Serial.println("Servidor HTTP iniciado");

  webSocket.begin();
  webSocket.onEvent(onWebSocketEvent);
  Serial.println("WebSocket iniciado");
}

// ======== Loop principal ========
void loop() {
  webSocket.loop();
  server.handleClient();

  unsigned long currentTime = millis();
  if (currentTime - lastReadTime >= READ_INTERVAL) {
    if (loadcell.is_ready()) {
      currentForce = loadcell.get_units(10); // Faz a média de 10 leituras para suavizar
      String msg = "{";
      msg += "\"type\":\"data\"";
      msg += ",\"tempo\":" + String(currentTime / 1000.0, 2);
      msg += ",\"forca\":" + String(currentForce, 3);
      msg += "}";
      webSocket.broadcastTXT(msg);
    }
    lastReadTime = currentTime;
  }

  // Comandos via Serial (mantidos para depuração)
  if (Serial.available()) {
    char cmd = Serial.read();
    switch (cmd) {
      case 't':
        loadcell.tare();
        Serial.println(F("<4,Tara realizada>"));
        break;
      // outros comandos seriais...
    }
    while (Serial.available()) Serial.read();
  }
}
