#include <Arduino.h>
#include <HX711.h>
#include <EEPROM.h>
#include <ESP8266WiFi.h>
#include <ESP8266WebServer.h>
#include <WebSocketsServer.h>
#include <FS.h>

// Redes Wi-Fi
const char* apSSID = "balancaESP";
const char* apPassword = "";
const char* staSSID = "BenincaGaspar";
const char* staPassword = "aabbccddee";

// HX711
const uint8_t pinData = D2;   // GPIO4
const uint8_t pinClock = D1;  // GPIO5
HX711 loadcell;

float conversionFactor;
const unsigned long READ_INTERVAL = 10; // leitura a cada 10ms
unsigned long lastReadTime = 0;
float currentForce = 0.0;
bool mostrarEmNewton = false;

ESP8266WebServer server(80);
WebSocketsServer webSocket = WebSocketsServer(81);

void onWebSocketEvent(uint8_t client_num, WStype_t type, uint8_t * payload, size_t length) {
  if (type == WStype_TEXT) {
    String msg = String((char*)payload);
    if (msg == "t") {
      loadcell.tare();
      Serial.println(F("<4,Tara Realizada via WebSocket>"));
    } else if (msg.startsWith("c:")) {
      float massaReferencial = msg.substring(2).toFloat();
      if (massaReferencial <= 0) return;
      long soma = 0;
      int leiturasValidas = 0;
      for (int i = 0; i < 1000; i++) {
        if (loadcell.is_ready()) {
          soma += loadcell.read();
          leiturasValidas++;
        }
        delay(1);
      }
      if (leiturasValidas > 0) {
        float media = soma / (float)leiturasValidas;
        float novoFator = (media - loadcell.get_offset()) / massaReferencial;
        loadcell.set_scale(novoFator);
        conversionFactor = novoFator;
        EEPROM.put(0, conversionFactor);
        EEPROM.commit();
        Serial.println(F("<6,Calibracao com massa conhecida realizada>"));
      }
    }
  }
}

void handleFileRequest() {
  String path = server.uri();
  if (path == "/") path = "/index.html";

  String contentType = "text/plain";
  if (path.endsWith(".html")) contentType = "text/html";
  else if (path.endsWith(".js")) contentType = "application/javascript";
  else if (path.endsWith(".css")) contentType = "text/css";

  File file = SPIFFS.open(path, "r");
  if (!file) {
    server.send(404, "text/plain", "Arquivo não encontrado");
    return;
  }

  server.streamFile(file, contentType);
  file.close();
}

void setup() {
  Serial.begin(115200);
  EEPROM.begin(512);
  SPIFFS.begin();

  loadcell.begin(pinData, pinClock);
  while (!loadcell.is_ready()) delay(100);
  loadcell.tare();

  EEPROM.get(0, conversionFactor);
  if (fabs(conversionFactor) < 1E-10) {
    conversionFactor = 1.0;
    EEPROM.put(0, conversionFactor);
    EEPROM.commit();
  }
  loadcell.set_scale(conversionFactor);

  WiFi.mode(WIFI_AP_STA);
  WiFi.softAP(apSSID, apPassword);
  WiFi.begin(staSSID, staPassword);

  Serial.println("Ponto de acesso criado: " + String(apSSID));
  Serial.println("IP (AP): " + WiFi.softAPIP().toString());
  Serial.print("Conectando-se a " + String(staSSID));
  unsigned long startAttemptTime = millis();
  while (WiFi.status() != WL_CONNECTED && millis() - startAttemptTime < 10000) {
    delay(500);
    Serial.print(".");
  }
  if (WiFi.status() == WL_CONNECTED) {
    Serial.println("\nConectado. IP (STA): " + WiFi.localIP().toString());
  } else {
    Serial.println("\nFalha ao conectar na rede STA");
  }

  server.onNotFound(handleFileRequest);
  server.begin();
  Serial.println("Servidor HTTP iniciado na porta 80");

  webSocket.begin();
  webSocket.onEvent(onWebSocketEvent);
  Serial.println("WebSocket iniciado na porta 81");
}

void loop() {
  webSocket.loop();
  server.handleClient();

  unsigned long currentTime = millis();
  if (currentTime - lastReadTime >= READ_INTERVAL) {
    if (loadcell.is_ready()) {
      currentForce = loadcell.get_units();
      String msg = "{";
      msg += "\"tempo\":" + String(currentTime / 1000.0, 3);
      msg += ",\"forca\":" + String(currentForce, 4);
      msg += "}";
      webSocket.broadcastTXT(msg);
    }
    lastReadTime = currentTime;
  }

  if (Serial.available() > 0) {
    char cmd = Serial.read();
    switch (cmd) {
      case 's':
        conversionFactor = Serial.parseFloat();
        loadcell.set_scale(conversionFactor);
        EEPROM.put(0, conversionFactor);
        EEPROM.commit();
        Serial.println(F("<3,Calibração Atualizada>"));
        break;
      case 'g':
        Serial.print(F("<2,"));
        Serial.print(loadcell.get_scale(), 5);
        Serial.println(F(">"));
        break;
      case 't':
        loadcell.tare();
        Serial.println(F("<4,Tara Realizada>"));
        break;
      default:
        Serial.println(F("<0,Comando Inválido>"));
        break;
    }
    while (Serial.available()) Serial.read();
  }
}
