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

// --- CONFIGURAÇÃO CORRETA DO DISPLAY (DO FABRICANTE) ---
#define SCREEN_WIDTH 128
#define SCREEN_HEIGHT 64
#define OLED_RESET -1
#define SCREEN_ADDRESS 0x3C
// Pinos não-padrão para o display I2C. A chamada Wire.begin() vai usá-los.
#define OLED_SDA 14 // GPIO14 -> D5 na placa
#define OLED_SCL 12 // GPIO12 -> D6 na placa
Adafruit_SSD1306 display(SCREEN_WIDTH, SCREEN_HEIGHT, &Wire, OLED_RESET);

// --- ESTRUTURA DE CONFIGURAÇÃO ---
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


// --- PINOS DO SENSOR DE PESO (LIVRES DE CONFLITO) ---
// const uint8_t pinVCC = D1;   //HIGH
// const uint8_t pinGND = D4;    //LOW
const uint8_t pinData =D3 ;
const uint8_t pinClock = D2;

//define o pino de alimentação do HX711 como D0 (GPIO16) para evitar conflitos com o I2C do display


// --- OBJETOS E VARIÁVEIS GLOBAIS ---
HX711 loadcell;
ESP8266WebServer server(80);
WebSocketsServer webSocket(81);
String balancaStatus = "Iniciando...";
float pesoAtual_g = 0.0;
unsigned long lastReadTime = 0;


// --- PROTÓTIPOS DE FUNÇÕES ---
void atualizarDisplay(String status, float peso_em_gramas);
void saveConfig();
void loadConfig();
void broadcastStatus(const char *type, const String &message);
bool aguardarEstabilidade(const String &proposito);
void handleFileRequest();
void onWebSocketEvent(uint8_t client_num, WStype_t type, uint8_t *payload, size_t length);


// =======================================================
// SETUP (CONFIGURAÇÃO INICIAL)
// =======================================================
void setup() {
    Serial.begin(115200);
    Serial.println("\n\nBalança ESP8266 - Versão Estável e Corrigida");

    Wire.begin(OLED_SDA, OLED_SCL);
    if (!display.begin(SSD1306_SWITCHCAPVCC, SCREEN_ADDRESS)) {
        Serial.println(F("Falha ao iniciar display Adafruit SSD1306."));
        for (;;);
    }
    atualizarDisplay("Iniciando...", 0);

    EEPROM.begin(sizeof(Config));
    SPIFFS.begin();
    loadConfig();
    
    // O bloco de alimentação por GPIO foi removido por segurança e estabilidade.
    // pinMode(pinVCC, OUTPUT);
    // pinMode(pinGND, OUTPUT);
    // digitalWrite(pinVCC, HIGH); // Liga o VCC do HX711
    // digitalWrite(pinGND, LOW);  // Liga o GND do HX711

    Serial.println("Iniciando HX711 nos pinos D1 e D2...");
    loadcell.begin(pinData, pinClock);
    loadcell.set_scale(config.conversionFactor);
    loadcell.set_offset(config.tareOffset);

    if (config.tareOffset == 0) {
        if (aguardarEstabilidade("Tara Inicial")) {
            loadcell.tare(20);
            config.tareOffset = loadcell.get_offset();
            saveConfig();
        } else {
            Serial.println("FALHA na tara inicial.");
        }
    }

    atualizarDisplay("Conectando WiFi...", 0);
    WiFi.mode(WIFI_AP_STA);
    WiFi.softAP("balancaESP", "");
    WiFi.begin(config.staSSID, config.staPassword);
    
    unsigned long start = millis();
    while (WiFi.status() != WL_CONNECTED && millis() - start < 10000) {
        delay(500);
        Serial.print(".");
    }

    server.on("/salvarRede", HTTP_POST, []() {
        strncpy(config.staSSID, server.arg("ssid").c_str(), sizeof(config.staSSID) - 1);
        strncpy(config.staPassword, server.arg("senha").c_str(), sizeof(config.staPassword) - 1);
        saveConfig();
        server.send(200, "text/plain", "Rede salva. Reinicie.");
    });
    server.onNotFound(handleFileRequest);
    server.begin();

    webSocket.onEvent(onWebSocketEvent);
    webSocket.begin();

    balancaStatus = "Pronta";
    atualizarDisplay(balancaStatus, pesoAtual_g);
    Serial.println("\nSetup concluído. Balança pronta.");
}
// =======================================================
// LOOP (CÓDIGO PRINCIPAL)
// =======================================================
void loop() {
    webSocket.loop();
    server.handleClient();

    if (millis() - lastReadTime >= 300) {
        lastReadTime = millis();
        
        if (balancaStatus.indexOf("Tarar") != -1 || balancaStatus.indexOf("Calibrar") != -1) {
            return;
        }

        if (loadcell.is_ready()) {
            balancaStatus = "Pesando";
            pesoAtual_g = loadcell.get_units(5);
            if (config.conversionFactor < 0) {
                pesoAtual_g *= -1;
            }
            
            StaticJsonDocument<200> doc;

            
            doc["type"] = "data";
            doc["tempo"] = millis() / 1000.0;
            // Converte a massa (em kg) para força (em Newtons) usando F = m * g
            doc["forca"] = (pesoAtual_g / 1000.0) * config.gravity; 
            doc["status"] = balancaStatus;
            String output;
            serializeJson(doc, output);
            webSocket.broadcastTXT(output);

            atualizarDisplay(balancaStatus, pesoAtual_g);
        } else {
            broadcastStatus("info", "Aguardando celula...");
        }
    }
}


// =======================================================
// DEFINIÇÃO DAS FUNÇÕES
// =======================================================

void atualizarDisplay(String status, float peso_em_gramas) {
    display.clearDisplay();
    display.setTextColor(SSD1306_WHITE);

    // Status
    display.setTextSize(1);
    display.setCursor(0, 0);
    display.println("Status:");
    display.setCursor(0, 12);
    display.println(status);

    // Peso
    display.setTextSize(2);
    display.setCursor(0, 30);
    display.print(peso_em_gramas / 1000.0, 3); // Exibe em kgf com 3 decimais
    display.print(" kgf");

    // IP
    display.setTextSize(1);
    display.setCursor(0, 56);
    display.print("IP: ");
    display.print(WiFi.localIP());

    display.display();
}

void saveConfig() {
    EEPROM.put(0, config);
    EEPROM.commit();
}

void loadConfig() {
    EEPROM.get(0, config);
    if (config.magic_number != 123456789) {
        Serial.println("Configuracao invalida, carregando padroes.");
        config = Config();
        saveConfig();
    } else {
        Serial.println("Configuracao carregada da EEPROM.");
    }
}

void broadcastStatus(const char *type, const String &message) {
    balancaStatus = message;
    atualizarDisplay(balancaStatus, pesoAtual_g);
    StaticJsonDocument<256> doc;
    doc["type"] = "status";
    doc["status"] = type;
    doc["message"] = message;
    String output;
    serializeJson(doc, output);
    webSocket.broadcastTXT(output);
}

bool aguardarEstabilidade(const String &proposito) {
    if (!loadcell.is_ready()) {
        broadcastStatus("error", "Célula de carga não pronta para iniciar.");
        return false;
    }
    
    unsigned long inicioTimeout = millis();
    long leituraAnterior = loadcell.read_average(10);
    int leiturasEstaveisCount = 0;
    
    while (leiturasEstaveisCount < config.leiturasEstaveis) {
        // Verifica o timeout
        if (millis() - inicioTimeout > 30000) { // Usando o timeout de 30s
            broadcastStatus("error", "Timeout! A balança não estabilizou a tempo.");
            balancaStatus = "Pronta"; // Reseta o status global após a falha
            return false;
        }

        // Verifica se a célula está pronta DENTRO do loop
        if (loadcell.is_ready()) {
            long leituraAtual = loadcell.read_average(10);
            String statusProgresso = proposito + " (" + String(leiturasEstaveisCount + 1) + "/" + String(config.leiturasEstaveis) + ")";
            
            // Usa a função broadcastStatus para atualizar TANTO o display OLED QUANTO a interface web
            broadcastStatus("info", statusProgresso); 
            
            // Compara com a leitura anterior para ver se está estável
            if (abs(leituraAtual - leituraAnterior) < config.toleranciaEstabilidade) {
                leiturasEstaveisCount++;
            } else {
                // Se não estiver estável, reseta a contagem
                leiturasEstaveisCount = 0;
            }
            leituraAnterior = leituraAtual;
        } else {
            // Se a célula ficar "não pronta", informa o usuário em vez de travar em silêncio
            broadcastStatus("info", "Aguardando célula... (" + proposito + ")");
        }
        
        delay(200); // Delay para não sobrecarregar o processador e evitar reset
    }
    
    broadcastStatus("success", "Estabilizado para " + proposito + "!");
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
        server.streamFile(file, contentType);
        file.close();
    } else {
        server.send(404, "text/plain", "404: Not Found");
    }
}

void onWebSocketEvent(uint8_t client_num, WStype_t type, uint8_t *payload, size_t length) {
    switch (type) {
    case WStype_DISCONNECTED:
        Serial.printf("[%u] Desconectado!\n", client_num);
        break;
    case WStype_CONNECTED: {
        IPAddress ip = webSocket.remoteIP(client_num);
        Serial.printf("[%u] Conectado de %s\n", client_num, ip.toString().c_str());
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
        String msg = String((char *)payload);

        if (msg == "t") {
            if (aguardarEstabilidade("Tarar")) {
                loadcell.tare(20);
                config.tareOffset = loadcell.get_offset();
                saveConfig();
                broadcastStatus("success", "Tara concluída com sucesso!");
            } else {
                broadcastStatus("error", "Falha ao tarar. Balança não estabilizou.");
            }
        } 
        // LÓGICA DE CALIBRAÇÃO CORRIGIDA
        else if (msg.startsWith("c:")) {
            float massa_g = msg.substring(2).toFloat();
            if (massa_g <= 0) {
                broadcastStatus("error", "Massa de calibração deve ser maior que 0.");
                return;
            }
            if (!aguardarEstabilidade("Calibrar")) {
                balancaStatus = "Pronta";
                return;
            }
            
            // Lógica de cálculo corrigida
            long leituraComPesoRaw = loadcell.read_average(20);
            long offset = loadcell.get_offset();
            config.conversionFactor = (float)(leituraComPesoRaw - offset) / massa_g;
            
            loadcell.set_scale(config.conversionFactor);
            saveConfig();
            
            broadcastStatus("success", "Balança calibrada com sucesso!");
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
