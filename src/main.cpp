#include <Arduino.h>
#include <Wire.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>
#include <HX711.h>
#include <EEPROM.h>
#include <ESP8266WiFi.h>
#include <ESP8266WebServer.h>
#include <WebSocketsServer.h>
#include <LittleFS.h>
#include <ArduinoJson.h>

// --- CONFIGURAÇÃO DO DISPLAY ---
#define SCREEN_WIDTH 128
#define SCREEN_HEIGHT 64
#define OLED_RESET -1
#define SCREEN_ADDRESS 0x3C
#define OLED_SDA 14 // GPIO14 -> D5
#define OLED_SCL 12 // GPIO12 -> D6
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

// --- PINOS DO SENSOR DE PESO (CONFORME SOLICITADO) ---
// ATENÇÃO: Esta abordagem de alimentar o sensor por GPIOs é instável e não recomendada.
// const uint8_t pinVCC = D1;
// const uint8_t pinGND = D4;
const uint8_t pinData = D7;
const uint8_t pinClock = D8;

// --- OBJETOS GLOBAIS ---
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
// SETUP
// =======================================================
void setup() {
    Serial.begin(115200);
    Serial.println("\nBalança ESP8266 - Versão Final");

    // 1. Inicializa o display
    Wire.begin(OLED_SDA, OLED_SCL);
    if (!display.begin(SSD1306_SWITCHCAPVCC, SCREEN_ADDRESS)) {
        Serial.println(F("Falha ao iniciar display."));
        for (;;);
    }
    atualizarDisplay("Iniciando...", 0);

    // 2. Inicia sistema de arquivos e configurações
    if(!LittleFS.begin()){ Serial.println("Erro ao montar LittleFS."); return; }
    EEPROM.begin(sizeof(Config));
    loadConfig();

    // 3. Inicializa servidores de Rede (antes de hardware sensível)
    WiFi.mode(WIFI_AP_STA);
    WiFi.softAP("balancaESP", "");
    WiFi.begin(config.staSSID, config.staPassword);
    Serial.print("Conectando ao WiFi");
    unsigned long start = millis();
    while (WiFi.status() != WL_CONNECTED && millis() - start < 10000) {
        delay(500); Serial.print(".");
    }
    Serial.println(WiFi.status() == WL_CONNECTED ? "\nConectado!" : "\nFalha ao conectar.");

    server.onNotFound(handleFileRequest);
    server.on("/salvarRede", HTTP_POST, []() {
        strncpy(config.staSSID, server.arg("ssid").c_str(), sizeof(config.staSSID) - 1);
        strncpy(config.staPassword, server.arg("senha").c_str(), sizeof(config.staPassword) - 1);
        saveConfig();
        server.send(200, "text/plain", "Rede salva. Reinicie o dispositivo.");
    });
    server.begin();
    webSocket.onEvent(onWebSocketEvent);
    webSocket.begin();
    Serial.println("Servidores Web e WebSocket iniciados.");

    // 4. ATIVA A ALIMENTAÇÃO DO SENSOR VIA GPIO (Conforme solicitado)
    Serial.println("ATENCAO: Ativando alimentacao do sensor via GPIO (instavel).");
    // pinMode(pinVCC, OUTPUT);
    // digitalWrite(pinVCC, HIGH);
    // pinMode(pinGND, OUTPUT);
    // digitalWrite(pinGND, LOW);
    delay(200); // Pausa para a alimentação do sensor estabilizar

    // 5. Inicializa o sensor
    Serial.println("Iniciando HX711 nos pinos D7 e D8...");
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
    
    balancaStatus = "Pronta";
    atualizarDisplay(balancaStatus, pesoAtual_g);
    Serial.println("\nSetup concluído. Balança pronta para uso.");
}
// =======================================================
// LOOP
// =======================================================
void loop() {
    webSocket.loop();
    server.handleClient();

    if (millis() - lastReadTime >= 300) {
        lastReadTime = millis();
        if (balancaStatus.indexOf("Tarar") != -1 || balancaStatus.indexOf("Calibrar") != -1) return;

        if (loadcell.is_ready()) {
            balancaStatus = "Pesando";
            pesoAtual_g = loadcell.get_units(5);
            if (config.conversionFactor < 0) { pesoAtual_g *= -1; }
            
            StaticJsonDocument<256> doc;
            doc["type"] = "data";
            doc["tempo"] = millis() / 1000.0;
            doc["forca"] = (pesoAtual_g / 1000.0) * config.gravity;
            doc["gramas"] = pesoAtual_g;
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

// =======================================================
// DEFINIÇÃO DAS FUNÇÕES
// =======================================================

void handleFileRequest() {
    String path = server.uri();
    if (path.endsWith(".js")) {
        String pathWithGz = path + ".gz";
        if (LittleFS.exists(pathWithGz)) {
            File file = LittleFS.open(pathWithGz, "rb");
            if (file && !file.isDirectory()) {
                server.sendHeader("Content-Encoding", "gzip");
                server.streamFile(file, "application/javascript");
                file.close();
                return;
            }
        }
    }
    if (path.endsWith("/")) path += "index.html";
    String contentType = "text/html";
    if (LittleFS.exists(path)) {
        File file = LittleFS.open(path, "r");
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
