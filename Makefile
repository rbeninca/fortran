# Compilador
FC = gfortran

# Flags de compilação
FFLAGS = -Wall

# Diretórios
SRC_DIR = src
BIN_DIR = bin

# Arquivos fonte
SOURCES = $(wildcard $(SRC_DIR)/*.f90)

# Gerar lista de executáveis removendo a extensão
TARGETS = $(patsubst $(SRC_DIR)/%.f90, $(BIN_DIR)/%, $(SOURCES))

# Alvo padrão: compila tudo e roda o binário mais recente
all: ensure_dirs $(TARGETS)
	@echo "Executando o binário mais recente..."
	@LAST_SRC=$$(ls -t $(SOURCES) | head -n1); \
	PROG_NAME=$$(basename $$LAST_SRC .f90); \
	EXEC_FILE="$(BIN_DIR)/$$PROG_NAME"; \
	if [ -x $$EXEC_FILE ]; then \
	  echo ">>> Rodando $$EXEC_FILE..."; \
	  $$EXEC_FILE; \
	else \
	  echo "Executável não encontrado: $$EXEC_FILE"; \
	fi

# Compila cada arquivo fonte para a pasta bin
$(BIN_DIR)/%: $(SRC_DIR)/%.f90
	$(FC) $(FFLAGS) -o $@ $<

# Cria as pastas se não existirem
ensure_dirs:
	@mkdir -p $(SRC_DIR)
	@mkdir -p $(BIN_DIR)

# Limpa binários
clean:
	rm -f $(BIN_DIR)/*

# Ajuda
help:
	@echo "Comandos disponíveis:"
	@echo "  make         --> Compila e executa o mais recente"
	@echo "  make clean   --> Limpa executáveis"
	@echo "  make help    --> Mostra esta mensagem"
