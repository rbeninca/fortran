# Compilador
FC = gfortran

# Flags de compilação
FFLAGS =  -Wall -cpp -I modules

# Diretórios
SRC_DIR = src
BIN_DIR = bin
MOD_DIR = modules

# Busca recursiva por arquivos .f90, exceto portlib
SOURCES = $(shell find "$(SRC_DIR)" -name '*.f90' ! -path "$(SRC_DIR)/marchi/*")

# Cria os nomes dos binários, pegando só o nome base
TARGETS = $(patsubst %.f90, $(BIN_DIR)/%, $(notdir $(SOURCES)))

# Alvo padrão: compila tudo e executa o último
all: ensure_dirs $(MOD_DIR)/portlib.mod $(TARGETS)
	@echo "Executando o binário mais recente..."
	@LAST_SRC=$$(ls -t $(SOURCES) | head -n1); \
	PROG_NAME=$$(basename "$$LAST_SRC" .f90); \
	EXEC_FILE="$(BIN_DIR)/$$PROG_NAME"; \
	if [ -x "$$EXEC_FILE" ]; then \
	  echo ">>> Rodando $$EXEC_FILE..."; \
	  "$$EXEC_FILE"; \
	else \
	  echo "Executável não encontrado: $$EXEC_FILE"; \
	fi

# Compila cada arquivo fonte para a pasta bin
$(BIN_DIR)/%: $(MOD_DIR)/portlib.mod
	@SRC_FILE=$$(find "$(SRC_DIR)" -name "$*.f90" ! -path "$(SRC_DIR)/marchi/*"); \
	if [ -z "$$SRC_FILE" ]; then \
	  echo "Erro: Não foi encontrado o arquivo para $@"; \
	  exit 1; \
	fi; \
	echo "Compilando $$SRC_FILE -> $@"; \
	$(FC) $(FFLAGS) -o "$@" "$$SRC_FILE" modules/portlib.o

# Compila portlib.f90 como módulo
$(MOD_DIR)/portlib.mod: $(MOD_DIR)/portlib.f90
	@echo "Compilando módulo portlib -> $@"
	$(FC) -c $< -J $(MOD_DIR) -o $(MOD_DIR)/portlib.o

# Cria as pastas se não existirem

ensure_dirs:
	@mkdir -p "$(SRC_DIR)"
	@mkdir -p "$(BIN_DIR)"
	@mkdir -p "$(MOD_DIR)"

# Limpa binários e apenas arquivos gerados em modules/
clean:
	rm -f "$(BIN_DIR)"/*
	rm -f "$(MOD_DIR)"/portlib.mod
	rm -f "$(MOD_DIR)"/portlib.o

# Ajuda
help:
	@echo "Comandos disponíveis:"
	@echo "  make         --> Compila e executa o mais recente"
	@echo "  make clean   --> Limpa executáveis e arquivos gerados de módulo"
	@echo "  make help    --> Mostra esta mensagem"
