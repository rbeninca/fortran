# Compilador
FC = gfortran

# Flags de compilação
FFLAGS =  -Wall -cpp

# Diretórios
SRC_DIR = src
BIN_DIR = bin

# Busca recursiva por arquivos .f90, cuidando de espaços
SOURCES = $(shell find "$(SRC_DIR)" -name '*.f90')

# Cria os nomes dos binários, pegando só o nome base
TARGETS = $(patsubst %.f90, $(BIN_DIR)/%, $(notdir $(SOURCES)))

# Alvo padrão: compila tudo e executa o último
all:  clean ensure_dirs $(TARGETS)
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
$(BIN_DIR)/%:
	@SRC_FILE=$$(find "$(SRC_DIR)" -name "$*.f90"); \
	if [ -z "$$SRC_FILE" ]; then \
	  echo "Erro: Não foi encontrado o arquivo para $@"; \
	  exit 1; \
	fi; \
	echo "Compilando $$SRC_FILE -> $@"; \
	$(FC) $(FFLAGS) -o "$@" "$$SRC_FILE"

# Cria as pastas se não existirem
ensure_dirs:
	@mkdir -p "$(SRC_DIR)"
	@mkdir -p "$(BIN_DIR)"

# Limpa binários
clean:
	rm -f "$(BIN_DIR)"/*

# Ajuda
help:
	@echo "Comandos disponíveis:"
	@echo "  make         --> Compila e executa o mais recente"
	@echo "  make clean   --> Limpa executáveis"
	@echo "  make help    --> Mostra esta mensagem"
