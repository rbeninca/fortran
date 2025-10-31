#!/bin/bash
# Sincroniza data/ → docs/ para GitHub Pages
# Mantém código web em fonte única

set -e  # Para em caso de erro

echo "📦 Sincronizando arquivos web data/ → docs/"
echo ""

# Arquivos web para sincronizar
WEB_FILES=(
    "index.html"
    "rede.html"
    "script.js"
    "dataWorker.js"
    "script_grafico_sessao.js"
    "funcoespdf.js"
    "estilo.css"
    "chartist.min.css"
    "chartist.min.js"
    "apexcharts"
)

# Conta arquivos sincronizados
SYNC_COUNT=0
SKIP_COUNT=0

# Copia cada arquivo
for file in "${WEB_FILES[@]}"; do
    SOURCE="data/$file"
    DEST="docs/$file"
    
    if [ -e "$SOURCE" ]; then
        # Verifica se é arquivo ou diretório
        if [ -d "$SOURCE" ]; then
            # É diretório - copia recursivamente
            cp -r "$SOURCE" "$DEST"
            echo "  ✓ $file (diretório)"
        else
            # É arquivo - copia direto
            cp "$SOURCE" "$DEST"
            echo "  ✓ $file"
        fi
        ((SYNC_COUNT++))
    else
        echo "  ⚠ $file (não encontrado em data/)"
        ((SKIP_COUNT++))
    fi
done

echo ""
echo "✅ Sincronização completa!"
echo "   Sincronizados: $SYNC_COUNT arquivos"
if [ $SKIP_COUNT -gt 0 ]; then
    echo "   Ignorados: $SKIP_COUNT arquivos (não encontrados)"
fi

# Adiciona arquivo de build info
BUILD_INFO="docs/BUILD_INFO.txt"
cat > "$BUILD_INFO" << EOF
=========================================
  ESTE DIRETÓRIO É GERADO AUTOMATICAMENTE
=========================================

Fonte: data/
Destino: docs/ (GitHub Pages)

⚠️  NÃO EDITE ARQUIVOS AQUI DIRETAMENTE!
    Suas mudanças serão sobrescritas.

📝  Para fazer alterações:
    1. Edite arquivos em data/
    2. Execute ./scripts/sync-docs.sh
    3. Commit ambas as pastas

Build: $(date '+%Y-%m-%d %H:%M:%S')
Host: $(hostname)
User: $(whoami)

=========================================
EOF

echo ""
echo "📄 Arquivo BUILD_INFO.txt criado em docs/"
echo ""
echo "🎯 Próximos passos:"
echo "   git add docs/"
echo "   git commit -m 'chore: sync docs from data'"
