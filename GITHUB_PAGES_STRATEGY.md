# Estratégia GitHub Pages - Eliminando Duplicação de Código

## 📋 Problema Atual

Atualmente, o código web está **duplicado** em duas pastas:
- `data/` - Código usado pelo servidor ESP32/PlatformIO
- `docs/` - Código servido pelo GitHub Pages (cópia idêntica)

**Problemas:**
1. ❌ Manutenção duplicada - toda mudança precisa ser copiada
2. ❌ Risco de inconsistência entre as duas versões
3. ❌ Espaço desperdiçado no repositório
4. ❌ Commits duplicados em ambas as pastas

---

## 🎯 Soluções Possíveis

### **Opção 1: Build Script Automatizado** ⭐ (RECOMENDADO)

Manter apenas `data/` como fonte única da verdade e gerar `docs/` automaticamente.

#### Estrutura:
```
balanca-web/
├── data/                     # ← Fonte única (desenvolvimento)
│   ├── index.html
│   ├── script.js
│   ├── dataWorker.js
│   └── estilo.css
├── docs/                     # ← Gerado automaticamente (GitHub Pages)
│   └── (conteúdo copiado de data/)
├── scripts/
│   ├── build-docs.sh         # Script de build
│   └── sync-docs.js          # Alternativa em Node.js
└── .github/
    └── workflows/
        └── deploy-docs.yml   # CI/CD automático
```

#### Script de Build (`scripts/build-docs.sh`):
```bash
#!/bin/bash
# Build script para GitHub Pages

echo "🔄 Sincronizando data/ → docs/"

# Limpa docs/ (exceto .git se existir)
rm -rf docs/*

# Copia todos os arquivos web
cp -r data/* docs/

# Adiciona banner identificando que é build gerado
cat > docs/BUILD_INFO.txt << EOF
Este diretório é gerado automaticamente de data/
NÃO edite diretamente - suas mudanças serão sobrescritas!
Build: $(date)
EOF

echo "✅ Build concluído!"
```

#### CI/CD Automático (`.github/workflows/deploy-docs.yml`):
```yaml
name: Deploy GitHub Pages

on:
  push:
    branches: [ main ]
    paths:
      - 'data/**'
  workflow_dispatch:

jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Build docs from data
        run: |
          chmod +x scripts/build-docs.sh
          ./scripts/build-docs.sh
      
      - name: Commit and push docs
        run: |
          git config user.name "GitHub Actions"
          git config user.email "actions@github.com"
          git add docs/
          git diff --staged --quiet || git commit -m "chore: auto-build docs/ from data/"
          git push
```

**Vantagens:**
- ✅ Fonte única em `data/`
- ✅ Automático via GitHub Actions
- ✅ Sem intervenção manual
- ✅ Histórico claro de builds

---

### **Opção 2: Configurar GitHub Pages para Servir de `data/`**

GitHub Pages permite configurar a pasta de publicação.

#### Como Configurar:
1. Vá em **Settings** → **Pages**
2. Em **Source**, selecione a branch `main`
3. Em **Folder**, você pode escolher:
   - `/` (raiz)
   - `/docs`
   - **Não há opção `/data` diretamente** ❌

**Limitação:** GitHub Pages **só aceita `/` ou `/docs`** como pasta de publicação.

#### Solução Alternativa - Reorganizar:
```
balanca-web/
├── web/                      # Todo código web aqui
│   ├── index.html
│   ├── script.js
│   └── ...
├── embedded/
│   └── data/                 # Symlink para web/
├── server/
│   ├── server.py
│   └── docker-compose.yml
└── firmware/
    ├── src/
    └── platformio.ini
```

Configurar GitHub Pages para servir `/web`.

**Problema:** PlatformIO espera pasta `data/` na raiz. ❌

---

### **Opção 3: Usar Submodule Git** (Complexo)

Criar repositório separado só para web e incluir como submodule.

```
balanca-web/                  # Repositório principal
├── data/ → submodule         # Aponta para balanca-web-ui
└── docs/ → submodule         # Mesmo submodule

balanca-web-ui/               # Repositório só com UI
├── index.html
├── script.js
└── ...
```

**Desvantagens:**
- ❌ Complexidade aumentada
- ❌ Dois repositórios para gerenciar
- ❌ Difícil para contribuidores

---

### **Opção 4: Symlink no Repositório** (Não funciona no GitHub)

```bash
ln -s ../data docs
```

**Problema:** Git não suporta symlinks corretamente no Windows e GitHub Pages não segue symlinks. ❌

---

## 🏆 Solução Recomendada

### **Build Script Manual + Hook de Pre-commit**

Enquanto não configuramos CI/CD, podemos usar um hook Git que atualiza `docs/` automaticamente.

#### 1. Criar Script de Build:

**`scripts/sync-docs.sh`:**
```bash
#!/bin/bash
# Sincroniza data/ → docs/

echo "📦 Sincronizando arquivos web..."

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

# Copia cada arquivo
for file in "${WEB_FILES[@]}"; do
    if [ -f "data/$file" ]; then
        cp "data/$file" "docs/$file"
        echo "  ✓ $file"
    fi
done

echo "✅ Sincronização completa!"
```

#### 2. Adicionar ao `.git/hooks/pre-commit`:

```bash
#!/bin/bash
# Hook pre-commit - sincroniza data/ → docs/

# Verifica se houve mudanças em data/
if git diff --cached --name-only | grep -q "^data/"; then
    echo "🔄 Detectadas mudanças em data/, sincronizando para docs/..."
    ./scripts/sync-docs.sh
    
    # Adiciona arquivos atualizados ao commit
    git add docs/
    echo "✅ docs/ atualizado automaticamente"
fi
```

#### 3. Instalação:

```bash
# Torna o script executável
chmod +x scripts/sync-docs.sh

# Copia o hook
cp scripts/pre-commit-hook .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit
```

**Vantagens:**
- ✅ Automático a cada commit
- ✅ Simples de configurar
- ✅ Não depende de CI/CD
- ✅ Funciona offline

---

## 📝 Workflow Recomendado

### Desenvolvimento:

1. **Edite apenas em `data/`**:
   ```bash
   vim data/script.js
   ```

2. **Teste localmente** (servidor Python ou ESP32):
   ```bash
   python server.py
   # ou PlatformIO Upload Filesystem
   ```

3. **Commit** (hook sincroniza automaticamente):
   ```bash
   git add data/script.js
   git commit -m "feat: adicionar recurso X"
   # Hook copia para docs/ automaticamente
   git push
   ```

4. **GitHub Pages** atualiza automaticamente de `docs/`

### Comando Manual (se precisar):

```bash
# Sincronizar manualmente
./scripts/sync-docs.sh
git add docs/
git commit -m "chore: sync docs from data"
```

---

## 🔍 Detecção de Ambiente

O código já detecta se está rodando no GitHub Pages:

```javascript
// dataWorker.js e script.js
if (location.hostname.includes('github.io')) {
    console.log("Modo GitHub Pages - recursos offline");
    // Desabilita WebSocket
    // Desabilita salvamento no servidor
    // Habilita modo visualização
}
```

**Funcionalidades em GitHub Pages:**
- ✅ Visualização de interface
- ✅ Conversões de unidades (N, gf, kgf)
- ✅ Ferramentas de cálculo offline
- ✅ Visualização de dados importados (JSON)
- ❌ Conexão com ESP32/servidor
- ❌ Salvamento em MySQL
- ❌ WebSocket tempo real

---

## 🎨 Alternativa: Branch Separada para GitHub Pages

Criar branch `gh-pages` específica:

```bash
# Criar branch órfã (sem histórico)
git checkout --orphan gh-pages

# Limpar tudo
git rm -rf .

# Copiar apenas arquivos web
cp -r ../main-branch/data/* .

# Commit e push
git add .
git commit -m "Initial GitHub Pages"
git push -u origin gh-pages
```

Configurar GitHub Pages para usar branch `gh-pages` em vez de `/docs`.

**Desvantagens:**
- Duas branches para manter
- Merge manual necessário

---

## 📊 Comparação de Soluções

| Solução | Complexidade | Automação | Manutenção | Recomendado |
|---------|--------------|-----------|------------|-------------|
| Build Script + Hook | ⭐⭐ Baixa | ⭐⭐⭐ Alta | ⭐⭐⭐ Fácil | ✅ Sim |
| GitHub Actions CI/CD | ⭐⭐⭐ Média | ⭐⭐⭐⭐ Total | ⭐⭐⭐⭐ Mínima | ✅ Ideal longo prazo |
| Reconfigurar pastas | ⭐⭐⭐⭐ Alta | ⭐ Nenhuma | ⭐⭐ Difícil | ❌ Não |
| Submodule Git | ⭐⭐⭐⭐⭐ Muito Alta | ⭐⭐ Média | ⭐ Muito Difícil | ❌ Não |
| Branch gh-pages | ⭐⭐⭐ Média | ⭐ Baixa | ⭐⭐ Difícil | ⚠️ Alternativa |

---

## 🚀 Plano de Implementação Gradual

### Fase 1 - Imediato (Manual):
```bash
# Criar script de sincronização
./scripts/sync-docs.sh

# Usar manualmente antes de cada commit importante
```

### Fase 2 - Curto Prazo (Hook):
```bash
# Instalar hook pre-commit
cp scripts/pre-commit-hook .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit

# Sincronização automática a cada commit
```

### Fase 3 - Médio Prazo (CI/CD):
```yaml
# Adicionar GitHub Actions workflow
.github/workflows/deploy-docs.yml

# Build automático a cada push em data/
```

### Fase 4 - Longo Prazo (Otimização):
- Minificação de JS/CSS para GitHub Pages
- Compressão de assets
- Service Worker para cache offline
- PWA para uso como aplicativo

---

## 📖 Documentação do Script

### Criar `scripts/README.md`:

```markdown
# Scripts de Build

## sync-docs.sh
Sincroniza arquivos de `data/` para `docs/` (GitHub Pages).

### Uso:
```bash
./scripts/sync-docs.sh
```

### Arquivos sincronizados:
- HTML: index.html, rede.html
- JavaScript: script.js, dataWorker.js, etc.
- CSS: estilo.css, chartist.min.css
- Assets: apexcharts, etc.

### Quando executar:
- Automaticamente via hook pre-commit
- Manualmente antes de push importante
- Após mudanças significativas em data/
```

---

## ✅ Checklist de Migração

- [ ] Criar pasta `scripts/`
- [ ] Criar `scripts/sync-docs.sh`
- [ ] Testar sincronização manual
- [ ] Criar hook pre-commit
- [ ] Testar hook com commit
- [ ] Documentar processo no README.md
- [ ] (Opcional) Configurar GitHub Actions
- [ ] (Opcional) Adicionar badge de build no README

---

## 🔗 Recursos Adicionais

- [GitHub Pages Documentation](https://docs.github.com/en/pages)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Git Hooks Documentation](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks)
