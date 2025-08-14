# Remoção Automática de Credenciais do GitHub no Windows

Este projeto contém scripts para **remover automaticamente as credenciais do GitHub** armazenadas no **Gerenciador de Credenciais do Windows**, rodando no **logon** e opcionalmente no **logoff/desligar** do computador.

## 📂 Estrutura de Arquivos

- `script_remover_credenciais.ps1` — Script PowerShell que identifica e remove todas as credenciais do GitHub, incluindo variações comuns e cache do GitHub CLI (`gh`).
- `run.bat` — Script em lote que chama o `script_remover_credenciais.ps1` com permissão de execução (`ExecutionPolicy Bypass`).
- `instalar_min.ps1` — Instalador que copia os scripts para `C:\Scripts` e cria um atalho de execução automática no **logon** de todos os usuários.

## 🚀 Como Instalar

1. **Copiar os arquivos para a mesma pasta**
   Coloque os seguintes arquivos juntos:




2. **Abrir o PowerShell como Administrador**
- Pressione `Win + S`, digite **powershell**.
- Clique com o botão direito e selecione **Executar como administrador**.

3. **Permitir execução temporária de scripts**
No PowerShell, execute:
```powershell
Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass

cd "C:\caminho\da\pasta\dos\arquivos"
.\instalar_min.ps1

```




