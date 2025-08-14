<#
    Remove TODAS as credenciais do GitHub armazenadas no Gerenciador de Credenciais do Windows.
    - Procura por qualquer "Alvo" que contenha "github.com" (maiúsculas/minúsculas ignoradas).
    - Apaga entradas comuns/legadas usadas pelo Git Credential Manager.
    - (Opcional) Limpa o cache do GitHub CLI (gh) no perfil do usuário.
    - (Opcional) Limpa o cache Web do Edge/IE (cookies/histórico).

    Recomendado para execução em evento de Logoff do usuário.
#>

# 1) Procurar e remover qualquer credencial cujo alvo contenha "github.com"
$alvosEncontrados = cmdkey /list |                                    # Lista todas as credenciais salvas no Windows
    Select-String -Pattern 'Target:\s*(.+)$' |                        # Filtra apenas as linhas que possuem "Target:"
    ForEach-Object {                                                  # Percorre cada linha filtrada
        $alvo = $_.Matches[0].Groups[1].Value.Trim()                  # Extrai e limpa o nome do alvo
        if ($alvo -match '(?i)github\.com') { $alvo }                  # Se o alvo contiver "github.com" (ignorando maiúsculas/minúsculas), retorna
    } |
    Sort-Object -Unique                                                # Remove duplicados e ordena

foreach ($alvo in $alvosEncontrados) {                                 # Para cada alvo encontrado
    Write-Host "Removendo credencial: $alvo"                           # Mostra no console qual credencial está sendo removida
    cmdkey /delete:$alvo | Out-Null                                    # Remove a credencial do Gerenciador de Credenciais
}

# 2) Remover credenciais comuns/legadas do GitHub
$alvosComuns = @(                                                     # Lista de possíveis nomes usados pelo Git Credential Manager
    'git:https://github.com',
    'git:http://github.com',
    'git:github.com',
    'legacy:https://github.com',
    'legacy:http://github.com'
) | Sort-Object -Unique                                               # Ordena e remove duplicados

foreach ($alvo in $alvosComuns) {                                     # Para cada alvo da lista
    cmdkey /delete:$alvo 2>$null | Out-Null                           # Remove sem exibir erros no console
}

# 3) (Opcional) Limpar cache do GitHub CLI (gh) no perfil do usuário
$pastasCacheGH = @(                                                   # Lista de pastas onde o GitHub CLI armazena configurações
    (Join-Path $env:USERPROFILE '.config\gh'),                        # Pasta padrão no perfil do usuário
    (Join-Path $env:APPDATA 'gh')                                     # Pasta no AppData
)

foreach ($pasta in $pastasCacheGH) {                                  # Para cada pasta de cache
    $arquivoHosts = Join-Path $pasta 'hosts.yml'                      # Arquivo de credenciais do GitHub CLI
    if (Test-Path $arquivoHosts) {                                    # Se o arquivo existir
        Write-Host "Removendo cache do GitHub CLI: $arquivoHosts"     # Mostra no console qual arquivo será removido
        Remove-Item $arquivoHosts -Force -ErrorAction SilentlyContinue # Remove o arquivo de forma silenciosa
    }
}

# 4) (Opcional) Limpar entradas do cache web do Edge/IE
Stop-Process -Name "iexplore","msedge","msedgewebview2" -ErrorAction SilentlyContinue  # Encerra processos do IE/Edge/WebView2 (se estiverem abertos)
RunDll32.exe InetCpl.cpl,ClearMyTracksByProcess 2                                     # Limpa cookies e cache do IE/Edge

