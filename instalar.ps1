# INSTALADOR MINIMAL: copia scripts para C:\Scripts e cria atalho de logon (pasta Inicializar - Todos os Usuários)

$PastaDestino = "C:\Scripts"                                                           # pasta onde ficarão os scripts
New-Item -ItemType Directory -Path $PastaDestino -Force | Out-Null                     # cria C:\Scripts (se já existir, só garante)

Copy-Item -Path ".\run.bat" -Destination (Join-Path $PastaDestino "run.bat") -Force    # copia run.bat para C:\Scripts
Copy-Item -Path ".\script_remover_credenciais.ps1" `
          -Destination (Join-Path $PastaDestino "script_remover_credenciais.ps1") `
          -Force                                                                        # copia o .ps1 para C:\Scripts

$PastaInicializarTodos = "$env:ProgramData\Microsoft\Windows\Start Menu\Programs\StartUp" # pasta Inicializar (Todos os usuários)
$Wsh = New-Object -ComObject WScript.Shell                                              # cria objeto COM para gerar atalho (.lnk)
$Atalho = $Wsh.CreateShortcut((Join-Path $PastaInicializarTodos "LimparCredenciais-GitHub.lnk")) # caminho do atalho
$Atalho.TargetPath = (Join-Path $PastaDestino "run.bat")                                # executável do atalho = run.bat em C:\Scripts
$Atalho.WorkingDirectory = $PastaDestino                                                # pasta de trabalho do atalho
$Atalho.Save()                                                                          # grava o atalho

Write-Host "Copiado para $PastaDestino e atalho de LOGON criado em 'Inicializar (Todos)'." # mensagem final

