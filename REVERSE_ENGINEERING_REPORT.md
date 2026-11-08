# REVERSE_ENGINEERING_REPORT — simpler_descriptive_app

## A. Diagnóstico geral

### Arquitetura identificada
Aplicativo R Shiny para pesquisadores explorarem variáveis do SIMPLER (Swedish Infrastructure for Medical Population-Based Life-Course and Environmental Research), Universidade de Uppsala. Estrutura:

- `app.R`: UI, server, logging de uso, opcional login
- `R/`: módulos (explore_variable_module.R, faq_search_block.R, _disable_autoload.R)
- `meta_data/`: listas de variáveis por dataset (11 arquivos CSV)
- `faq/`: conteúdo de FAQ (simpler_faq.csv)
- `renv.lock`: versões exatas dos pacotes R
- `Dockerfile`: imagem para SciLifeLab Serve
- `.github/workflows/docker.yml`: CI para build e publish de imagem Docker

### Tecnologias e dependências
- R 4.3.2
- Pacotes: shiny, shinyjs, shinyWidgets, shinymanager, DT, compareGroups, ggplot2, Hmisc, ComplexUpset, openxlsx, digest, openssl, rgeolocate, jsonlite, htmltools
- pandas, Docker

### Estado de manutenção
Branch default: main. Repositório: https://github.com/simpler4health/simpler_descriptive_app. Build automático via GitHub Actions + GHCR.

### Mecanismos de testes existentes
Nenhum teste automatizado encontrado.

### Limitações da investigação
- R não instalado; app não executado.
- Dados de participantes não incluídos no repositório (criptografados, fornecidos externamente).
- REA 6.7.0 não operacional; análise estática apenas.

## B. Problemas encontrados

| ID | Título | Severidade | Status | Localização |
|----|--------|------------|--------|-------------|
| REA-simpler-001 | Nenhum teste automatizado para o app Shiny | P3 | CONFIRMADO | raiz do repo |
| REA-simpler-002 | Chave de dados lida de arquivo local sem aviso de não commitar | P3 | PROVÁVEL | `app.R:87-92` |

### Detalhes dos problemas confirmados

**REA-simpler-001 (P3, CONFIRMADO)**: Não há testes automatizados para o aplicativo R Shiny (testes de UI, de exportação, de validação de variáveis).

### Detalhes do problema provável

**REA-simpler-002 (P3, PROVÁVEL)**: `app.R:87-92` lê a chave de dados de `overview_data/key_code.txt` se a variável de ambiente não estiver definida. Embora o README avise "Never commit the key", não há proteção no código contra commit acidental.

## C. Problemas recorrentes
Nenhum problema recorrente público encontrado.

## D. Erros não documentados
Todos os itens da seção B são problemas não documentados anteriormente.

## E. Performance
Carregamento de dados único na inicialização (não reativo). Logs de uso retidos por 90 dias com purga automática.

## F. Melhorias técnicas
- Adicionar testes para o app Shiny (REA-simpler-001)
- Adicionar verificação/gitignore para evitar commit da chave (REA-simpler-002)

## G. Plano de execução
Todas independentes e baixo risco.

## H. Resultado esperado
App mais testável, com proteção contra vazamento de credenciais.

**Ferramentas usadas:** git, gh, PowerShell, inspeção estática.  
**Limitação:** R não instalado.  
**Ambiente:** Windows, PowerShell 5.1, `C:\Users\User\audit-repos\simpler_descriptive_app` (clone do upstream, branch main).
