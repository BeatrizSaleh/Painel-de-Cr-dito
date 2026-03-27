Neste projeto, desenvolvi um dashboard executivo em R Shiny para monitoramento de indicadores de crédito associados ao Banco Central do Brasil. O código foi estruturado para organizar modalidades de crédito da pessoa física, gerar visualizações interativas e oferecer uma navegação analítica dividida entre visão geral, acompanhamento por modalidade e comparação entre séries. Embora a arquitetura esteja preparada para uso com dados reais do SGS, a versão atual utiliza dados simulados para demonstrar o funcionamento da aplicação, da lógica reativa e do design visual do painel.

Principais funcionalidades do projeto
mapeamento de séries econômicas do Banco Central por indicador e modalidade;
organização das modalidades de crédito com metadados visuais e identificadores;
geração de dados simulados para demonstração e testes da aplicação;
criação de gráficos temporais com ggplot2 e interatividade com Plotly;
filtro global por período para atualização dinâmica dos painéis;
aba de visão geral com indicadores macro de crédito;
aba de detalhamento por modalidade de pessoa física;
geração dinâmica de blocos analíticos para cada modalidade;
exportação individual de gráficos em PNG;
exportação consolidada do bloco 2x2 por modalidade;
comparação entre duas modalidades para saldo, concessões, juros e inadimplência;
estrutura pronta para futura integração com dados reais do SGS/BCB

Acesso ao app: https://42qjvw-bia-vs.shinyapps.io/Painel-de-Credito/ 
