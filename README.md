# ENSExtended rv project #

0. Link da apresentação da metodologia utilizada para a remoção de viés da
   Previsão de Precipitação para o Primeiro Mês da Operação (previsão estedida do ECMWF: 2a Reunião do GT HM 25/01/2022):

   [Vídeo da Apresentação](https://ctpmopld.org.br/documents/33692/411965/2a_Reuni%C3%A3o_GT+HM_Prev1%C2%B0m%C3%AAs_20220125.mp4/0068932d-e201-eccc-f1be-c9beb07eefda?version=1.0&t=1643807101432&download=true)

    Link da apresentação sobre a remoção de viés da
   Previsão de Precipitação para o Primeiro Mês da Operação (previsão estedida do ECMWF: 2a Reunião do GT HM 25/01/2022):

    [Apresentação](https://ctpmopld.org.br/group/ct-pmo-pld/gt-dados-hidrometeorol%C3%B3gicos/-/document_library/OVp6moe0yNrQ/view_file/411955?_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_OVp6moe0yNrQ_redirect=https%3A%2F%2Fctpmopld.org.br%2Fgroup%2Fct-pmo-pld%2Fgt-dados-hidrometeorol%25C3%25B3gicos%2F-%2Fdocument_library%2FOVp6moe0yNrQ%2Fview%2F411941%3F_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_OVp6moe0yNrQ_redirect%3Dhttps%253A%252F%252Fctpmopld.org.br%252Fgroup%252Fct-pmo-pld%252Fgt-dados-hidrometeorol%2525C3%2525B3gicos%253Fp_p_id%253Dcom_liferay_document_library_web_portlet_DLPortlet_INSTANCE_OVp6moe0yNrQ%2526p_p_lifecycle%253D0%2526p_p_state%253Dnormal%2526p_p_mode%253Dview)

# novidades (atualizacao 29/04/2025):
- atualizacao do arquivo Configuracao.xlsx que permite rodar PSATSALT (Salto)

Mais detalhes da versao do R e "attached base packages"
```
> R.version
platform       x86_64-w64-mingw32
arch           x86_64
os             mingw32
crt            ucrt
system         x86_64, mingw32
status
major          4
minor          2.0
year           2022
month          04
day            22
svn rev        82229
language       R
version.string R version 4.2.0 (2022-04-22 ucrt)
nickname       Vigorous Calisthenics
```

# novidades (atualizacao 25/04/2025):
- atualizacao do arquivo Configuracao.xlsx que permite rodar PSATSALT (Salto). (atualizacao parcial)
- GPM_MERGE.txt em Arq_Entrada/OBS atualizado, correspondente a atualizacao de Configuracao.xlsx

# novidades (atualizacao 06/12/2024):
- atualizacao do arquivo Configuracao.xlsx que permite rodar PSATAPI
- GPM_MERGE.txt em Arq_Entrada/OBS atualizado, correspondente a atualizacao de Configuracao.xlsx
- Inclusao de outputs de sessionInfo(), com a versao do R e dos pacotes utilizados para a rodada

```R (windows)
> sessionInfo()
R version 4.2.0 (2022-04-22 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)

Matrix products: default

locale:
[1] LC_COLLATE=Portuguese_Brazil.utf8  LC_CTYPE=Portuguese_Brazil.utf8   
[3] LC_MONETARY=Portuguese_Brazil.utf8 LC_NUMERIC=C
[5] LC_TIME=Portuguese_Brazil.utf8    

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
[1] foreach_1.5.2      plyr_1.8.9         readxl_1.4.3       lubridate_1.9.3
[5] fitdistrplus_1.2-1 survival_3.3-1     MASS_7.3-56        tictoc_1.2.1

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.12      codetools_0.2-18 lattice_0.20-45  fansi_1.0.6
 [5] utf8_1.2.4       grid_4.2.0       cellranger_1.1.0 lifecycle_1.0.4
 [9] jsonlite_1.8.8   magrittr_2.0.3   pillar_1.9.0     rlang_1.1.3
[13] cli_3.6.2        Matrix_1.4-1     vctrs_0.6.5      generics_0.1.3
[17] splines_4.2.0    iterators_1.0.14 glue_1.7.0       compiler_4.2.0
[21] pkgconfig_2.0.3  timechange_0.3.0 tibble_3.2.1
```

# novidades (atualizacao 19/11/2024):
- atualizacao do arquivo Configuracao.xlsx que permite rodar PSATSUIC, PSATJRI, 
PSATBENI, PSATEPB, PSATEGM,  PSATSTOA, PSATENC, PSATAMY1, PSATAMY2, PSATAMY3, PSATAMY4 e PSATJRN
- suporta rodada todos os dias V2 ** (ver abaixo)
- GPM_MERGE.txt em Arq_Entrada/OBS atualizado, correspondente a atualizacao de Configuracao.xlsx


# novidades (atualizacao 24/03/2024):
- 101 membros atualizados v2
- suporta rodada todos os dias V2 ** (ver abaixo)
- suporta rodada em paralelo (via namelist_ec.txt): *** (ver abaixo)
- suporta ano bissexto
- GPM_MERGE.txt em Arq_Entrada/OBS atualizado

# pacotes necessários:
- base (default)
- utils (default)
- tictoc (timing)
- lubridate
- parallel (versões mais novas já está em "base")
- fitdistrplus
- foreach
- stats
- readxl
- plyr


**
- Rodadas de dias impares utilizarão os respectivos reforecasts para a remoção de viés
- IMPORTANTE: PARA A REMOÇÃO DE VIÉS DOS DIAS QUE NÃO SÃO IMPARES (i.e. dias pares) PARA DETERMINADO(A) APROVEITAMENTO, SUB-BACIA, É NECESSÁRIO/OBRIGATÓRIO QUE O MESMO(A)
  APROVEITAMENTO/SUB-BACIA TENHA SIDO RODADO NO DIA DE HINDCAST DISPONIVEL IMEDIATAMENTE ANTERIOR

***
# namelist_ec.txt (default)
model=ECMWF  
n.subbac=130  
ic.date.char=20241203  
conf.file=./Configuracao.xlsx  
input.dir.obs=./Arq_Entrada/OBS  
n.ens.f=101  
n.ens.h=11  
input.dir.f=./Arq_Entrada/ECMWF/Prev  
input.dir.h=./Arq_Entrada/ECMWF/Reforecast  
nyears.hind=20  
ndays.fct=45  
out.dir=./Arq_Saida/ECMWF/Prev_Rem  
nproc_par=  
nproc_gamma=  

IMPORTANTE:
ic.date.char=20241203 # data exemplo

REALIZAR ESTE PROCEDIMENTO ANTES DE DISPARAR O PROCESSO: PARA SABER O NUMERO DE "CORES" DA MÁQUINA A SER UTILIZADA, INSTALAR A BIBLIOTECA parallel CARREGAR A MESMA E DIGITAR
> library(parallel)  
> detectCores()  

SUGESTÃO: DEIXAR PELO MENOS 2 CORES DISPONÍVEIS PARA O FUNCIONAMENTO GERAL DO DESKTOP/NOTEBOOK

nproc_par = número de cores utilizados para a remoção de viés  
npoc_gam  = número de processos utilizados para o bootstrapping estimativa da 
            função gamma dos dados observados  

Os parâmetros nproc_par e nproc_gamma consideram # de cores para rodadas em paralelo. Por default os parâmetros estão vazios em namelist_ec.txt (nproc_par=1 e nproc_gam=1)


As funções auxiliares splitcdf_function.R, add0s_function.R e matchandcorrectfct_function.R estão sendo chamadas por gammarize_funcrion.R