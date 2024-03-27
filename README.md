# ENSExtended rv project #

0. Link da apresentação da metodologia utilizada para a remoção de viés da
   Previsão de Precipitação para o Primeiro Mês da Operação (previsão estedida do ECMWF: 2a Reunião do GT HM 25/01/2022):

   [Vídeo da Apresentação](https://ctpmopld.org.br/documents/33692/411965/2a_Reuni%C3%A3o_GT+HM_Prev1%C2%B0m%C3%AAs_20220125.mp4/0068932d-e201-eccc-f1be-c9beb07eefda?version=1.0&t=1643807101432&download=true)

    Link da apresentação sobre a remoção de viés da
   Previsão de Precipitação para o Primeiro Mês da Operação (previsão estedida do ECMWF: 2a Reunião do GT HM 25/01/2022):

    [Apresentação](https://ctpmopld.org.br/group/ct-pmo-pld/gt-dados-hidrometeorol%C3%B3gicos/-/document_library/OVp6moe0yNrQ/view_file/411955?_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_OVp6moe0yNrQ_redirect=https%3A%2F%2Fctpmopld.org.br%2Fgroup%2Fct-pmo-pld%2Fgt-dados-hidrometeorol%25C3%25B3gicos%2F-%2Fdocument_library%2FOVp6moe0yNrQ%2Fview%2F411941%3F_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_OVp6moe0yNrQ_redirect%3Dhttps%253A%252F%252Fctpmopld.org.br%252Fgroup%252Fct-pmo-pld%252Fgt-dados-hidrometeorol%2525C3%2525B3gicos%253Fp_p_id%253Dcom_liferay_document_library_web_portlet_DLPortlet_INSTANCE_OVp6moe0yNrQ%2526p_p_lifecycle%253D0%2526p_p_state%253Dnormal%2526p_p_mode%253Dview)

# novidades:
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
- Rodadas de 2as e 5as utilizarão os respectivos reforecasts para a remoção de viés
- IMPORTANTE: PARA A REMOÇÃO DE VIÉS DOS DIAS QUE NÃO SÃO 2a OU 5a PARA       
  DETERMINADO APROVEITAMENTO, SUB-BACIA, É NECESSÁRIO/OBRIGATÓRIO QUE O MESMO
  APROVEITAMENTO/SUB-BACIA TENHA SIDO RODADO NA 2a OU 5a IMEDIATAMENTE ANTERIOR
- para os outros dias: 3as e 4as utilizarão os parâmetros estimados na rodada
  da 2a imediatamente anterior; 6as, sáb´s e dom´s utilizarão os parâmetros estimados na rodada da 5a imediatamente anterior
-  


***
# namelist_ec.txt (default)
model=ECMWF  
n.subbac=118  
ic.date.char=20240128  
conf.file=./configuracao.xlsx  
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
ic.date.char=20240128 # data exemplo

REALIZAR ESTE PROCEDIMENTO ANTES DE DISPARAR O PROCESSO: PARA SABER O NUMERO DE "CORES" DA MÁQUINA A SER UTILIZADA, INSTALAR A BIBLIOTECA parallel CARREGAR A MESMA E DIGITAR
> library(parallel)  
> detectCores()  

SUGESTÃO: DEIXAR PELO MENOS 2 CORES DISPONÍVEIS PARA O FUNCIONAMENTO GERAL DO DESKTOP/NOTEBOOK

nproc_par = número de cores utilizados para a remoção de viés  
npoc_gam  = número de processos utilizados para o bootstrapping estimativa da 
            função gamma dos dados observados  

Os parâmetros nproc_par e nproc_gamma consideram # de cores para rodadas em paralelo. Por default os parâmetros estão vazios em namelist_ec.txt (nproc_par=1 e nproc_gam=1)


As funções auxiliares splitcdf_function.R, add0s_function.R e matchandcorrectfct_function.R estão sendo chamadas por gammarize_funcrion.R