# ENSExtended rv project #
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

REALIZAR ESTE PROCEDIMENTO ANTES DE DISPARAR O PROCESSO: PARA SABER O NUMERO DE "CORES" DA MÁQUINA A SER UTILIZADA, INSTALAR A BIBLIOTECA parallelm CARREGAR A MESMA E DIGITAR
> library(parallel)  
> detectCores()  

SUGESTÃO: DEIXAR PELO MENOS 2 CORES DISPONÍVEIS PARA O FUNCIONAMENTO GERAL DO DESKTOP/NOTEBOOK

nproc_par = número de cores utilizados para a remoção de viés  
npoc_gam  = número de processos utilizados para o bottstrapping estimativa da 
            função gamma dos dados observados  

Os parâmetros nproc_par e nproc_gamma consideram # de cores para rodadas em paralelo. Por default os parâmetros estão vazios em namelist_ec.txt (nproc_par=1 e nproc_gam=1)


# funções auxiliares splitcdf_function.R, add0s_function.R e matchandcorrectfct_function.R estão sendo chamadas por gammarize_funcrion.R