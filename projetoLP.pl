% Carlota Ribeiro Domingos 107016
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl'].

% ------------------------------------------------------------------------------
% 3.1 Qualidade dos dados
% ------------------------------------------------------------------------------

% ------------------------------------------------------------------------------
% eventosSemSala(EventosSemSalas)
% EventosSemSala eh uma lista ordenada dos ids de todos os eventos sem sala
% ------------------------------------------------------------------------------
eventosSemSalas(EventosSemSalas):-
    findall(ID,evento(ID,_,_,_,semSala),EventosSemSalas).

% ------------------------------------------------------------------------------
% eventosSemSalaDiaSemana(Dia,SemSalaDia)
% SemSalaDia eh a lista ordenada dos eventos sem sala que ocorrem no dia da 
% semana dia
% ------------------------------------------------------------------------------
eventosSemSalasDiaSemana(Dia,SemSalaDia):-
    eventosSemSalas(SemSala),
    findall(ID,(horario(ID,Dia,_,_,_,_),member(ID,SemSala)),
    SemSalaDia).

% ------------------------------------------------------------------------------
% periodoAux(P,Paux)
% Paux eh o semestre do qual faz parte o periodo P
% ------------------------------------------------------------------------------
periodoAux(P,p1_2):-
    P == p1; P == p2.
periodoAux(P,p3_4):-
    P == p3; P == p4.

% ------------------------------------------------------------------------------
% eventosSemSalaPeriodo(ListaPeriodos,LstEventos)
% LstEventos eh a listaordenada de todos os eventos sem sala que ocorrem nos 
% periodos em ListaPeriodos
% ------------------------------------------------------------------------------
eventosSemSalasPeriodo([],[]):-!.
eventosSemSalasPeriodo([P|LstPer],Lstfinal):-
    periodoAux(P,Paux),
    eventosSemSalas(SemSala),
    findall(ID,((horario(ID,_,_,_,_,P);horario(ID,_,_,_,_,Paux)),
    member(ID,SemSala)),SemSalaPeriodo),
    eventosSemSalasPeriodo(LstPer,EvtPer),
    append(SemSalaPeriodo,EvtPer,FinalAux),
    sort(FinalAux,Lstfinal).

% ------------------------------------------------------------------------------
% 3.2 Pesquisas simples
% ------------------------------------------------------------------------------   

% ------------------------------------------------------------------------------
% organizaIDs(ID,ListaI,LstFinal)
% perdicado auxilar que organiza um elemento ID numa lista de elementos 
% organizada ListaI, LstFinal eh a lista com o ID.
% utilizado no perdicado organizaEventos e organizaDisciplinas
% ------------------------------------------------------------------------------
organizaIDs(ID,[],[ID]):-!.
organizaIDs(ID,[ID2|ListaI],[ID,ID2|ListaI]):-
    ID@=<ID2,!.
organizaIDs(ID,[ID2|ListaI],[ID2|Temp]):-
    ID@>ID2,
    organizaIDs(ID,ListaI,Temp).

% ------------------------------------------------------------------------------
%  organizaEventos(ListaIDs,Per,EventosOrganizados)
%  EventosOrganizados eh a lista ordenada dos IDs em ListaIds que pertencem ao 
%  periodo Per.
% ------------------------------------------------------------------------------
organizaEventos([],_,[]):-!.
organizaEventos([ID|ListaEventos],Per,EventosOrganizados):-
    periodoAux(Per,Paux),
    (horario(ID,_,_,_,_,Per);
    horario(ID,_,_,_,_,Paux)),!,
    organizaEventos(ListaEventos,Per,ProxLst),
    organizaIDs(ID,ProxLst,EventosOrganizados).
organizaEventos([_|ListaEventos],Per,EventosOrganizados):-
    organizaEventos(ListaEventos,Per,EventosOrganizados).

% ------------------------------------------------------------------------------
% eventosMenoresQue(Duracao,LstEventos)
% LstEventos eh a lista que eventos que tem uma duracao menor que Duracao
% ------------------------------------------------------------------------------
eventosMenoresQue(Duracao,LstEventos):-
    findall(ID,(horario(ID,_,_,_,Dur,_),Dur =< Duracao),LstEventos).

% ------------------------------------------------------------------------------
% eventosMenoresQueBool(ID,Duracao)
% verifica se o evento associado a ID tem uma duracao menor que Duracao
% ------------------------------------------------------------------------------
eventosMenoresQueBool(ID,Duracao):-
    horario(ID,_,_,_,Dur,_),
    Dur=<Duracao.

% ------------------------------------------------------------------------------
% procuraDisciplinas(Curso,LstDisciplinasOrg)
% LstDisciplinasOrg eh a lista organizada de todas as disciplinas do Curso
% ------------------------------------------------------------------------------
procuraDisciplinas(Curso,LstDisciplinasOrg):-
    findall(Discp,(turno(ID,Curso,_,_),evento(ID,Discp,_,_,_)),LstDisciplinas),
    sort(LstDisciplinas,LstDisciplinasOrg).

% ------------------------------------------------------------------------------
% organizaDisciplinas(ListaDisciplinas,Curso,LstOrganizada)
% LstOrganizada eh uma lista de listas onde a primeira lista contem as 
% disciplinas na ListaDisiciplinas que ocorrem no primeiro semestre e a segunda 
% os do segundo semestre. falha se o curso nao tiver uma das disciplinas
% ------------------------------------------------------------------------------
organizaDisciplinas([],_,[[],[]]):-!.
organizaDisciplinas([Discp|LstDisciplinas],Curso,[S1,S2]):-
    turno(ID,Curso,_,_),
    evento(ID,Discp,_,_,_),
    (horario(ID,_,_,_,_,p1); horario(ID,_,_,_,_,p2); 
    horario(ID,_,_,_,_,p1_2)),
    organizaDisciplinas(LstDisciplinas,Curso,[NextS1,S2]),
    organizaIDs(Discp,NextS1,S1).
organizaDisciplinas([Discp|LstDscp],Curso,[S1,S2]):-
    turno(ID,Curso,_,_),
    evento(ID,Discp,_,_,_),
    (horario(ID,_,_,_,_,p3);horario(ID,_,_,_,_,p4);
    horario(ID,_,_,_,_,p3_4)),
    organizaDisciplinas(LstDscp,Curso,[S1,NextS2]),
    organizaIDs(Discp,NextS2,S2).

% ------------------------------------------------------------------------------
% idToHora(ID,Hora)
% predicado auxiliar onde Hora eh a duracao do evento associado ao evento de ID
% ------------------------------------------------------------------------------
idToHora(ID,Hora):-
    horario(ID,_,_,_,Hora,_).

% ------------------------------------------------------------------------------
% horasCurso(Periodo, Curso, Ano, TotalHoras)
% TotalHoras eh o numero total de horas dos eventos associados aum Curso num
% determinado Periodo de um Ano
% ------------------------------------------------------------------------------
horasCurso(P, Curso, Ano, HorasTotal):-
    periodoAux(P,Paux),
    findall(ID,(turno(ID,Curso,Ano,_),(horario(ID,_,_,_,_,P);
    horario(ID,_,_,_,_,Paux))),ListaIDs),
    sort(ListaIDs,SetIDs),
    maplist(idToHora,SetIDs,ListaHoras),
    sum_list(ListaHoras,HorasTotal).

% ------------------------------------------------------------------------------
% evolucaoAnual(Ano,Curso,ListaAnual,ListaPeriodo)
% predicado auxiliar em que ListaPeriodo eh a lista de Periodos, EvolucaoAnual
% eh a lista de tuplos do tipo (Ano,Periodo,Horas) e Horas eh o total de horas
% associado a esse Ano e Periodo
% ------------------------------------------------------------------------------
evolucaoAnual(_,_,[],[]):-!.
evolucaoAnual(Ano,Curso,[(Ano,P,Horas)|EvolucaoAnual],[P|R]):-
    horasCurso(P,Curso,Ano,Horas),
    evolucaoAnual(Ano,Curso,EvolucaoAnual,R).

% ------------------------------------------------------------------------------
% evolucaoHorasCurso(Curso,Evolucao)
% Evolucao eh uma lista de tuplos da (Ano,Periodo,Horas), Horas eh o total de 
% horas associadas ao Curso em todos os Anos e periodos Periodo.
% ------------------------------------------------------------------------------
evolucaoHorasCurso(_,[],0):-!.
evolucaoHorasCurso(Curso,Evolucao ,Ano):-
    evolucaoAnual(Ano,Curso,EvolucaoAno,[p1,p2,p3,p4]),
    AnoSeg is Ano - 1,
    evolucaoHorasCurso(Curso,EvolucaoNew, AnoSeg),
    append(EvolucaoNew,EvolucaoAno,Evolucao).
evolucaoHorasCurso(Curso,Evolucao):-
    evolucaoHorasCurso(Curso,Evolucao,3).

% ------------------------------------------------------------------------------
% 3.3 Ocupacoes criticas de salas
% ------------------------------------------------------------------------------

% ------------------------------------------------------------------------------
% ocupaSlot(HoraI,HoraF,HoraIEvento,HoraFEvento,Horas)
% Hora eh o numero de horas sobrepostas entre o evento que tem inicio em 
% HoraIEvento e fim em HoraFEvento e o slot que tem inicio em HoraI e fim 
% em HoraFimDada
% ------------------------------------------------------------------------------
ocupaSlot(HoraI,HoraF,HoraIEvento,HoraFEvento,Horas):-
    HoraF-HoraI>=HoraFEvento-HoraIEvento,
    HoraF>=HoraFEvento,
    HoraIEvento>=HoraI,!,
    Horas is HoraFEvento-HoraIEvento,
    Horas>0.
ocupaSlot(HoraI,HoraF,HoraIEvento,HoraFEvento,Horas):-
    HoraF-HoraI>=HoraFEvento-HoraIEvento,
    HoraIEvento<HoraI,
    HoraFEvento>HoraI, 
    Horas is HoraFEvento-HoraI,
    Horas>0.
ocupaSlot(HoraI,HoraF,HoraIEvento,HoraFEvento,Horas):-
    HoraF-HoraI>=HoraFEvento-HoraIEvento,
    HoraFEvento>HoraF, 
    HoraIEvento<HoraF, 
    Horas is HoraF-HoraIEvento,
    Horas>0.
ocupaSlot(HoraI,HoraF,HoraIEvento,HoraFEvento,Horas):-
    HoraF-HoraI<HoraFEvento-HoraIEvento,
    ocupaSlot(HoraIEvento,HoraFEvento,HoraI,HoraF,Horas).

% ------------------------------------------------------------------------------
% listasInicioFim(P,TipoSala,Dia,ListaI,ListaF)
% perdicado auxiliar ListaI e ListaF sao listas com respetivamente as horas de 
% inicio e fim dos eventos que ocorrem nas salas do tipo dos TipoSala no periodo 
% P no dia da semana Dia
% ------------------------------------------------------------------------------
listasInicioFim(P,TipoSala,Dia,ListaI,ListaF):-
    periodoAux(P,Paux),
    salas(TipoSala,L1),
    findall(HI,(member(Sala,L1),evento(ID,_,_,_,Sala),
    (horario(ID,Dia,HI,_,_,P);horario(ID,Dia,HI,_,_,Paux))),ListaI),
    findall(HF,(member(Sala,L1),evento(ID,_,_,_,Sala),
    (horario(ID,Dia,_,HF,_,P);horario(ID,Dia,_,HF,_,Paux))),ListaF).

% ------------------------------------------------------------------------------
% ocupaSala(ListaI,ListaF,HoraI,HoraF,LHoras)
% perdicado auxiliar que usa o perdicado ocupa slot para calcular as horas 
% sobrepostas entre os eventos que comecam e acabam nos elementos de ListaI 
% e ListaF respetivamente e o evento que ocorre entre HoraI e HoraF, LHoras 
% eh a lista com as horas sobrepostas
% ------------------------------------------------------------------------------
ocupaSala([],[],_,_,[]):-!.
ocupaSala([I|ListaI],[F|ListaF],HoraI,HoraF,[H|LHoras]):-
    ocupaSlot(I,F,HoraI,HoraF,H),!,
    ocupaSala(ListaI,ListaF,HoraI,HoraF,LHoras).
ocupaSala([_|ListaI],[_|ListaF],HoraI,HoraF,LHoras):-
    ocupaSala(ListaI,ListaF,HoraI,HoraF,LHoras).

% ------------------------------------------------------------------------------
% pnumHorasOcupadas(P,TipoSala,Dia,HoraI,HoraF,Horas)
% Horas eh o numero de horas ocupadas nas salas do tipo TipoSala, no intervalo 
% de tempo entre HoraI e HoraF, no dia da semana Dia, e no periodo Periodo
% ------------------------------------------------------------------------------
numHorasOcupadas(P, TipoSala, Dia, HoraI, HoraF, Horas):-
    listasInicioFim(P, TipoSala, Dia, ListaI, ListaF),
    ocupaSala(ListaI,ListaF,HoraI,HoraF,LHoras),
    sum_list(LHoras, Horas).

% ------------------------------------------------------------------------------
% ocupacaoMax(TipoSala,HoraI,HoraF,Max)
% Max eh o numero de horas possiveis de ser ocupadas nas salas do tipo TipoSala 
% no intervalo de tempo entre HoraInicio e HoraFim
% ------------------------------------------------------------------------------
ocupacaoMax(TipoSala, HoraI, HoraF, Max):-
    salas(TipoSala, L1),
    length(L1,Nsalas),
    Duracao is HoraF - HoraI,
    Max is Duracao*Nsalas.

% ------------------------------------------------------------------------------
% percentagem(SomaHoras, Max, Percentagem) 
% Percentagem eh a divisao de SomaHoras por Max multiplicada por 100
% ------------------------------------------------------------------------------
percentagem(SomaHoras, Max, Percentagem ):-
    Percentagem is (SomaHoras/Max)*100.

% ------------------------------------------------------------------------------
% verficaOcupacao(HoraI,HoraF,casosCriticos(Dia,TipoSala,Percentagem))
% eh verdade se casosCriticos(DiaSemana,TipoSala,Percentagem) eh um tuplo em 
% que DiaSemana, TipoSala e Percentagem sao, respectivamente, um dia da semana, 
% um tipo de sala e a sua percentagem de ocupacao entre HoraInicio e HoraFim num 
% indetermidado periodo
% ------------------------------------------------------------------------------
verficaOcupacao(HoraI,HoraF,casosCriticos(Dia,TipoSala,Percentagem)):-
    member(Per,[p1,p2,p3,p4]),
    horario(_,Dia,_,_,_,_), 
    salas(TipoSala,_),
    numHorasOcupadas(Per,TipoSala,Dia,HoraI,HoraF,SomaHoras),
    ocupacaoMax(TipoSala,HoraI,HoraF,Max),
    percentagem(SomaHoras,Max,Percent),
    Percentagem is ceiling(Percent).

% ------------------------------------------------------------------------------
% ocupacaoCritica(HoraInicio,HoraFim,Threshold,Resultados)
% Resultados eh uma lista ordenada de tuplos do tipo 
% casosCriticos(DiaSemana,TipoSala,Percentagem) em que DiaSemana, TipoSala e 
% Percentagem sao, respectivamente, um dia da semana, um tipo de sala e a sua 
% percentagem de ocupacao entre HoraInicio e HoraFim
% Percentagem eh maior que Threshold.
% ------------------------------------------------------------------------------
ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados):-
    findall(casosCriticos(Dia,TipoSala,Percentagem), 
    (verficaOcupacao(HoraInicio,HoraFim,
    casosCriticos(Dia,TipoSala,Percentagem)),
    Percentagem>Threshold),Res),
    sort(Res,Resultados).

% ------------------------------------------------------------------------------
% 3.4 And now for something completely different...
% ------------------------------------------------------------------------------

% ------------------------------------------------------------------------------
% definicao das restricoes
% define as restricoes com a mesa associada no ultimo argumento
% ------------------------------------------------------------------------------

% ------------------------------------------------------------------------------
% cab1(Pessoa,Mesa)
% pessoa esta sentada na cabeceira do lado esquedo
% cab2(Pessoa,Mesa)
% pessoa esta sentada na cabeceira do lado direito
% honra(Pessoa1,Pessoa2,Mesa)
% Pessoa1 esta numa das cabelceiras e Pessoa 2 esta ao lado direito de Pessoa1
% ------------------------------------------------------------------------------
cab1(Pessoa,[[_,_,_],[Pessoa,_],[_,_,_]]).

cab2(Pessoa,[[_,_,_],[_,Pessoa],[_,_,_]]).

honra(Pessoa1,Pessoa2,[[_,_,Pessoa2],[_,Pessoa1],[_,_,_]]).
honra(Pessoa1,Pessoa2,[[_,_,_],[Pessoa1,_],[Pessoa2,_,_]]).

% ------------------------------------------------------------------------------
% lado(Pessoa1,Pessoa2,Mesa)
% Pessoa1 esta sentada ao lado de Pessoa2 (nao inclui as pessoas na cabeceira)
% naoLado(Pessoa1,Pessoa2,Mesa)
% Pessoa1 nao esta sentada ao lado de Pessoa2 
% ------------------------------------------------------------------------------
lado(Pessoa1,Pessoa2,[[Pessoa1,Pessoa2,_],_,_]).
lado(Pessoa1,Pessoa2,[[Pessoa2,Pessoa1,_],_,_]).
lado(Pessoa1,Pessoa2,[[_,Pessoa1,Pessoa2],_,_]).
lado(Pessoa1,Pessoa2,[[_,Pessoa2,Pessoa1],_,_]).
lado(Pessoa1,Pessoa2,[_,_,[Pessoa1,Pessoa2,_]]).
lado(Pessoa1,Pessoa2,[_,_,[Pessoa2,Pessoa1,_]]).
lado(Pessoa1,Pessoa2,[_,_,[_,Pessoa2,Pessoa1]]).
lado(Pessoa1,Pessoa2,[_,_,[_,Pessoa1,Pessoa2]]).

naoLado(Pessoa1,Pessoa2,Mesa):-
    Mesa = [[_,_,_],[_,_],[_,_,_]],
    \+ lado(Pessoa1,Pessoa2,Mesa).

% ------------------------------------------------------------------------------
% frente(Pessoa1,Pessoa2,Mesa)
% Pessoa1 esta sentada a frente de Pessoa2 (nao inclui as pessoas na cabeceira)
% naoFrente(Pessoa1,Pessoa2,Mesa)
% Pessoa1 nao esta sentada a frente de Pessoa2 
% ------------------------------------------------------------------------------
frente(Pessoa1,Pessoa2,[[Pessoa1,_,_],[_,_],[Pessoa2,_,_]]).
frente(Pessoa1,Pessoa2,[[_,Pessoa1,_],[_,_],[_,Pessoa2,_]]).
frente(Pessoa1,Pessoa2,[[_,_,Pessoa1],[_,_],[_,_,Pessoa2]]).
frente(Pessoa1,Pessoa2,[[Pessoa2,_,_],[_,_],[Pessoa1,_,_]]).
frente(Pessoa1,Pessoa2,[[_,Pessoa2,_],[_,_],[_,Pessoa1,_]]).
frente(Pessoa1,Pessoa2,[[_,_,Pessoa2],[_,_],[_,_,Pessoa1]]).

naoFrente(Pessoa1,Pessoa2,Mesa):-
    Mesa = [[_,_,_],[_,_],[_,_,_]],
    \+ frente(Pessoa1,Pessoa2,Mesa).
    
% ------------------------------------------------------------------------------
% predicados auxiliares e final
% ------------------------------------------------------------------------------

% ------------------------------------------------------------------------------
% exists(Pessoa,OcupacaoMesa)
% eh verdade se Pessoa eh membro de uma das listas de OcupacaoMesa
% verifica se a pessoa Pessoa se encontra sentada a mesa
% ------------------------------------------------------------------------------
exists(Pessoa,[L1|Mesa]):-
    member(Pessoa,L1);
    exists(Pessoa,Mesa).

% ------------------------------------------------------------------------------
% adicionaPresenca(OcupacaoMesa,Pessoa,Pred)
% eh verdade se Pred for o literal exists(Pessoa,OcupacaoMesa)
% ------------------------------------------------------------------------------
adicionaPresenca(OcupacaoMesa,Pessoa,Pred):-
    functor(Pred,exists,2),
    arg(2,Pred,OcupacaoMesa),
    arg(1,Pred,Pessoa).

% ------------------------------------------------------------------------------
% adicionaMesa(OcupacaoMesa,Rest,RestMesa)
% eh verdade se RestMesa eh o literal com o mesmo predicado e argumentos que 
% Rest mas adiciona OcupacaoMesa como o ultimo argumento do literal
% ------------------------------------------------------------------------------
adicionaMesa(OcupacaoMesa,Rest,RestMesa):-
    Rest=..[Lit|Args],
    append(Args,[OcupacaoMesa],ListaF),
    RestMesa =.. [Lit|ListaF].

% ------------------------------------------------------------------------------
% ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa)
% ListaPessoas eh a lista das pessoas a sentar a mesa, ListaRestricoes eh lista 
% de restricoes a verificar e OcupacaoMesa eh uma lista com tres listas, a 
% primeira contem as pessoas de um lado da mesa, a segunda as pessoas a cabeceira
% e a terceira as pessoas do outro lado da mesa
% ------------------------------------------------------------------------------
ocupacaoMesa(ListaPessoas, Rest, OcupacaoMesa):-
    OcupacaoMesa = [[_,_,_],[_,_],[_,_,_]],
    maplist(adicionaPresenca(OcupacaoMesa),ListaPessoas,Presenca),
    maplist(adicionaMesa(OcupacaoMesa),Rest,RestMesa),
    append(Presenca,RestMesa,New),
    maplist(call,New).


