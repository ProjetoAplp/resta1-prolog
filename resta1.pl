:- use_module(library(clpfd)).


/*Predicados de inicialacao do jogo */

pino('1').
livre('0').
tamanho(7).

instructions :-
    write("RRRRRRRRRRRRRRRRR                                                 tttt                                   1111111"),nl,
    write("R::::::::::::::::R                                             ttt:::t                                  1::::::1"),nl,  
    write("R::::::RRRRRR:::::R                                            t:::::t                                 1:::::::1"),nl,
    write("RR:::::R     R:::::R                                           t:::::t                                 111:::::1"),nl,
    write("  R::::R     R:::::R    eeeeeeeeeeee        ssssssssss   ttttttt:::::ttttttt      aaaaaaaaaaaaa           1::::1"),nl,
    write("  R::::R     R:::::R  ee::::::::::::ee    ss::::::::::s  t:::::::::::::::::t      a::::::::::::a          1::::1"),nl,
    write("  R::::RRRRRR:::::R  e::::::eeeee:::::eess:::::::::::::s t:::::::::::::::::t      aaaaaaaaa:::::a         1::::1"),nl,   
    write("  R:::::::::::::RR  e::::::e     e:::::es::::::ssss:::::stttttt:::::::tttttt               a::::a         1::::l"),nl,   
    write("  R::::RRRRRR:::::R e:::::::eeeee::::::e s:::::s  ssssss       t:::::t              aaaaaaa:::::a         1::::l"),nl,   
    write("  R::::R     R:::::Re:::::::::::::::::e    s::::::s            t:::::t            aa::::::::::::a         1::::l"),nl,   
    write("  R::::R     R:::::Re::::::eeeeeeeeeee        s::::::s         t:::::t           a::::aaaa::::::a         1::::l"),nl,   
    write("  R::::R     R:::::Re:::::::e           ssssss   s:::::s       t:::::t    tttttta::::a    a:::::a         1::::l"),nl,   
    write("RR:::::R     R:::::Re::::::::e          s:::::ssss::::::s      t::::::tttt:::::ta::::a    a:::::a      111::::::111"),nl,
    write("R::::::R     R:::::R e::::::::eeeeeeee  s::::::::::::::s       tt::::::::::::::ta:::::aaaa::::::a      1::::::::::1"),nl,
    write("R::::::R     R:::::R  ee:::::::::::::e   s:::::::::::ss          tt:::::::::::tt a::::::::::aa:::a     1::::::::::1"),nl,
    write("RRRRRRRR     RRRRRRR    eeeeeeeeeeeeee    sssssssssss              ttttttttttt    aaaaaaaaaa  aaaa     111111111111"),nl,nl,

    write("REGRAS:"),nl, 
    write("O objetivo é deixar apenas uma peça no tabuleiro após uma sequência de movimentos válidos."),nl,    
    write("O tabuleiro possui um espaço vazio no centro, representado pelo caractere '0', com um número de peças, representadas pelo caractere '1', que designam uma estrutura pré-definida."),nl,
    write("O jogo apresenta duas formas de tabuleiro, a primeira com padrão inglês com 32 peças e a outra o padrão europeu com 36 peças."),nl,
    write("Um movimento consiste em pegar uma peça e fazê-la 'saltar' sobre outra peça, sempre na horizontal ou na vertical, terminando "),nl,
    write("em um espaço vazio, representado pelo caractere '0', adjacente a peça 'saltada'. A peça que foi 'saltada' é retirada do tabuleiro."),nl,
    nl.


/*Escolhe o tipo do tabuleiro*/

escolheTabuleiro(Tabuleiro):-
    write("Digite 1 para o tabuleiro Ingles, 2 para o tabuleiro Europeu"), read(Escolha),
    criar_tabuleiro(Escolha, Tabuleiro).

/*Tabuleiro Ingles*/
criar_tabuleiro(1,[
    [' ',' ','1','1','1',' ',' '],
    [' ',' ','1','1','1',' ',' '],
    ['1','1','1','1','1','1','1'],
    ['1','1','1','0','1','1','1'],
    ['1','1','1','1','1','1','1'],
    [' ',' ','1','1','1',' ',' '],
    [' ',' ','1','1','1',' ',' ']
]).

/*Tabuleiro Europeu*/
criar_tabuleiro(2,[
    [' ',' ','1','1','1',' ',' '],
    [' ','1','1','1','1','1',' '],
    ['1','1','1','1','1','1','1'],
    ['1','1','1','0','1','1','1'],
    ['1','1','1','1','1','1','1'],
    [' ','1','1','1','1','1',' '],
    [' ',' ','1','1','1',' ',' ']
]).


/* Esses fatos decidem como o tabuleiro eh impresso */

imprimeTabuleiro(Matrix) :-
    write('    A  B  C  D  E  F  G'), nl, nl,
    printLines(Matrix, 0).

printLines([], _).
printLines([H|T], Index) :- 
    indexa_linha(Index, Index_linha), write(Index_linha), write('  '), printLine(H), nl,
    NewIndex is Index+1,
    printLines(T, NewIndex).

printLine([]).
printLine([H|T]) :- write(H), write('  '), printLine(T).

indexa_linha(0, "1").
indexa_linha(1, "2").
indexa_linha(2, "3").
indexa_linha(3, "4").
indexa_linha(4, "5").
indexa_linha(5, "6").
indexa_linha(6, "7").

indexa_coluna(1, "A").
indexa_coluna(2, "B").
indexa_coluna(3, "C").
indexa_coluna(4, "D").
indexa_coluna(5, "E").
indexa_coluna(6, "F").
indexa_coluna(7, "G").


/*Recebe jogada*/

lerLinha(Linha) :- write("Selecione a linha(1-7): "), read(Linha).

lerColuna(Coluna) :- write("Selecione a coluna(A-G): "), read(Coluna).

lerDirecao(Direcao) :- write("Selecione a direção(0 - Cima; 1 - Baixo; 2 - Esquerda; 3 - Direita): "), read(Direcao).



/*Valida jogada recebida*/

verificaOrigem(Linha, Coluna, Matrix) :- indexa_coluna(C, Coluna), existePino(Matrix, [Linha,C]).

verificaSalto(Linha, Coluna, 0, Matrix) :- indexa_coluna(C, Coluna), L is Linha - 1, existePino(Matrix, [L,C]).
verificaSalto(Linha, Coluna, 1, Matrix) :- indexa_coluna(C, Coluna), L is Linha + 1, existePino(Matrix, [L,C]).
verificaSalto(Linha, Coluna, 2, Matrix) :- indexa_coluna(C, Coluna), C1 is C - 1, existePino(Matrix, [Linha,C1]).
verificaSalto(Linha, Coluna, 3, Matrix) :- indexa_coluna(C, Coluna), C1 is C + 1, existePino(Matrix, [Linha,C1]).

verificaDestino(Linha, Coluna, 0, Matrix) :- indexa_coluna(C, Coluna), L is Linha - 2, estaLivre(Matrix, [L,C]).
verificaDestino(Linha, Coluna, 1, Matrix) :- indexa_coluna(C, Coluna), L is Linha + 2,estaLivre(Matrix, [L,C]).
verificaDestino(Linha, Coluna, 2, Matrix) :- indexa_coluna(C, Coluna), C1 is C - 2, estaLivre(Matrix, [Linha,C1]).
verificaDestino(Linha, Coluna, 3, Matrix) :- indexa_coluna(C, Coluna), C1 is C + 2, estaLivre(Matrix, [Linha,C1]).

validaJogada(Linha, Coluna, Direcao, Matrix) :- verificaOrigem(Linha, Coluna, Matrix), verificaSalto(Linha, Coluna, Direcao, Matrix), verificaDestino(Linha, Coluna, Direcao, Matrix). 

existe(Tabuleiro, [X, Y], V) :- nth1(X, Tabuleiro, Val), nth1(Y, Val, V).

existePino(Tabuleiro, [X, Y]) :- pino(V), existe(Tabuleiro, [X, Y], V).

estaLivre(Tabuleiro, [X, Y]) :- livre(V), existe(Tabuleiro, [X, Y], V).

validaPosicao(Tabuleiro, [X,Y]) :- estaDentro(X), estaDentro(Y), estaLivre(Tabuleiro, [X,Y]).
validaPosicao(Tabuleiro, [X,Y]) :- estaDentro(X), estaDentro(Y), existePino(Tabuleiro, [X,Y]).

estaDentro(X) :- tamanho(T), between(1, T, X).

/*Verifica Fim do jogo*/

rotacionaMatrix(Xss, Zss) :-
   transpose(Xss, Yss),
   maplist(reverse, Yss, Zss).

sublist([], L).
sublist([X1,X2,X3], [X1,X2,X3|Ys]).
sublist(Xs, [_|Ys]) :- sublist(Xs, Ys).

temJogadaValidaLinha(Linha) :- 
    once(
        sublist(['1','1','0'], Linha);
        sublist(['0','1','1'], Linha)
    ).

temJogadaValidaHorizontalMatrix(Matrix) :-
    once(
        (nth0(0, Matrix, Linha1), temJogadaValidaLinha(Linha1));
        (nth0(1, Matrix, Linha2), temJogadaValidaLinha(Linha2));
        (nth0(2, Matrix, Linha3), temJogadaValidaLinha(Linha3));
        (nth0(3, Matrix, Linha4), temJogadaValidaLinha(Linha4));
        (nth0(4, Matrix, Linha5), temJogadaValidaLinha(Linha5));
        (nth0(5, Matrix, Linha6), temJogadaValidaLinha(Linha6));
        (nth0(6, Matrix, Linha7), temJogadaValidaLinha(Linha7))
    ).

temJogadaValidaMatrix(Tabuleiro) :-
    rotacionaMatrix(Tabuleiro, TabuleiroRotacionado),
    once(
        temJogadaValidaHorizontalMatrix(Tabuleiro);
        temJogadaValidaHorizontalMatrix(TabuleiroRotacionado)
    ).

count(_, [], 0).
count(X, [X | T], N) :-
  !, count(X, T, N1),
  N is N1 + 1.
count(X, [_ | T], N) :-
  count(X, T, N).


verificaVitoria(Tabuleiro) :-
    flatten(Tabuleiro, Flat), count('1',Flat,N), N = 1.

verificarFimDeJogo(Tabuleiro) :-
     \+ temJogadaValidaMatrix(Tabuleiro),
     (
        verificaVitoria(Tabuleiro) -> write('Parabéns, você venceu!');
        write('Você perdeu, tente novamente.')
     ).



/*Loop Principal */

ganharAutomaticamente([], Matrix) :- 
    write("Parabéns! Você VENCEU!"),
    imprimeTabuleiro(Matrix).
ganharAutomaticamente([H|T], Matrix) :- 
    imprimeTabuleiro(Matrix),
    nth0(0, H, Linha),
    nth0(1, H, Coluna),
    nth0(2, H, Direcao),
    indexa_coluna(Coluna, Coluna1),
    /*realizaJogada(Matrix, Linha, Coluna1, Direcao, MatrixAtualizada),*/
    write(T),
    ganharAutomaticamente(T, MatrixAtualizada).

sequencia([[2,4,1], [5,4,0], [4,6,2], [4,3,3], [2,3,1], [5,6,2], [3,1,3], [7,5,0], [3,4,2], [4,5,1], [5,1,0], [7,3,3], [3,1,3], [7,5,0], [4,3,0], [5,4,3], [3,6,2], [1,5,1], [5,7,2], [5,2,3], [5,4,3], [3,7,1], [5,7,2], [6,3,3], [6,5,0], [4,5,0], [1,3,3], [1,5,1], [3,5,2], [2,3,1], [4,2,3]]).


jogar(s) :- sequencia(Jogadas), criar_tabuleiro(1, Tabuleiro), ganharAutomaticamente(Jogadas, Tabuleiro).
jogar(n) :- escolheTabuleiro(Tabuleiro), gameloop(Tabuleiro).

/*Realiza jogada*/
replace( Matriz , Linha , Coluna , NovoValor , NovaMatriz ) :-

    append(RowPfx,[Row|RowSfx],Matriz),        
    length(RowPfx,Linha) ,                         
    append(ColPfx,[_|ColSfx],Row) ,            
    length(ColPfx,Coluna) ,                         
    append(ColPfx,[NovoValor|ColSfx],RowNew) , 
    append(RowPfx,[RowNew|RowSfx],NovaMatriz)
  .

/*movimenta para Cima*/
executarJogada(Linha, Coluna, 0, Matrix, MatrixAtualizada):-
    indexa_coluna(C,Coluna), 
    Col is  C-1,
    Lin is Linha -1,
    Lin2 is Lin -1 ,
    Lin3 is Lin -2,


    replace(Matrix, Lin  ,Col ,'0', MatrixAtualizada1), % limpa espaco inicial
    replace(MatrixAtualizada1, Lin2 ,Col ,'0', MatrixAtualizada2), % limpa espaco adjacente
    replace(MatrixAtualizada2, Lin3 ,Col ,'1', MatrixAtualizada) % adciona peca ao destino final
    .

/*movimenta para Baixo*/
executarJogada(Linha, Coluna, 1, Matrix, MatrixAtualizada):-
    indexa_coluna(C,Coluna), 
    Col is  C-1,
    Lin is Linha -1,
    Lin2 is Lin +1 ,
    Lin3 is Lin +2,


    replace(Matrix, Lin  ,Col ,'0', MatrixAtualizada1), % limpa espaco inicial
    replace(MatrixAtualizada1, Lin2 ,Col ,'0', MatrixAtualizada2), % limpa espaco adjacente
    replace(MatrixAtualizada2, Lin3 ,Col ,'1', MatrixAtualizada) % adciona peca ao destino final
.

/*movimenta para Esquerda*/
executarJogada(Linha, Coluna, 2, Matrix, MatrixAtualizada):-
    indexa_coluna(C,Coluna), 
    Col is  C-1,
    Lin is Linha -1,
    Col2 is Col -1 ,
    Col3 is Col -2,


    replace(Matrix, Lin  ,Col ,'0', MatrixAtualizada1), % limpa espaco inicial
    replace(MatrixAtualizada1, Lin ,Col2 ,'0', MatrixAtualizada2), % limpa espaco adjacente
    replace(MatrixAtualizada2, Lin ,Col3 ,'1', MatrixAtualizada) % adciona peca ao destino final
.

/*movimenta para Direita*/
executarJogada(Linha, Coluna, 3, Matrix, MatrixAtualizada):-
    indexa_coluna(C,Coluna), 
    Col is  C-1,
    Lin is Linha -1,

    Col2 is Col +1 ,
    Col3 is Col +2,


    replace(Matrix, Lin  ,Col ,'0', MatrixAtualizada1), % limpa espaco inicial
    replace(MatrixAtualizada1, Lin ,Col2 ,'0', MatrixAtualizada2), % limpa espaco adjacente
    replace(MatrixAtualizada2, Lin ,Col3 ,'1', MatrixAtualizada) % adciona peca ao destino final
.



gameloop(Matrix) :- imprimeTabuleiro(Matrix), 
                    lerLinha(Linha), 
                    lerColuna(Coluna), 
                    lerDirecao(Direcao),
                    (
                        validaJogada(Linha, Coluna, Direcao, Matrix) -> executarJogada(Linha, Coluna, Direcao, Matrix, MatrixAtualizada), /*verificarFimDeJogo(MatrixAtualizada),*/ gameloop(MatrixAtualizada);
                        write("Jogada inválida"), nl, gameloop(Matrix)
                    ).



/*inicia o Jogo*/
:- initialization main.
main :-
    instructions,
    write("Deseja jogar automaticamente? (s/n) "), read(Op),
    atom_string(R, Op),
    jogar(R),
    halt(0).
