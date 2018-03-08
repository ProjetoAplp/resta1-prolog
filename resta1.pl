:- initialization main.

existe(tabuleiro, [X, Y], pino) :-
    nth1(X, tabuleiro, valores),
    nth1(Y, valores, pino).

existePino(tabuleiro, [X, Y]) :-
    pino(p),
    existe(tabuleiro, [X, Y], p).

estaLivre(tabuleiro, [X, Y]) :-
    livre(p),
    existe(tabuleiro, [X, Y], p).

validaPosicao(tabuleiro, [X,Y]) :- 
    estaDentro(X), estaDentro(Y), estaLivre(tabuleiro, [X,Y]).
validaPosicao(tabuleiro, [X,Y]) :- 
    estaDentro(X), estaDentro(Y), existePino(tabuleiro, [X,Y]).

estaDentro(X) :- tamanho(d), between(1, d, X).




pino(1).
livre(0).
tamanho(7).

criar_tabuleiro([
    [' ',' ','1','1','1',' ',' '],
    [' ',' ','1','1','1',' ',' '],
    ['1','1','1','1','1','1','1'],
    ['1','1','1','0','1','1','1'],
    ['1','1','1','1','1','1','1'],
    [' ',' ','1','1','1',' ',' '],
    [' ',' ','1','1','1',' ',' ']
]).

imprimeTabuleiro(Matrix) :-
    write('    A  B  C  D  E  F  G'), nl, nl,
    printLines(Matrix, 0).

printLines([], _).
printLines([H|T], Index) :- indexa_linha(Index, Index_linha), write(Index_linha), write('  '), printLine(H), nl,
    						NewIndex is Index+1,
    						printLines(T, NewIndex).

printLine([]).
printLine([H|T]) :- write(H), write('  '), printLine(T).

indexa_linha(0, "1 ").
indexa_linha(1, "2 ").
indexa_linha(2, "3 ").
indexa_linha(3, "4 ").
indexa_linha(4, "5 ").
indexa_linha(5, "6 ").
indexa_linha(6, "7 ").

indexa_coluna(0, "A ").
indexa_coluna(1, "B ").
indexa_coluna(2, "C ").
indexa_coluna(3, "D ").
indexa_coluna(4, "E ").
indexa_coluna(5, "F ").
indexa_coluna(6, "G ").

gameloop(Matrix) :- imprimeTabuleiro(Matrix), 
                    lerLinha(Linha), 
                    lerColuna(Coluna), 
                    lerDirecao(Direcao), 
                    (
                        validarJogada(Linha, Coluna, Direcao, Matrix) -> write("executarJogada(Matrix, MatrixAtualizada), verificarFimDeJogo(MatrixAtualizada), gameloop(MatrixAtualizada)"), nl, gameloop(Matrix);
                        write("Jogada inválida"), nl, gameloop(Matrix)
                    ).

lerLinha(Linha) :- write("Selecione a linha(1-7): "), read(Linha).

lerColuna(Coluna) :- write("Selecione a culuna(A-G): "), read(Coluna).

lerDirecao(Direcao) :- write("Selecione a direção(0 - Cima; 1 - Baixo; 2 - Esquerda; 3 - Direita): "), read(Direcao).

verificaOrigem(Linha, Coluna, Matrix) :- indexa_linha(L, Linha), indexa_coluna(C, Coluna), existePino(Matrix, [L,C]).

verificaSalto(Linha, Coluna, "0 ", Matrix) :- indexa_linha(L, Linha), indexa_coluna(C, Coluna), existePino(Matrix, [L,(C - 1)]).

verificaSalto(Linha, Coluna, "1 ", Matrix) :- indexa_linha(L, Linha), indexa_coluna(C, Coluna), existePino(Matrix, [L,(C + 1)]).

verificaSalto(Linha, Coluna, "2 ", Matrix) :- indexa_linha(L, Linha), indexa_coluna(C, Coluna), existePino(Matrix, [(L - 1),C]).

verificaSalto(Linha, Coluna, "3 ", Matrix) :- indexa_linha(L, Linha), indexa_coluna(C, Coluna), existePino(Matrix, [(L + 1),C]).

verificaDestino(Linha, Coluna, "0 ", Matrix) :- indexa_linha(L, Linha), indexa_coluna(C, Coluna), estaLivre(Matrix, [L,(C - 2)]).

verificaDestino(Linha, Coluna, "1 ", Matrix) :- indexa_linha(L, Linha), indexa_coluna(C, Coluna), estaLivre(Matrix, [L,(C + 1)]).

verificaDestino(Linha, Coluna, "2 ", Matrix) :- indexa_linha(L, Linha), indexa_coluna(C, Coluna), estaLivre(Matrix, [(L - 1),C]).

verificaDestino(Linha, Coluna, "3 ", Matrix) :- indexa_linha(L, Linha), indexa_coluna(C, Coluna), estaLivre(Matrix, [(L + 1),C]).

validarJogada(Linha, Coluna, Direcao, Matrix) :- verificaOrigem(Linha, Coluna, Matrix), verificaSalto(Linha, Coluna, Direcao, Matrix), verificaDestino(Linha, Coluna, Direcao, Matrix). 

main:-
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
    write("Um movimento consiste em pegar uma peça e fazê-la 'saltar' sobre outra peça, sempre na horizontal ou na vertical, terminando em um espaço vazio, representado pelo caractere '0', adjacente a peça 'saltada'. A peça que foi 'saltada' é retirada do tabuleiro."),nl,

    criar_tabuleiro(Tabuleiro),


    gameloop(Tabuleiro),

    halt(0).
