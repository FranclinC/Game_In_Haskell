{-
    Jogo da velha
-}

import Control.Exception
import System.IO
import System.IO.Error
import System.Process
import Data.List

-- definção dos tipos de dados

type Jogadores = [Jogador]
type Nome = String
type Pontuacao = Int
type Vez = Int
type Tabela = [Char]
data Jogador = Jogador Nome Pontuacao
    deriving ( Show, Read )

inicio :: IO ()
inicio = do
    {catch (ler_arquivo) tratar_erro;}
    where
        ler_arquivo = do
        {
            arq <- openFile "dados.txt" ReadMode; -- abre arquivo para leitura
            dados <- hGetLine arq;
            hClose arq;
            menu (read dados);
            return ()
        }
        tratar_erro error = if isDoesNotExistError error then do
        {
            arq <- openFile "dados.txt" WriteMode;
            hPutStrLn arq "[]";
            hClose arq;
            return ()
        }
        else do
            ioError error


menu :: Jogadores -> IO Jogadores
menu dados = do
    system "clear"
    putStrLn "----------------------------------------- Jogo da Velha -----------------------------------------"
    putStrLn "\nDigite 1 para cadastrar jogador"
    putStrLn "Digite 2 para jogar"
    putStrLn "Digite 3 para visualizar o ranking"
    putStrLn "Digite 0 para sair"
    putStr "Opção: "
    op <- getChar
    getChar -- descartar o enter
    executarOpcao dados op

executarOpcao :: Jogadores -> Char -> IO Jogadores
executarOpcao dados '0' = do
    putStrLn "Bye bye"
    return dados
executarOpcao dados '1' = cadastrarJogador dados
executarOpcao dados '2' = prepararJogo dados
executarOpcao dados _ = do
    putStrLn "\nOpção invalidada! Tente novamente..."
    putStrLn "Pressione <Enter> para retornar ao menu"
    getChar
    menu dados

getString :: String -> IO String
getString str = do
    putStr str
    res <- getLine
    return res

cadastrarJogador :: Jogadores -> IO Jogadores
cadastrarJogador dados = do
    nome <- getString "Digite o nome do jogador: "
    if (existeJogador dados nome) then do
        putStrLn "\nEsse jogador ja existe, escolha outra..."
        putStr "Pressione <Enter> para continuar..."
        getChar
        return dados
    else do
        arq <- openFile "dados.txt" WriteMode
        hPutStrLn arq (show ((Jogador nome 0):dados))
        hClose arq
        putStrLn ("\nUsuáro " ++ nome ++ " cadastrado com sucesso.")
        putStr "\nPressione <Enter> para continuar..."
        getChar
        menu ((Jogador nome 0):dados)

existeJogador :: Jogadores -> Nome -> Bool
existeJogador [] _ = False
existeJogador ((Jogador n p):xs) nome
        | (n == nome) = True
        | otherwise = existeJogador xs nome


prepararJogo :: Jogadores -> IO Jogadores
prepararJogo dados = do
    jogador1 <- getString "\nDigite o nome do primeiro jogador: "
    if not (existeJogador dados jogador1) then do
        putStrLn "\nEsse jogador não existe!"
        putStr "Pressione <Enter> para continuar..."
        getChar
        menu dados
    else do
        jogador2 <- getString "\nDigite o nome do segundo jogador: "
        if not (existeJogador dados jogador2) then do
            putStrLn "\nEsse jogador não existe!"
            putStr "Pressione <Enter> para continuar..."
            getChar
            menu dados
        else do
            novoJogo dados jogador1 jogador2


novoJogo :: Jogadores -> Nome -> Nome -> IO Jogadores
novoJogo dados jogador1 jogador2 = do
    putStrLn ("\nIniciando o jogo \"" ++ jogador1 ++ " vs " ++ jogador2 ++ "\" ...")
    putStrLn ("\nOs jogadores que possuem numeros não estAo marcados.")
    putStrLn ("\n" ++ jogador1 ++ " será o \'X\' e " ++ jogador2 ++ " será o \'O\'. Vamos lá!!")
    getChar

    -- 
    rodarJogo dados ['1', '2', '3', '4', '5', '6', '7', '8', '9'] jogador1 jogador2 0

-- Função que exibe o tabuleiro
rodarJogo :: Jogadores -> [Char] -> Nome -> Nome -> Vez -> IO Jogadores
rodarJogo dados tabela jogador1 jogador2 vez = do
    -- Imprimir tabuleiro
    putStrLn ("\n" ++ "                              " ++
            (show (tabela !! 0)) ++ " | " ++ (show (tabela !! 1)) ++ " | " ++ (show (tabela !! 2)) ++
            "\n                              ---------------\n" ++ "                              " ++
            (show (tabela !! 3)) ++ " | " ++ (show (tabela !! 4)) ++ " | " ++ (show (tabela !! 5)) ++
            "\n                              ---------------\n" ++ "                              " ++
            (show (tabela !! 6)) ++ " | " ++ (show (tabela !! 7)) ++ " | " ++ (show (tabela !! 8)) ++
            "\n")

    -- Verifica se jogador 1 venceu
    if (venceuJogador1 tabela) then do
        putStrLn ("Parábens " ++ jogador1 ++ "! Você venceu!!")
        arq_escrita <- openFile "dados.txt" WriteMode
        hPutStrLn arq_escrita (show (atualizaPontuacao dados jogador1))
        hClose arq_escrita

        arq_leitura <- openFile "dados.txt" ReadMode
        dados_atualizados <- hGetLine arq_leitura
        hClose arq_leitura

        putStr "\nPressione <Enter> para voltar ao menu..."
        getChar
        menu (read dados_atualizados)
    else do
        -- Veririca se o jogador2 venceu
        if (venceuJogador2 tabela) then do
            putStrLn ("Parábens " ++ jogador2 ++ "! Você venceu!!")
            arq_escrita <- openFile "dados.txt" WriteMode
            hPutStrLn arq_escrita (show (atualizaPontuacao dados jogador2))
            hClose arq_escrita

            arq_leitura <- openFile "dados.txt" ReadMode
            dados_atualizados <- hGetLine arq_leitura
            hClose arq_leitura

            putStr "\nPressione <Enter> para voltar ao menu..."
            getChar
            menu (read dados_atualizados)
        else do
            -- verifica se foi empate
            -- Se existir algum numero ainda na tabela, então o jogo ainda esta rodando, logo ainda não é empate
            if ((length (intersect "123456789" tabela)) == 0) then do
                putStrLn ("Deu empate!")
                putStr "\nPressione <Enter> para voltar ao menu..."
                getChar
                menu dados
            else do
                -- verifica se a vez é do jogador1
                if (vez == 0) then do
                    putStrLn (jogador1 ++ ", é a sua vez! Onde você quer marcar? ")
                    op <- getChar
                    getChar -- descarta o enter
                    if not (elem op "123456789") then do
                        putStrLn "\nEssa opção não é válida, tente novamente..."
                        rodarJogo dados tabela jogador1 jogador2 0
                    else do
                        -- A opção é valida, mas preciso testar se ja não foi marcada
                        if not (elem op tabela) then do
                            putStrLn "Essa opção já foi marcada, escolha outra opção..."
                            rodarJogo dados tabela jogador1 jogador2 0
                        else
                            rodarJogo dados (obterNovoTabuleiro tabela vez op) jogador1 jogador2 1
                else do
                    putStrLn (jogador2 ++ ", é a sua vez! Onde você quer marcar? ")
                    op <- getChar
                    getChar -- descarta o enter
                    if not (elem op "123456789") then do
                        putStrLn "\nEssa opção não é válida, tente novamente..."
                        rodarJogo dados tabela jogador1 jogador2 1
                    else do
                        -- A opção é valida, mas preciso testar se ja não foi marcada
                        if not (elem op tabela) then do
                            putStrLn "Essa opção já foi marcada, escolha outra opção..."
                            rodarJogo dados tabela jogador1 jogador2 1
                        else
                            rodarJogo dados (obterNovoTabuleiro tabela vez op) jogador1 jogador2 0


obterNovoTabuleiro :: Tabela -> Vez -> Char -> Tabela
obterNovoTabuleiro (x:xs) vez e
                        | ((x == e) && (vez == 0)) = (['X'] ++ xs)
                        | ((x == e) && (vez == 1)) = (['O'] ++ xs)
                        | otherwise = x:(obterNovoTabuleiro xs vez e)

-- Veririfca se jogador 1 venceu
venceuJogador1 :: Tabela -> Bool
venceuJogador1 tabela
                | (((tabela !! 0) == 'X') && ((tabela !! 1) == 'X') && ((tabela !! 2) == 'X')) = True
                | (((tabela !! 3) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 5) == 'X')) = True
                | (((tabela !! 6) == 'X') && ((tabela !! 7) == 'X') && ((tabela !! 8) == 'X')) = True
                -- Colunas
                | (((tabela !! 0) == 'X') && ((tabela !! 3) == 'X') && ((tabela !! 6) == 'X')) = True
                | (((tabela !! 1) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 7) == 'X')) = True
                | (((tabela !! 2) == 'X') && ((tabela !! 5) == 'X') && ((tabela !! 8) == 'X')) = True
                -- Diagonais
                | (((tabela !! 0) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 8) == 'X')) = True
                | (((tabela !! 2) == 'X') && ((tabela !! 4) == 'X') && ((tabela !! 6) == 'X')) = True
                | otherwise = False

-- Veririfca se jogador 2 venceu
venceuJogador2 :: Tabela -> Bool
venceuJogador2 tabela
                | (((tabela !! 0) == 'O') && ((tabela !! 1) == 'O') && ((tabela !! 2) == 'O')) = True
                | (((tabela !! 3) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 5) == 'O')) = True
                | (((tabela !! 6) == 'O') && ((tabela !! 7) == 'O') && ((tabela !! 8) == 'O')) = True
                -- Colunas
                | (((tabela !! 0) == 'O') && ((tabela !! 3) == 'O') && ((tabela !! 6) == 'O')) = True
                | (((tabela !! 1) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 7) == 'O')) = True
                | (((tabela !! 2) == 'O') && ((tabela !! 5) == 'O') && ((tabela !! 8) == 'O')) = True
                -- Diagonais
                | (((tabela !! 0) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 8) == 'O')) = True
                | (((tabela !! 2) == 'O') && ((tabela !! 4) == 'O') && ((tabela !! 6) == 'O')) = True
                | otherwise = False

atualizaPontuacao :: Jogadores -> String -> Jogadores
atualizaPontuacao ((Jogador nome pontuacao):xs) vencedor
                        | (nome == vencedor) = [(Jogador nome (pontuacao + 1))] ++ xs
                        | otherwise = (Jogador nome pontuacao):(atualizaPontuacao xs vencedor)