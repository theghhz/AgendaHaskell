-- ghc AgendaHaskell.hs -> ./AgendaHaskell

import Data.List (elemIndex) -- buscarContatoPorNome
import System.IO (hFlush, stdout) -- fflush

data Contato = Contato { nome :: String, telefone :: String, endereco :: String, relacao :: String }

instance Show Contato where
    show (Contato n t e r) = "--------------------\nNome: " ++ n ++ "\nTelefone: " ++ t ++ "\nEndereço: " ++ e ++ "\nRelação: " ++ r ++"\n--------------------"

type ListaTelefonica = [Contato]

-- buffer do teclado
limparBuffer :: IO ()
limparBuffer = hFlush stdout

insereOuAltera :: ListaTelefonica -> IO ListaTelefonica
insereOuAltera lista = do
    putStrLn "-- Adicionar contato --"
    putStr "Nome: "
    limparBuffer
    nome <- getLine
    putStr "Telefone: "
    limparBuffer
    telefone <- getLine
    putStr "Endereço: "
    limparBuffer
    endereco <- getLine
    putStr "Relação: "
    limparBuffer
    relacao <- getLine

    let novoContato = Contato nome telefone endereco relacao
    let novaLista = atualizarContato lista novoContato
    putStrLn "Contato adicionado ou alterado com sucesso!"
    return novaLista

-- instance Read Contato where
--     readsPrec _ str =
--         case words str of
--             [nome, telefone, endereco, relacao] -> [(Contato nome telefone endereco relacao, "")]
--             _ -> []

atualizarContato :: ListaTelefonica -> Contato -> ListaTelefonica
atualizarContato lista novoContato =
    case buscarContatoPorNome lista (nome novoContato) of
        Just index -> take index lista ++ [novoContato] ++ drop (index + 1) lista -- Atualiza o contato se existe
        Nothing -> lista ++ [novoContato] -- adiciona contato novo na lista

buscarContatoPorNome :: ListaTelefonica -> String -> Maybe Int -- Maybe é um tipo que pode ser Just ou Nothing / elemenIndex retorna Maybe Int
buscarContatoPorNome lista nomeBusca = elemIndex nomeBusca (map nome lista)

buscarContato :: ListaTelefonica -> IO ()
buscarContato lista = do
    putStrLn "\n----- Buscar contato -----"
    putStr "Digite o nome do contato que deseja buscar: "
    limparBuffer
    nome <- getLine
    case buscarContatoPorNome lista nome of
        Just index -> do
            let contatoEncontrado = lista !! index -- (!!) :: [a] -> Int -> a | retorna o elemento da lista na posição index
            putStrLn "Contato encontrado:"
            putStrLn (show contatoEncontrado)
        Nothing -> putStrLn "Contato não encontrado!"

exibirContatos :: ListaTelefonica -> IO ()
exibirContatos lista = do
    putStrLn "------ Lista de contatos ------"
    mapM_ (\contato -> putStrLn (show contato)) lista

-- não funciona
-- salvarLista :: ListaTelefonica -> FilePath -> IO ()
-- salvarLista lista arquivo = do
--     writeFile arquivo (show lista)
--     putStrLn "A lista telefônica foi salva com sucesso!"

--não funciona com dados no arquivo. Só se tiver sem dados ele carrega e o programa funciona
-- carregarLista :: FilePath -> IO ListaTelefonica
-- carregarLista arquivo = do
--     conteudo <- readFile arquivo
--     let linhas = lines conteudo
--     let listaTelefonica = map readContato linhas
--     putStrLn "A lista telefônica foi carregada com sucesso!"
--     return listaTelefonica
--   where
--     readContato :: String -> Contato
--     readContato str =
--         case dropWhile (/= '{') str of
--             ('{':rest) ->
--                 case reads $ "Contato" ++ rest of
--                     [(contato, _)] -> contato
--                     _ -> error $ "Erro ao ler contato do arquivo: " ++ show str
--             _ -> error $ "Erro ao ler contato do arquivo: " ++ show str

confirmarRemocao :: Contato -> IO Bool
confirmarRemocao contato = do
    putStrLn $ "Confirma a remoção do seguinte contato:\n" ++ show contato
    putStr "Digite 1 para confirmar ou 2 para cancelar: "
    limparBuffer
    opcao <- getLine
    return (opcao == "1" || opcao == "1\n")

-- Função para remover um contato da lista
removeContato :: ListaTelefonica -> IO ListaTelefonica
removeContato lista = do
    putStrLn "----- Remover contato -----"
    putStr "Digite o nome do contato que deseja remover: "
    limparBuffer
    nomeRemover <- getLine

    case buscarContatoPorNome lista nomeRemover of
        Just index -> do
            confirmacao <- confirmarRemocao (lista !! index)
            if confirmacao
                then do
                    let novaLista = take index lista ++ drop (index + 1) lista
                    putStrLn "Contato removido com sucesso!"
                    return novaLista
                else do
                    putStrLn "Remoção cancelada."
                    return lista
        Nothing -> do
            putStrLn "Contato não encontrado!"
            return lista

main :: IO ()
main = do
    let arquivo = "lista_telefonica.txt"
    menu [] arquivo

menu :: ListaTelefonica -> FilePath -> IO ()
menu lista arquivo = do
    putStrLn "\n-- Menu --"
    putStrLn "1. Adicionar contato"
    putStrLn "2. Remover contato"
    putStrLn "3. Buscar contato"
    putStrLn "4. Exibir todos"
    putStrLn "0. Sair"
    putStr "[->] "
    limparBuffer
    opcao <- getLine

    case opcao of
        "0" -> putStrLn "Encerrando o programa..."
        "1" -> do
            novaLista <- insereOuAltera lista
            menu novaLista arquivo
        "2" -> do
            novaLista <- removeContato lista
            menu novaLista arquivo
        "3" -> do
            buscarContato lista
            menu lista arquivo
        "4" -> do
            exibirContatos lista
            menu lista arquivo
        _ -> do
            putStrLn "Opção inválida! Tente novamente."
            menu lista arquivo
