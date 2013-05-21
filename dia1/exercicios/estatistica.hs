{-Usando fold e map, faça uma função para calcular a média aritmética e outra para o desvio padrão de uma lista de números.-}
media lista = (foldl (+) 0 lista) / (foldl (\x y -> x+1) 0 lista)

variancia lista m = media (map (\x -> (x - m)^2) lista)

desvio lista = sqrt $ (variancia lista (media lista))