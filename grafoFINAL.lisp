;Programacao Funcional
;Trabalho Bimestral III 
;Grupo:
	;Edson Monteiro
	;Lucca Portes



(setq g '((
	(0 "curitiba") 
	(1 "pg") 
	(2 "castro") 
	(3 "pinhais") 
	(4 "paranagua")
	(5 "guarapuava")
	(6 "guaratuba")
	(7 "sao paulo")
	) 
	(
	(0 1 1) 
	(0 2 1) 
	(0 3 1)
	(0 4 1)
	(0 0 1)
	(0 6 1)
	(6 3 1)
	(5 6 1)
	(4 3 1)
	(7 1 1)
	)
	)
)


(defun menu ()
	(loop
		(print "")
		(print "Trabalho Bimestral 3 de Programacao Funcional")
		(print "Escolha a alternativa:")
		(print "1 - Carregar Arquivo Pajek")
		(print "2 - Gravar Arquivo Pajek")
		(print "3 - Verificar Grau do Vertice N")
		(print "4 - Verificar Grau de Proximidade do vertice N")
		(print "5 - Verificar Grau de Intermediacao do vertice N")
		(print "6 - Verificar todas as Metricas do Grafo")
		(print "7 - Verificar menor caminho entre vertices Origem e Destino")
		(print "8 - Imprimir grafo")
		(print "0 - Sair")
		(print "Escolha: ")

		(setq option (read))

		(cond 
			((equal 0 option)
				(return T)
			)
			((equal 1 option)
				(print "Digite a nome do arquivo a para carregar:")
				(setq nomeArquivo (read))
				(le_grafo nomeArquivo)
				(print "Arquivo carregado com sucesso!")
			)
			((equal 2 option)
				(print "Digite a nome do arquivo a ser salvo:")
				(setq nomeArquivo (read))
				(grava_grafo g nomeArquivo)
				(print "Arquivo salvo com sucesso!")
			)
			((equal 3 option)
				(print "Digite a origem (id ou Rotulo):")
				(setq origem (read))
				(setq str (format nil "O grau do vertice eh: ~A " (grau origem g)))
				(print str)
			)
			((equal 4 option)
				(setq modoPrint nil)
				(print "Digite a origem (id ou Rotulo):")
				(setq origem (read))
				(setq str (format nil "O  de centralidade de proximidade do vertice eh: ~6,4F " (centralidade_de_proximidade origem))) 
				(setq modoPrint T)
				(print str)
			)
			((equal 5 option)
				(setq modoPrint nil)
				(print "Digite a origem (id ou Rotulo):")
				(setq origem (read))
				(setq str (format nil "O grau de centralidade de intermediacao do vertice eh: ~6,4F " (centralidade_de_intermediacao origem))) 
				(setq modoPrint T)
				(print str)
			)
			((equal 6 option)
				(metricas)
			)
			((equal 7 option)
				(print "Digite a origem (id ou Rotulo):")
				(setq origem (read))
				(print "Digite a destino (id ou Rotulo):")
				(setq destino (read))
				(melhorCaminho origem destino)
			)
			((equal 8 option)
				(print "Vertices:")
				(print vertices)
				(print "")
				(print "Arestas:")
				(print arestas)

			)
		)
		
	)

)


;__________________ 1 carregamento e gravacao
(defun le_grafo (arquivo)
	(let ((str_linha "") (bool_vertice))
		(with-open-file (s arquivo :direction :input)
			(setq str "((")
			(loop
				(setq str_linha  (read-line s nil nil))
				
				(cond
					((null str_linha) (return str))
					((= (length str_linha) 0) str)
					((and (equal bool_vertice nil) (equal "*" (subseq str_linha 0 1))) (setq bool_vertice t))
					((and (equal "*" (subseq str_linha 0 1)) T) (setq str (format nil "~A) (" str)))
					(t (setq str (format nil "~A(~A) " str str_linha)))
				)
			)
			
			(setq str (format nil "~A))" str))
		)
	)
	(setq g (read-from-string str))
	(inicializa_grafo)
	(padroniza_indices)
)



; Gravacao
(defun grava_grafo (grafo nomeArquivo) 
	(let ((str "") (vertices (car grafo)) (arestas (car (cdr grafo))))
		(setq str (format nil "*vertices ~A" (list-length vertices)))
		(dolist (e vertices)
			(setq str (format nil "~A~C~C~A \"~A\"" str #\return #\linefeed (+(car e) 1) (cadr e)))
		)
		(setq str (format nil "~A~C~C*edges" str #\return #\linefeed))
		(dolist (e arestas)
			(setq str (format nil "~A~C~C~A ~A ~A" str #\return #\linefeed (+ (car e) 1) (+ (cadr e) 1) (caddr e)))
		)
		(with-open-file (s nomeArquivo :direction :output)
			(format s "~A" str)
		)
	)
)

;FIM__________________ 1 carregamento e gravacao


;__________________ 2 - calcula grau do vertice
(defun grau (vertice grafo)
	(let ((arestas (car (cdr grafo))) (grau_vertice 0))
		(if (stringp vertice) ; verifica se entrada e str e converte p/ id equivalente
		(setq vertice (stringToId vertice))
		)
		(dolist (e arestas grau_vertice)
			(when (or (equal (car e) vertice) (equal (car (cdr e)) vertice))
				(incf grau_vertice)
			)
		)
	)
)


(defun allGrau ()
	(setq listaGrau nil)
	(dotimes (i tam)
		(setq str (format nil 	"Grau [~A] : ~A || Adjacentes: ~A" i (grau i g) (adjacencias i g)))
		(setq listaGrau (append listaGrau (list (list i (grau i g))) ) ) ; p; usar nas metricas
		(print str)
	)
)
;FIM__________________ 2 - calcula grau do vertice


;__________________ 3 - calcula centralidade_de_proximidade do vertice
(defun centralidade_de_proximidade (i)
	(if (stringp i) ; verifica se entrada e str e converte p/ id equivalente
		(setq i (stringToId i))
	)
	(if (not (indice_valido i i))
		nil
		(let ((closeness 0) (aux 0))
			(dotimes (x tam closeness)
				(if (/= x i)
					(setq closeness (+ closeness (melhorCaminho i x)))
				)
			)
			(float (setq closeness (/ (- tam 1) closeness)))
		)
	)
)


(defun all_centralidade_de_proximidade ()
	(setq modoPrint nil)
	(setq listaProximidade nil)
	(dotimes (i tam)
		(setq str (format nil 	"~A : ~A" i (centralidade_de_proximidade i)))
		(setq listaProximidade (append listaProximidade (list (list i (centralidade_de_proximidade i)))))
		(print str)
	)
	(setq modoPrint T)
)
;FIM__________________ 3 - calcula centralidade_de_proximidade do vertice


;__________________ 3 - calcula centralidade_de_intermediacao do vertice
(defun centralidade_de_intermediacao (x)
	(if (stringp x) ; verifica se entrada e str e converte p/ id equivalente
		(setq x (stringToId x))
	)
	(setq resultado	0)
	(let ((i 0) (j 0))
		(dotimes (i tam)
			(dotimes (j tam)
				(if (and (/= i x) (/= j x))
					(progn
						(setq listaCaminho nil)
						(melhorCaminho i j)
						(if (member x listaCaminho)
							(incf resultado)	
						)		
					)
				)	
			)
		)
	)
	(float (/ resultado (* (- tam 1) (- tam 2) ) ) )
)


(defun all_centralidade_de_intermediacao ()
	(setq modoPrint nil)
	(setq listaIntermediacao nil)
	(dotimes (i tam)
		(setq str (format nil 	"~A : ~A" i (centralidade_de_intermediacao i)))
		(setq listaIntermediacao (append listaIntermediacao (list (list i (centralidade_de_intermediacao i)))))
		(print str)
	)
	(setq modoPrint T)
)
;FIM__________________ 3 - calcula centralidade_de_intermediacao do vertice

;__________________PRINT DAS METRICAS
(defun metricas ()
	(allGrau)
	(all_centralidade_de_proximidade)
	(all_centralidade_de_intermediacao)
	(setq listaGrau (ordenaSubLista listaGrau))
	(setq listaProximidade (ordenaSubLista listaProximidade))
	(setq listaIntermediacao (ordenaSubLista listaIntermediacao))
	(cls)
	(print '____________________________________________________________________________________)
	(setq str (format nil "~C GRAU ~C~C|~C PROXIMIDADE ~C~C|~C INTERMEDIACAO" 
		#\tab #\tab #\tab #\tab #\tab #\tab #\tab   ))
	(print str)
	(setq str (format nil "VERTICE | VALOR ~C|~C VERTICE | VALOR ~C|~C VERTICE | VALOR" 
		#\tab #\tab #\tab #\tab #\tab ))
	(print str)

	(loop
		(if (NULL listaGrau)
			(return)
		)

		(setq grau (car listaGrau))
		(setq proximidade (car listaProximidade))
		(setq intermediacao (car listaIntermediacao))


		(setq str (format nil "~A ~C | ~A ~C~C|~C ~A ~C | ~6,4F ~C|~C ~A ~C | ~6,4F" 
			(car grau) #\tab (cadr grau) #\tab #\tab  #\tab
			(car proximidade) #\tab (cadr proximidade) #\tab #\tab 
			(car intermediacao) #\tab(cadr intermediacao) #\tab ))
		(print str)

		(setq listaGrau (cdr listaGrau))
		(setq listaProximidade (cdr listaProximidade))
		(setq listaIntermediacao (cdr listaIntermediacao))
	)
	(print '____________________________________________________________________________________)
	(print "")
	
);


;FIM__________________PRINT DAS METRICAS

;interface que verifica algumas condicoes p/ execucao do algoritmo
(defun melhorCaminho (origem destino)
	(if (stringp origem) ; verifica se entrada e str e converte p/ id equivalente
		(setq origem (stringToId origem))
	)

	(if (stringp destino) ; verifica se entrada e str e converte p/ id equivalente
		(setq destino (stringToId destino))
	)

	(faz_fechamento) ; algoritmo de warshall

	(cond
		((not (indice_valido origem destino))  ; cond1 -> se o indice nao for valido
			(progn
			(print "Indices nao sao validos!")
			(setq INFINITO INFINITO)
			)
		)
		((equal (getPosicaoMatriz MF origem destino) nil) ; cond2 -> se nao existir caminho entre os 2 vertices
			(progn
				(print "Nao existe caminho entre estes 2 vertices!")
				(setq INFINITO INFINITO)
			)
		)
		((equal origem destino) 0)  ; cond3  -> se origem for igual destino
		(T (melhorCaminhoAlgoritmo origem destino)) ; executa o algoritmo caso todas as condicoes estejam atendidas )
	) 
)

;s =origem, t = destino
(defun melhorCaminhoAlgoritmo (_origem _destino) ; algoritmo de dijkstra
	(let ( (perm (newLista tam)) ;declaracoes
			(corrente nil) (i nil) (k _origem) (dc nil) (j 0)
			(menordist nil) (novadist 0)
		 ); fim declaracoes

		(dotimes(i tam)
			(setPosicao perm i NAOMEMBRO)
			(setPosicao distancia i INFINITO)
			(setPosicao caminho i -1)
		)

		(setPosicao perm _origem MEMBRO)
		(setPosicao distancia _origem 0)
		(setq corrente _origem)

		(loop while (/= corrente _destino) do 
  			(setq menordist INFINITO)
  			(setq dc (getPosicao distancia corrente))
  			(dotimes(i tam)
				(if (equal (getPosicao perm i) nil)
					(progn
						(setq novadist (+ dc (retorna_peso corrente i)))
						(if (< novadist (getPosicao distancia i))
							(progn
								(setPosicao distancia i novadist)
								(setPosicao caminho i corrente)
							)
						)
						(if (< (getPosicao distancia i) menordist)
							(progn
								(setq menordist (getPosicao distancia i))
								(setq k i)
							)
						)
						
					)
				)
			)

			(setq corrente k)
			(setPosicao perm corrente MEMBRO)
  		) 
  		(if (equal modoPrint T) ; caso seja usado por outra funcao , nao imprime
  			(imprimirDijkstra _origem _destino)
  		)
  		(setq listaCaminho (getListaMenorCaminho _origem _destino))
  		(setq pesoTamanhoCaminho (getPosicao distancia _destino))	
  		(setq tamanhoCaminho ( - (length (getListaMenorCaminho _origem _destino)) 1))
	) 
)


(defun imprimirDijkstra (_origem _destino)
	(let ((i (getPosicao caminho _destino)) (lista nil))
		(setq lista (append lista (list (getVertice _destino))))
		(loop while (/= i _origem) do 
			(progn
				(setq lista (append lista (list (getVertice i))))
				(setq i (getPosicao caminho i))	
			)
		)
		(setq lista (append lista (list (getVertice _origem))))
		(setq lista (inverte lista))
		(setq listaCaminho lista)
		(setq str (format nil 	
			"Caminho: ~A  ~C~C Tamanho caminho: ~A ~C~C Peso Caminho: ~A~C~C" 
			lista #\return #\linefeed ( - (length lista) 1) #\return #\linefeed  (getPosicao distancia _destino)  #\return #\linefeed) )
		(print str)
	)  
)
; para pegar a lista com o menor caminho apos executar dijkstra
(defun getListaMenorCaminho (_origem _destino)
	(let ((i (getPosicao caminho _destino)) (lista nil))
		(setq lista (append lista (list (getVertice _destino))))
		(loop while (/= i _origem) do 
			(progn
				(setq lista (append lista (list (getVertice i))))
				(setq i (getPosicao caminho i))	
			)
		)
		(setq lista (append lista (list (getVertice _origem))))
	) 
)

;inicializa matriz fechamento (new)
(defun cria_MF ()
	(setq MF (newLista tam))
	(dotimes (linha tam MF)
		(setq coluna (newLista tam))
		(setPosicao MF linha coluna)
	)
)


;algoritmo de warshall
(defun faz_fechamento ()
	(let ((i 0) (j 0) (k 0))
		(dotimes (i tam)
			(dotimes (j tam)
				(if (isAdjacente i j) 
					(setPosicaoMatriz MF i j T)
					(setPosicaoMatriz MF i j nil)
				)
			)
		)

		(dotimes (k tam)
			(dotimes (i tam)
				(if (equal (getPosicaoMatriz MF i k) T)
					(dotimes (j tam)
						(setPosicaoMatriz MF i j (or (getPosicaoMatriz MF i j) (getPosicaoMatriz MF k j)))
					)
				)
				
			)
		)

	)
)

; retorno o peso da aresta origem-destino
(defun retorna_peso (origem destino)
	(let ((peso INFINITO))
		(dolist (aresta arestas peso)
			(if (and (equal (origem aresta) origem) (equal (destino aresta) destino))
				(setq peso (peso aresta))
			)
			(if (and (equal (origem aresta) destino) (equal (destino aresta) origem))
				(setq peso (peso aresta))
			)
			(if (equal origem destino) 
				(setq peso 0)
			)
		)

	)
)


;________---- AUXILIARES ----________;
;valores entre 0 e tam-1 sao permitidos
(defun indice_valido (origem destino)
	(if (or (equal origem nil) (equal destino nil))
		nil
		(if (and  (>= origem 0) (< destino tam)) 
			T
			nil
		)
	)
	
)

;retorna a str do vertice com base no Id do vertice
(defun idToString (idVertice)
	(let ((strVertice nil))
		(dolist (vertice (car g) strVertice)
			(if (equal (car vertice) idVertice)
				(setq strVertice (cadr vertice))
			)
		)
	)	
)

;retorna o Id do vertice com base na string do vertice
(defun stringToId (strVertice)
	(let ((idVertice nil))
		(dolist (vertice (car g) idVertice)
			(if (equal (cadr vertice) strVertice)
				(setq idVertice	(car vertice))
			)
		)
	)	
)

(defun getVertice (idVertice)
	;(- (car (getPosicao vertices i)) 1)
	(let ((retorno nil))
		(dolist (vertice vertices retorno)
			(if (equal (car vertice) idVertice)
				(setq retorno (car vertice))
			)
		)
	)
	
)

;retorna o vertice de origem da aresta
(defun origem (aresta)
	(car aresta)
)

;retorna o vertice de destino da aresta
(defun destino (aresta)
	(cadr aresta)
)

;retorna o peso da aresta
(defun peso (aresta)
	(caddr aresta)
)

;verifica se origem e adjacente de destino
(defun isAdjacente (origem destino)
	(let ((listaAdjacentes (adjacencias origem g)))
		(if (member destino	listaAdjacentes)
			t	
		)	
	)
)


(defun tamanho(grafo)
	(length (car grafo))
)

;retorna uma lista de tamanho n inicializada com nil; listaRetorno = new ListaRetorno[tamanho]
(defun newLista (tamanho)
	(setq listaRetorno nil)
	(dotimes (i tamanho listaRetorno)
		(setq listaRetorno (append listaRetorno (list nil)))
	)
)

;seta elemento da posicao com o novo valor lista[posicao] = newValue
(defun setPosicao (lista posicao newValue)
	(setf (nth posicao lista) newValue)
)


;retorna elemento da posicao ; return lista[posicao]
(defun getPosicao (lista posicao)
	(nth posicao lista)
)

(defun getPosicaoMatriz (lista i j)
	(let ((coluna (getPosicao MF i)))
		(getPosicao coluna j)
	)
)

(defun setPosicaoMatriz (lista i j newValue)
	(let ((coluna (getPosicao MF i)))
		(setPosicao coluna j newValue)
	)
)
;inverte uma lista
(defun inverte (lista)
	(let ((retorno nil))
		(dolist (e lista retorno)
			(setq retorno (append (list e) retorno))
		)
	)
)





;________________________________


;retorna adjacencias bidirecional
(defun adjacencias (vertice grafo)
	(let ((lista_adj) (arestas (car (cdr grafo))))
		(dolist (e arestas lista_adj)
			(cond
				((equal (car e) vertice) (setq lista_adj (append lista_adj (list (car (cdr e))))))
				((equal (car (cdr e)) vertice) (setq lista_adj (append lista_adj (list (car e)))))
			)
		)
	)
)


(defun imprimeMatriz(matriz)
	(dolist (linha matriz)
		(print linha)
	)
	(print T)
)

(defun ordenaLista (lista)
	(sort lista #'<)
)
;com base no 2 parametro da sublista, ordem decrescente
(defun ordenaSubLista (lista)
 	(sort (copy-seq lista) #'> :key #'cadr)
)

(defun cls ()
	(setq str nil)
  	(dotimes (i 30) 
   		(setq str (format nil "~C~C" #\return #\linefeed )) 
    	(print str)
  	)
);



(defun padroniza_indices ()
	;para mudar indice dos vertices (1 p/ 0) (11 p/ 10) etc.
	(dotimes (i tam)
		(setq newPadrao (getPosicao vertices i))
		;(print newPadrao)
		(setPosicao newPadrao 0 (- (car newPadrao) 1)) ; decrementa o indice do vertice
		;(print newPadrao)
		(setPosicao vertices i newPadrao)
	)
	;para mudar os indices das arestas
	(dotimes (i (length arestas))
		(setq newPadrao (getPosicao arestas i))
		(setPosicao newPadrao 0 (- (car newPadrao) 1)) ; decrementa o indice do vertice origem
		(setPosicao newPadrao 1 (- (cadr newPadrao) 1)) ; decrementa o indice do vertice destino
		(setPosicao arestas i newPadrao)
	)
)

(defun inicializa_grafo ()
	;VARIAVEIS DO GRAFO
	;CLASS GRAFO { 
	
	;______ DADOS SOBRE O GRAFO
	(setq vertices (car g))
	(setq arestas (cadr g))
	(setq tam (tamanho g))

	;_______PARA ALGORITMO DIJKSTRA E WARSHALL
	(setq caminho (newLista tam))
	(setq NAOMEMBRO nil)
	(setq MEMBRO T)
	(setq INFINITO 99999)
	(setq distancia (newLista tam))
	(setq MF (cria_MF))

	(setq tamanhoCaminho 0)
	(setq pesoTamanhoCaminho 0)
	(setq listaCaminho nil)
	;______

	;______PARA GUARDAR DADOS DAS METRICAS
	(setq listaProximidade nil); vertice, coef
	(setq listaIntermediacao nil) ;vertice, coef
	(setq listaGrau nil); vertice, grau

	(setq modoPrint T)
	


; }
)

(inicializa_grafo)