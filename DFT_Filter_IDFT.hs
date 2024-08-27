{- Programma Haskell per calcolare la trasformata discreta di Fourier (DFT) di un segnale,
   applicare un filtro e, infine, calcolare la trasformata discreta di Fourier inversa (IDFT) 
   del segnale filtrato. -}

{- Importazione delle librerie. -}

{- Libreria necessaria per:
   - utilizzo della funzione readMaybe, legge una stringa in input, se la stringa può essere 
     interpretata come il valore richiesto restituisce Just, se l'interpretazione non 
     ha successo restituisce Nothing. -}
import Text.Read (readMaybe)

{- Libreria necessaria per:
   - rappresentazione di numeri complessi con coefficienti reali;
   - utilizzo della funzione realPart, restituisce la parte reale di un numero complesso;
   - utilizzo della funzione imagPart, restituisce la parte immaginaria di un numero complesso;
   - utilizzo dell'operatore ':+', utilizzato per la creazione di numeri complessi. -}         
import Data.Complex           
  
{- Main -}
{- Il programma accetta in input una lista finita di numeri reali rappresentante un segnale 
   nel dominio del tempo e la frequenza di campionamento del segnale stesso, converte la 
   lista di numeri reali in una lista di numeri complessi, calcola e stampa la trasformata 
   discreta di Fourier (DFT) del segnale. 
   Successivamente richiede la tipologia di filtro (passa basso, passa alto o passa banda) 
   da applicare al segnale nel dominio delle frequenze e la/e frequenza/e di taglio. 
   Dopo aver stampato il segnale filtrato nel dominio della frequenza, calcola la trasformata 
   discreta inversa di Fourier (IDFT) del segnale e infine stampa il segnale ottenuto nel dominio 
   del tempo. -}  
                            
main :: IO()
main = do
  putStrLn "Progetto per il corso di Programmazione Logica e Funzionale."
  putStrLn "Sessione Autunnale 2022/2023."
  
  putStrLn "\n\nInserire il segnale da filtrare (es. [1, 2.3, -2]): "
  input_signal <- read_input_signal

  putStrLn "\nInserire la frequenza di campionamento del segnale: "
  sampling_rate <- read_double_value

  putStrLn "\nApplicazione della trasformata discreta di Fourier (DFT)."
  let dft_signal = dft (real_to_complex input_signal)
  putStrLn "Segnale nel dominio della frequenza:"
  putStrLn $ show (dft_signal)

  putStrLn "\nSelezione del filtro da applicare al segnale:"
  putStrLn " 1 - Filtro passa basso;"
  putStrLn " 2 - Filtro passa alto;"
  putStrLn " 3 - Filtro passa banda."
  putStrLn "Digitare 1, 2 o 3: "
  filter <- read_filter_choice 

  filtered_signal <- filtering filter sampling_rate dft_signal 
  
  putStrLn "\nSegnale filtrato nel dominio della frequenza:"
  putStrLn $ show (filtered_signal)

  putStrLn "\nApplicazione della trasformata discreta di Fourier inversa (IDFT)."
  let time_domain_filtered_signal = complex_to_real (idft filtered_signal)
  putStrLn "Segnale filtrato nel dominio del tempo:"
  putStrLn $ show (time_domain_filtered_signal)

{- Definizione delle funzioni -}

{- Definizione delle funzioni principali -}

{- Funzione per applicare la trasformata discreta di Fourier (DFT): 
   - il suo unico argomento è il segnale nel dominio del tempo. -}
dft :: [Complex Double] -> [Complex Double]
dft [] = []
dft signal = dft_k_handler signal (length signal) 0

{- Funzione per la gestione dell'indice k per il calcolo della DFT:
   - il primo argomento è il segnale nel dominio del tempo;
   - il secondo argomento è la dimensione del segnale;
   - il terzo argomento è l'indice k, indica l'elemento a cui si sta facendo riferimento. -}
dft_k_handler :: [Complex Double] -> Int -> Int -> [Complex Double]
dft_k_handler [] _ _ = []
dft_k_handler signal size k 
  | k == size = []
  | otherwise = (dft_k_sum signal size k 0) : 
    (dft_k_handler signal size (k + 1))

{- Funzione per effettuare il calcolo del k-esimo elemento della DFT:
   - il primo argomento è il segnale nel dominio del tempo;
   - il secondo argomento è la dimensione del segnale; 
   - il terzo argomento è l'indice k, indica l'elemento del segnale nel dominio della frequenza
     a cui si sta facendo riferimento;
   - il quarto argomento è l'indice n, indica l'elemento del segnale nel dominio del tempo
     a cui si sta facendo riferimento. -}
dft_k_sum :: [Complex Double] -> Int -> Int -> Int -> Complex Double
dft_k_sum [] _ _ _ = 0.0
dft_k_sum (x:xs) size k n =
  x * (cos esp :+ sin esp) + dft_k_sum xs size k (n + 1)
  where
    esp = - 2.0 * pi * fromIntegral n * fromIntegral k / fromIntegral size

{- Funzione per acquisire e validare il-i valore-i della-e frequenza-e di taglio 
  ed eseguire l'applicazione del filtro scelto:
  - il primo argomento rappresenta il filtro selezionato;
  - il secondo argomento è la frequenza di campionamento del segnale;
  - il terzo argomento è il segnale da filtrare. -}
filtering :: Int -> Double -> [Complex Double] -> IO [Complex Double]
filtering _ _ [] = return []
filtering 1 sampling_rate signal = do
  putStrLn "\nInserire la frequenza di taglio per il filtro passa basso:"
  cutoff <- read_double_value
  return (low_pass_filter cutoff sampling_rate signal)
filtering 2 sampling_rate signal = do
  putStrLn "\nInserire la frequenza di taglio per il filtro passa alto:"
  cutoff <- read_double_value
  return (high_pass_filter cutoff sampling_rate signal)
filtering 3 sampling_rate signal = do
  putStrLn "\nInserire la frequenza di taglio inferiore:"
  cutoff_min <- read_double_value
  putStrLn "\nInserire la frequenza di taglio superiore:"
  cutoff_max <- read_double_value
  if cutoff_min >= cutoff_max
    then do
      putStr "Errore, la frequenza di taglio inferiore non deve essere "
      putStrLn "maggiore o uguale della frequenza di taglio superiore!"
      filtering 3 sampling_rate signal
    else return (band_pass_filter cutoff_min cutoff_max sampling_rate signal)

{- Funzione per il filtro passa basso:
   - il primo argomento è la frequenza di taglio;
   - il secondo è la frequenza di campionamento;
   - il terzo argomento è il segnale da filtrare. -}
low_pass_filter :: Double -> Double -> [Complex Double] -> [Complex Double]
low_pass_filter _ _ [] = []
low_pass_filter cutoff sr (x:xs) = x : low_pass_filter_helper cutoff sr 1 0 (length (x:xs)) xs

{- Funzione per applicare il filtro passa basso al segnale meno il primo elemento:
   - il primo argomento è la frequenza di taglio;
   - il secondo argomento è la frequenza di campionamento;
   - il terzo argomento è l'indice dell'elemento in valutazione;
   - il quarto argomento è un parametro di controllo per ottimizzare il calcolo,
     evitando di calcolare la frequenza associata all'elemento quando non necessario;
   - il quinto argomento è il numero degli elementi del segnale;
   - il sesto argomento è il segnale da filtrare. -}
low_pass_filter_helper :: Double -> Double -> Int -> Int -> Int -> [Complex Double] -> [Complex Double]
low_pass_filter_helper _ _ _ _ _ [] = []
low_pass_filter_helper cutoff sr index control size (x:xs)
  | control == 1 = 0 :+ imagPart x : low_pass_filter_helper cutoff sr (index + 1) control size xs
  | fq > cutoff = 0 :+ imagPart x : low_pass_filter_helper cutoff sr (index + 1) 1 size xs
  | otherwise = x : low_pass_filter_helper cutoff sr (index + 1) control size xs
    where
      fq = ((fromIntegral index / fromIntegral size) * sr)

{- Funzione per il filtro passa alto:
   - il primo argomento è la frequenza di taglio;
   - il secondo è la frequenza di campionamento;
   - il terzo argomento è il segnale da filtrare. -}
high_pass_filter :: Double -> Double -> [Complex Double] -> [Complex Double]
high_pass_filter _ _ [] = []
high_pass_filter cutoff sr (x:xs) = x : high_pass_filter_helper cutoff sr 1 (length (x:xs)) xs

{- Funzione per applicare il filtro passa alto al segnale meno il primo elemento:
   - il primo argomento è la frequenza di taglio;
   - il secondo argomento è la frequenza di campionamento;
   - il terzo argomento è l'indice dell'elemento in valutazione;
   - il quarto argomento è il numero degli elementi del segnale;
   - il quinto argomento è il segnale da filtrare. -}
high_pass_filter_helper :: Double -> Double -> Int -> Int -> [Complex Double] -> [Complex Double]
high_pass_filter_helper _ _ _ _ [] = []
high_pass_filter_helper cutoff sr index size (x:xs)
  | fq < cutoff = 0 :+ imagPart x : high_pass_filter_helper cutoff sr (index + 1) size xs
  | otherwise = (x:xs)
    where
      fq = ((fromIntegral index / fromIntegral size) * sr)

{- Funzione per il filtro passa banda:
   - il primo argomento è la frequenza di taglio inferiore;
   - il secondo argomento è la frequenza di taglio superiore;
   - il terzo elemento è la frequenza di campionamento;
   - il quarto elemento è il segnale da filtrare. -}
band_pass_filter :: Double -> Double -> Double -> [Complex Double] -> [Complex Double]
band_pass_filter cutoff_min cutoff_max sr signal = filtered_signal
  where
    low_passed_signal = low_pass_filter cutoff_max sr signal
    filtered_signal = high_pass_filter cutoff_min sr low_passed_signal

{- Funzione per applicare la trasformata discreta inversa di Fourier (IDFT):
   - il suo unico argomento è il segnale nel dominio della frequenza. -}
idft :: [Complex Double] -> [Complex Double]
idft [] = []
idft signal = idft_k_handler signal (length signal) 0

{- Funzione per la gestione dell'indice k per il calcolo della IDFT:
   - il primo argomento è il segnale nel dominio della frequenza;
   - il secondo argomento è la dimensione del segnale;
   - il terzo argomento è l'indice K, indica l'elemento a cui si sta facendo riferimento. -}
idft_k_handler :: [Complex Double] -> Int -> Int -> [Complex Double]
idft_k_handler [] _ _ = []
idft_k_handler signal size k
  | k == size = []
  | otherwise = 
    (idft_k_sum signal size k 0) / fromIntegral size : 
    (idft_k_handler signal size (k + 1))

{- Funzione per effettuare il calcolo del k-esimo elemento della trasformata inversa di Fourier:
   - il primo argomento è il segnale nel dominio della frequenza;
   - il secondo argomento è la dimensione del segnale; 
   - il terzo argomento è l'indice K, indica l'elemento del segnale nel dominio della frequenza
     a cui si sta facendo riferimento;
   - il quarto argomento è l'indice N, indica l'elemento del segnale nel dominio del tempo
     a cui si sta facendo riferimento.  -}
idft_k_sum :: [Complex Double] -> Int -> Int -> Int -> Complex Double
idft_k_sum [] _ _ _ = 0.0
idft_k_sum (x:xs) size k n =
  x * (cos esp :+ sin esp) + idft_k_sum xs size k (n + 1)
  where
    esp = 2.0 * pi * fromIntegral n * fromIntegral k / fromIntegral size 

{- Definizione delle funzioni ausiliarie -}

{- Funzione per acquisire un segnale fornito in input dall'utente,
   se il segnale non è valido viene richiesto un nuovo inserimento. -}
read_input_signal :: IO [Double]
read_input_signal = do   
  input <- getLine   
  case readMaybe input of 
    Just xs -> return xs  
    Nothing -> do
      putStrLn "\nInput non valido. Inserire una lista finita di numeri reali (es. [1, -2.5, 3.2]):" 
      read_input_signal

{- Funzione per acquisire e validare un numero reale positivo fornito 
   in input dall'utente, se il valore non è valido viene richiesto un nuovo inserimento. -}
read_double_value :: IO Double
read_double_value = do   
  input <- getLine    
  case readMaybe input of 
    Just x | x > 0 -> return x
    Just _ -> do
      putStrLn "\nInput non valido. Inserire un numero reale positivo: "
      read_double_value
    Nothing -> do
      putStrLn "\nInput non valido. Inserire un numero reale positivo: "
      read_double_value    

{- Funzione per acquisire e validare la scelta relativa al filtro da utilizzare,
   se l'inserimento non è valido, viene richiesto un nuovo inserimento. -}
read_filter_choice :: IO Int
read_filter_choice = do
  input <- getLine
  case input of
    "1" -> return 1
    "2" -> return 2      
    "3" -> return 3       
    _ -> do
      putStrLn "\nInput non valido. Inserire 1, 2 o 3."
      read_filter_choice      

{- Funzione per convertire una lista di numeri reali in una lista di numeri complessi:
   - il suo unico argomento è la lista di numeri reali da convertire. -}
real_to_complex :: [Double] -> [Complex Double]
real_to_complex  = map (\x -> x :+ 0)  

{- Funzione per convertire una lista di numeri complessi in una lista di numeri reali:
   - il suo unico argomento è la lista di numeri complessi da convertire. -}
complex_to_real :: [Complex Double] -> [Double]
complex_to_real xs = map realPart xs
