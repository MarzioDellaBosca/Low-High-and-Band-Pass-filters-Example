/* Programma Prolog per calcolare la trasformata discreta di Fourier (DFT) di un segnale,
   applicare un filtro e, infine, calcolare la trasformata discreta di Fourier inversa (IDFT) 
   del segnale filtrato. */

/* Main */
/* Il programma accetta in input una lista finita di numeri reali rappresentante un segnale 
   nel dominio del tempo e la frequenza di campionamento del segnale stesso, converte la 
   lista di numeri reali in una lista di numeri complessi, calcola e stampa la trasformata 
   discreta di Fourier (DFT) del segnale. 
   Successivamente richiede la tipologia di filtro (passa basso, passa alto o passa banda) 
   da applicare al segnale nel dominio delle frequenze e la-e frequenza-e di taglio. 
   Dopo aver stampato il segnale filtrato nel dominio della frequenza, calcola la trasformata 
   discreta inversa di Fourier (IDFT) del segnale e infine stampa il segnale ottenuto nel dominio 
   del tempo. */

main :-
  write('Progetto per il corso di Programmazione Logica e Funzionale'), nl,
  write('Sessione Autunnale 2022/2023'), nl, nl, nl,

  write('Inserire il segnale da filtrare (es. [1, 2.3, -2]): '), nl,
  read_input_signal(RealInputSignal),
  real_to_complex(RealInputSignal, ComplexInputSignal), nl,

  write('Inserire la frequenza di campionamento del segnale: '), nl,
  read_double_value(SamplingRate), nl,

  write('Applicazione della trasformata discreta di Fourier (DFT).'), nl,
  dft(ComplexInputSignal, DftSignal), 
  write('Segnale nel dominio della frequenza:'), nl,
  print_complex_list(0, DftSignal), nl, nl,
  
  write('Selezione del filtro da applicare al segnale:'), nl,
  write(' 1 - Filtro passa basso'), nl,
  write(' 2 - Filtro passa alto'), nl,
  write(' 3 - Filtro passa banda'), nl,
  write('Digitare 1, 2 o 3: '), nl, 
  read_filter_choice(Filter), nl,
  filtering(Filter, SamplingRate, DftSignal, FilteredSignal), nl,

  write('Segnale filtrato nel dominio della frequenza:'), nl,
  print_complex_list(0, FilteredSignal), nl, nl,
  
  write('Applicazione della trasformata discreta di Fourier inversa (IDFT).'), nl,
  idft(FilteredSignal, IdftSignal), 
  write('Segnale filtrato nel dominio del tempo:'), nl,
  complex_to_real(IdftSignal, TimeDomainFilteredSignal), 
  write(TimeDomainFilteredSignal).
  

/* Definizione dei predicati */

/* Definizione dei predicati principali */

/* Predicato per applicare la trasformata discreta di Fourier (DFT):
   - il primo argomento è il segnale nel dominio del tempo;
   - il secondo argomento è il segnale convertito nel dominio delle frequenze. */
dft([], []).
dft([X | Xs], DFT) :- 
  size([X | Xs], Size),
  dft_k_handler([X | Xs], Size, 0, DFT).

/* Predicato per la gestione dell'indice k per il calcolo della DFT:
   - il primo argomento è il segnale nel dominio del tempo;
   - il secondo argomento è la dimensione del segnale;
   - il terzo argomento è l'indice k, indica l'elemento a cui si sta facendo riferimento; 
   - il quarto argomento è la DFT del segnale. */                   
dft_k_handler([], _, _, []).
dft_k_handler(InputSignal, Size, K, DFT) :- 
  ((K == Size) ->
    DFT = []
  ;
    K1 is K + 1,
    dft_k_handler(InputSignal, Size, K1, DftTail),
    dft_k_sum(InputSignal, Size, K, 0, DftKElement),
    append([DftKElement], DftTail, DFT)).

/* Predicato per effettuare il calcolo del k-esimo elemento della DFT:
   - il primo argomento è il segnale nel dominio del tempo;
   - il secondo argomento è la dimensione del segnale; 
   - il terzo argomento è l'indice k, indica l'elemento del segnale nel dominio della frequenza
     a cui si sta facendo riferimento;
   - il quarto argomento è l'indice n, indica l'elemento, del segnale nel dominio del tempo
     a cui si sta facendo riferimento. 
   - il quinto argomento è il k-esimo elemento della DFT calcolato. */
dft_k_sum([], _, _, _, (0, 0)).
dft_k_sum([Head | Tail], Size, K, N, DftKElement) :- 
  N1 is N + 1,
  Esp is (-2 * pi * N * K / Size),
  dft_k_sum(Tail, Size, K, N1, DftTail),
  complex_prod(Head, (cos(Esp), sin(Esp)), Prod),
  complex_sum(Prod, DftTail, DftKElement).

/* Predicato per acquisire e validare il-i valore-i della-e frequenza-e di taglio 
   ed eseguire l'applicazione del filtro scelto:
   - il primo argomento rappresenta il filtro selezionato;
   - il secondo argomento è la frequenza di campionamento del segnale;
   - il terzo argomento è il segnale da filtrare;
   - il quarto argomento è il segnale filtrato. */
filtering(_, _, [], []).
filtering(1, SamplingRate, Signal, FilteredSignal) :-
    write('Inserire la frequenza di taglio '), 
    write('per il filtro passa basso:'), nl,
    read_double_value(Cutoff),
    low_pass_filter(Cutoff, SamplingRate, Signal, FilteredSignal).
filtering(2, SamplingRate, Signal, FilteredSignal) :-
    write('Inserire la frequenza di taglio '), 
    write('per il filtro passa alto:'), nl,
    read_double_value(Cutoff),
    high_pass_filter(Cutoff, SamplingRate, Signal, FilteredSignal).
filtering(3, SamplingRate, Signal, FilteredSignal) :-
    repeat,
    write('Inserire la frequenza di taglio inferiore: '), nl,
    read_double_value(CutoffMin), nl,
    write('Inserire la frequenza di taglio superiore: '), nl,
    read_double_value(CutoffMax),
    (CutoffMin >= CutoffMax -> 
      write('Errore, la frequenza di taglio inferiore non deve '), 
      write('essere maggiore o uguale della frequenza di taglio superiore!'), 
      nl, nl, fail
    ;
      band_pass_filter(CutoffMin, CutoffMax, SamplingRate, Signal, FilteredSignal), !
    ).

/* Predicato per il filtro passa basso:
   - il primo argomento è la frequenza di taglio;
   - il secondo argomento è la frequenza di campionamento del segnale;
   - il terzo argomento è il segnale da filtrare;
   - il quarto argomento è il segnale filtrato. */
low_pass_filter(_, _, [], []).
low_pass_filter(Cutoff, SamplingRate, [Head | Rest], [Head | FilteredTail]) :-
  size([Head | Rest], Size),
  low_pass_helper(Cutoff, 1, 0, Size, SamplingRate, Rest, FilteredTail).

/* Predicato per applicare il filtro passa basso al segnale meno il primo elemento:
   - il primo argomento è la frequenza di taglio;
   - il secondo argomento è l'indice dell'elemento in valutazione;
   - il terzo argomento è un parametro di controllo per ottimizzare il calcolo,
     evitando di calcolare la frequenza associata all'elemento quando non necessario;
   - il quarto argomento è il numero di elementi del segnale;
   - il quinto argomento è la frequenza di campionamento del segnale;
   - il sesto argomento è il segnale da filtrare; 
   - il settimo argomento è il segnale filtrato. */
low_pass_helper(_, _, _, _, _, [], []).
low_pass_helper(Cutoff, Index, 1, Size, SR, [(_, Img) | Tail], [FilteredHead | FilteredTail]) :-
  FilteredHead = (0, Img),
  low_pass_helper(Cutoff, Index + 1, 1, Size, SR, Tail, FilteredTail).
low_pass_helper(Cutoff, Index, 0, Size, SR, [Head | Tail], [FilteredHead | FilteredTail]) :-
  Head = (Real, Img),
  ((Index / Size * SR) > Cutoff ->
    FilteredHead = (0, Img),
    low_pass_helper(Cutoff, Index + 1, 1, Size, SR, Tail, FilteredTail)
  ;
    FilteredHead = (Real, Img),
    low_pass_helper(Cutoff, Index + 1, 0, Size, SR, Tail, FilteredTail)
  ).

/* Predicato per il filtro passa alto:
   - il primo argomento è la frequenza di taglio;
   - il secondo argomento è la frequenza di campionamento del segnale;
   - il terzo argomento è il segnale da filtrare;
   - il quarto argomento è il segnale filtrato. */
high_pass_filter(_, _, [], []).
high_pass_filter(Cutoff, SR, [Head | Rest], [Head | FilteredTail]) :-
  size([Head | Rest], Size),
  high_pass_helper(Cutoff, 1, Size, SR, Rest, FilteredTail).

/* Predicato per applicare il filtro passa alto al segnale meno il primo elemento:
   - il primo argomento è la frequenza di taglio;
   - il secondo argomento è l'indice dell'elemento in valutazione;
   - il terzo argomento è il numero degli elementi del segnale;
   - il quarto argomento è la frequenza di campionamento del segnale;
   - il quinto argomento è il segnale da filtrare;
   - il sesto argomento è il segnale filtrato. */
high_pass_helper(Cutoff, Index, Size, SR, [(Real, Img) | Tail], [FilteredHead | FilteredTail]) :-
  ((Index / Size * SR) < Cutoff ->
    FilteredHead = (0, Img),
    high_pass_helper(Cutoff, Index + 1, Size, SR, Tail, FilteredTail)
  ;
    FilteredHead = (Real, Img),
    FilteredTail = Tail
  ).

/* Predicato per il filtro passa banda:
   - il primo argomento è la frequenza di taglio inferiore;
   - il secondo argomento è è la frequenza di taglio superiore;
   - il terzo argomento è la frequenza di campionamento del segnale;
   - il quarto argomento è il segnale da filtrare;
   - il quinto argomento è il segnale filtrato. */
band_pass_filter(CutoffMin, CutoffMax, SR, Signal, FilteredSignal) :-
    low_pass_filter(CutoffMax, SR, Signal, LowPassFilterResult),
    high_pass_filter(CutoffMin, SR, LowPassFilterResult, FilteredSignal).

/* Predicato per applicare la trasformata discreta inversa di Fourier (IDFT):
   - il primo argomento è il segnale nel dominio della frequenza;
   - il secondo argomento è il segnale convertito nel dominio del tempo. */
idft([], []).
idft([X | Xs], IDFT) :- 
  size([X | Xs], Size),
  idft_k_handler([X | Xs], Size, 0, IDFT).            

/* Predicato per la gestione dell'indice k per il calcolo della IDFT:
   - il primo argomento è il segnale nel dominio della frequenza;
   - il secondo argomento è il numero di elementi segnale;
   - il terzo argomento è l'indice N, indica l'elemento a cui si sta facendo riferimento; 
   - il quarto argomento è il segnale convertito nel dominio del tempo. */  
idft_k_handler([], _, _, []).
idft_k_handler(InputSignal, Size, N, IDFT) :-
  ((N == Size) -> IDFT = []
  ;
    N1 is N + 1,
    idft_k_handler(InputSignal, Size, N1, IdftTail),
    idft_k_sum(InputSignal, Size, 0, N, IdftKElement),
    complex_div_real(IdftKElement, Size, Prod),
    append([Prod], IdftTail, IDFT)).

/* Predicato per effettuare il calcolo del k-esimo elemento della trasformata inversa di Fourier:
   - il primo argomento è il segnale nel dominio della frequenza;
   - il secondo argomento è il numero di elementi segnale;
   - il terzo argomento è l'indice K, indica l'elemento, del segnale nel dominio della frequenza
     a cui si sta facendo riferimento;
   - il quarto argomento è l'indice N, indica l'elemento, del segnale nel dominio del tempo
     a cui si sta facendo riferimento. 
   - il quinto argomento è il k-esimo elemento della IDFT calcolato. */
idft_k_sum([], _, _, _, (0, 0)).
idft_k_sum([Head | Tail], Size, K, N, IdftKElement) :- 
  K1 is K + 1,
  Esp is (2 * pi * K * N / Size),
  idft_k_sum(Tail, Size, K1, N, IdftTail),
  complex_prod(Head, (cos(Esp), sin(Esp)), Prod),
  complex_sum(Prod, IdftTail, IdftKElement). 

/* Predicati ausiliari */

/* Predicato per acquisire un segnale fornito in input dall'utente,
   se il segnale non è valido viene richiesto un nuovo inserimento:
   - il suo unico argomento è il segnale validato. */
read_input_signal(List) :-
  repeat,
  catch(read(List), _, fail),
  (valid_real_list(List) -> 
    !
  ; 
    nl, write('Input non valido. Inserire una lista finita di numeri reali (es. [1, -2.5, 3.2]): '), 
    nl, fail).

/* Predicato per validare una lista di numeri reali, verifica che ogni elemento presente sia
   un numero reale:
   - il suo unico argomento è la lista di numeri reali validata. */
valid_real_list([]).
valid_real_list([X | Rest]) :-
  number(X),
  valid_real_list(Rest).

/* Predicato per acquisire e validare un numero reale positivo fornito 
   in input dall'utente, se il valore non è valido viene richiesto un nuovo inserimento:
   - il suo unico argomento è il valore della frequenza di taglio, rappresentato da un numero reale. */
read_double_value(Value) :-
    repeat,
    read(Value),
    (number(Value) ->
        (Value > 0 ->
            !
        ;
            nl, write('Input non valido. Inserire un numero reale positivo: '), 
            nl, fail
        )
    ;
        nl, write('Input non valido. Inserire un numero reale positivo: '), 
        nl, fail
    ).

/* Predicato per acquisire e validare la scelta relativa al filtro da parte dell'utente,
   se l'inserimento non è valido, viene richiesto un nuovo inserimento. 
   - il suo unico argomento è il valore rappresentante la scelta del filtro. */   
read_filter_choice(Filter) :-
  repeat,
  catch(read(Filter), _, fail),
  (member(Filter, [1, 2, 3])  ->
    !
  ;
    nl, write('Input non valido. Inserire 1, 2 o 3.'), 
    nl, fail).
               
/* Predicato per convertire una lista di numeri reali in una lista di numeri complessi:
   - il primo argomento è la lista di numeri reali da convertire;
   - il secondo argomento è la lista di numeri complessi ottenuta. */
real_to_complex([], []).
real_to_complex([X | RealTail], [(X, 0) | ComplexTail]) :-
  real_to_complex(RealTail, ComplexTail).
real_to_complex([X], [(X, 0)]).
  
/* Predicato per convertire una lista di numeri complessi in una lista di numeri reali:
   - il primo argomento è la lista di numeri complessi da convertire;
   - il secondo argomento è la lista di numeri reali ottenuta. */
complex_to_real([], []).
complex_to_real([(Real, _) | Tail], [RealHead | RealTail]) :-
  RealHead is Real,
  complex_to_real(Tail, RealTail).

/* Predicato per stampare una lista di numeri complessi in modo leggibile:
   - il primo argomento è un valore di controllo per gestire la stampa;
   - il secondo argomento è la lista di numeri complessi da stampare. */
print_complex_list(_, []) :- write('[]').
print_complex_list(0, [(Real, Imag) | []]) :-
  format('[~w :+ ~w]', [Real, Imag]).
print_complex_list(0, [(Real, Imag) | ComplexTail]) :- 
  format('[~w :+ ~w', [Real, Imag]),
  print_complex_list(1, ComplexTail).
print_complex_list(1, [(Real, Imag) | []]) :-
  format(', ~w :+ ~w]', [Real, Imag]).
print_complex_list(1, [(Real, Imag) | ComplexTail]) :-
  format(', ~w :+ ~w', [Real, Imag]),
  print_complex_list(1, ComplexTail).

/* Predicato per calcolare il prodotto tra due numeri complessi:
   - il primo argomento è il primo dei due numeri complessi;
   - il secondo argomento è il secondo dei due numeri complessi;
   - il terzo argomento è il prodotto dei due numeri complessi. */
complex_prod((Real1, Imag1), (Real2, Imag2), (ProdReal, ProdImag)) :-
  ProdReal is Real1 * Real2 - Imag1 * Imag2,
  ProdImag is Real1 * Imag2 + Imag1 * Real2.

/* Predicato per calcolare la divisione tra due numeri complessi:
   - il primo argomento è il dividendo;
   - il secondo argomento è il divisore;
   - il terzo argomento è quoziente. */
complex_div_real((Real, Imag), Divisor, (DivReal, DivImag)) :-
  DivReal is Real / Divisor,
  DivImag is Imag / Divisor.

/* Predicato per calcolare la somma tra due numeri complessi:
   - il primo argomento è il primo dei due numeri complessi;
   - il secondo argomento è il secondo dei due numeri complessi;
   - il terzo argomento è la somma dei due numeri complessi. */
complex_sum((Real1, Imag1), (Real2, Imag2), (SumReal, SumImag)) :-
  SumReal is Real1 + Real2,
  SumImag is Imag1 + Imag2.

/* Predicato per calcolare il numero di elementi in una lista:
   - il primo argomento è la lista di cui calcolare la dimensione;
   - il secondo argomento è la dimensione della lista summenzionata. */
size([], 0).
size([_ | Tail], Size) :-
  size(Tail, TailSize),
  Size is TailSize + 1.
  
