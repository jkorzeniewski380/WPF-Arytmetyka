(* ======= Autor: Jakub Korzeniewski ======= *)
(* ============== Arytmetyka =============== *)
(* ===== Code Review: Jakub Jakubowski ===== *)

(* == Testy: https://gitlab.com/MIMUW-wpf/testy-arytmetyka/-/tree/master/tests == *)


(* Wartosci przechowujemy albo jako zakres [a, b], albo jako sumę zakresów (-inf, a] u [b, inf) *)
type wartosc = Zakres of float * float | SumaZakresow of (float * float)

(* ========== Funkcje pomocnicze ========== *)

(* Funkcja zwracająca początek zakresu 
   wartosc (Zakres) -> float *)
let zakres_od (Zakres (a, _)) = a;;

(* Funkcja zwracająca koniec zakresu 
   wartosc (Zakres) -> float *)
let zakres_do (Zakres (_, b)) = b;;

(* Funkcja sprawdzająca czy liczba a == nan
   float -> bool *)
let is_nan a =
  compare a nan = 0;;

(* Minimum dwóch liczb z uwzględnieniem nan
   float -> float -> float *)
let min a b =
  if is_nan a || a > b then b
  else a;;

(* Maksimum dwóch liczb z uwzględnieniem nan 
   float -> float -> float *)
let max a b =
  if is_nan a || a < b then b
  else a;;

(* Zakres pusty *)
let empty = Zakres (nan, nan);;

(* Zwraca zakres lub sumę zakresów, które sumują się z zakresem (a, b) do (-inf, inf) 
   wartosc (Zakres) -> wartosc *)
let dopelnienie_zakres (a, b) =
  if a = neg_infinity then
    if b = infinity then empty
    else Zakres (b, infinity)
  else
  if b = infinity then Zakres (neg_infinity, a)
  else SumaZakresow ((min a b), (max a b));;

(* Zwraca zakres, który sumuje się z sumą zakresów (a, b) do (-inf, inf) 
   wartosc (SumaZakresow) -> wartosc (Zakres) *)
let dopelnienie_suma_zakres (a, b) = 
  if a <= b then Zakres (a, b)
  else empty;;

(* Funkcja zwracająca wartość przeciwną od w 
   wartosc -> wartosc *)
let przeciwna w =
  match w with
    Zakres (a, b) -> Zakres ((min (-.a)  (-.b)), (max (-.a) (-.b))) |
    SumaZakresow (a, b) -> 
    let dop = dopelnienie_suma_zakres (a, b) in
    let m = zakres_od dop and n = zakres_do dop in
    dopelnienie_zakres (-. m, -. n);;

(* Funkcja zwracająca odwrotność wartości w
   wartosc -> wartosc *)
let odwrotna w =
  match w with 
    Zakres (a, 0.) -> Zakres (neg_infinity, 1. /. a) | 
    Zakres (0., b) -> Zakres (1. /. b, infinity) |
    Zakres (a, b) ->
    if a *. b < 0. then
      dopelnienie_zakres ((min (1. /. a) (1. /. b)), (max (1. /. a) (1. /. b)))
    else
      Zakres ((min (1. /. a) (1. /. b)), (max (1. /. a) (1. /. b))) |
    SumaZakresow (a, b) -> 
    let dop = dopelnienie_suma_zakres (a, b) in
    let m = zakres_od dop and n = zakres_do dop in 
    if m *. n < 0. then
      Zakres ((min (1. /. m) (1. /. n)), (max (1. /. m) (1. /. n)))
    else
      dopelnienie_zakres ((min (1. /. m) (1. /. n)), (max (1. /. m) (1. /. n)));;


(* ========== Koniec funkcji pomocniczych ==========*)


(* Funkcja zwracająca zakres x +- p% 
   float -> float -> wartosc (Zakres) *)
let wartosc_dokladnosc x p =
  let odchylenie = x *. (p /. 100.) in
  Zakres (min (x -. odchylenie) (x +. odchylenie), max (x -. odchylenie) (x +. odchylenie));; 

(* Funkcja zwracająca zakres [x, y]
   float -> float -> wartosc (Zakres) *)
let wartosc_od_do x y =
  Zakres (x, y);;

(* Funkcja zwracajaca zakres [x, x]
   float -> wartosc (Zakres) *)
let wartosc_dokladna x =
  Zakres (x, x);;

(* Funkcja sprawdzająca, czy dana liczba x zawiera się w wartości w
   wartosc -> float -> bool *)
let in_wartosc w x =
  let in_zakres (a, b) x =  (* Funkcja pomocnicza, zwraca czy x zawiera się w zakresie [a, b] *)
    a <= x && x <= b in
  match w with
    Zakres (a, b) -> in_zakres (a, b) x |
    SumaZakresow (a, b) -> in_zakres (neg_infinity, a) x || in_zakres (b, infinity) x;; 

(* Funkcja zwracająca minimum z wartości w
   wartosc -> float *)
let min_wartosc w =
  let min_zakres (a, b) =   (* Funkcja pomocnicza, zwraca minimum w zakresie [a, b] *)
    min a b in
  match w with
    Zakres (a, b) -> min_zakres (a, b) |
    SumaZakresow (a, b) -> min (min_zakres (neg_infinity, a)) (min_zakres (b, infinity));;

(* Funkcja zwracająca maksimum z wartości w
   wartosc -> float *)
let max_wartosc w =
  let max_zakres (a, b) =   (* Funkcja pomocnicza, zwraca maksimum w zakresie [a, b] *)
    max a b in
  match w with
    Zakres (a, b) -> max_zakres (a, b) |
    SumaZakresow (a, b) -> max (max_zakres (neg_infinity, a)) (max_zakres (b, infinity));;

(* Funkcja zwracająca średnią wartość elementów z wartości w 
   wartosc -> float *)
let sr_wartosc w =
  ((min_wartosc w) +. (max_wartosc w)) /. 2.;;


(* Funkcja pomocnicza dla "plus"
   Funkcja zwracająca sumę Sumy Zakresów i drugiej wartości 
   wartosc (SumaZakresow) -> wartosc -> wartosc *)
let sum_dla_suma_zakresow (SumaZakresow (a, b)) v =
  (* dop -> zakres będący dopełnieniem Sumy Zakresów (a, b) *) 
  let dop = dopelnienie_suma_zakres (a, b) in  
  (* m, n -> początek i koniec dop, procedury pomocnicze dla lepszego zapisu *)
  let m = zakres_od dop and n = zakres_do dop in
  match v with
  (* suma SumyZakresów (-inf, m) u (n, inf) oraz Zakresu (p, q) to 
     SumaZakresów (-inf, m + q), u (n + p, inf) *)
    Zakres (p, q) -> 
    (* Sumę Zakresów (-inf, a) u (b, inf) dla a > b możemy zapisać jako (-inf, inf) *)
    if (m +. q) >= (n +. p) then Zakres (neg_infinity, infinity)
    else if (compare (m +. q) nan = 0 || compare (n +. p) nan = 0) then empty
    else dopelnienie_zakres ((m +. q), (n +. p)) |
    (* suma SumyZakresow (-inf, m) u (n, inf) oraz SumyZakresow (-inf, p) u (q, inf) to 
       Zakres(-inf, inf) bez względu na wartości m, n, p, q *)
    SumaZakresow (p, q) -> Zakres (neg_infinity, infinity);;

(* Funkcja pomocnicza dla "plus"
   Funkcja zwracająca sumę Zakresu i drugiej wartości 
   wartosc (Zakres) -> wartosc -> wartosc *)
let sum_dla_zakres (Zakres (a, b)) v =
  match v with
  (* suma Zakresu(a, b) i Zakresu(p,q) to trywialnie Zakres((a+p), (b+q)) *)
    Zakres (p, q) -> Zakres ((a +. p), (b +. q)) |
    (* obliczanie sumy Zakresu(a, b) i SumyZakresow(p, q) zapisane zostało w funkcji
       sum_dla_suma_zakresow *)
    SumaZakresow (p, q) -> sum_dla_suma_zakresow (SumaZakresow (p, q)) (Zakres (a, b));;

(* Funkcja zwracająca sumę dwóch wartości 
   wartosc -> wartosc -> wartosc *)
let plus w v =
  match w with
    SumaZakresow (a, b) -> sum_dla_suma_zakresow (SumaZakresow (a, b)) v |
    Zakres (a, b) -> sum_dla_zakres (Zakres (a, b)) v;;

(* Funkcja zwracająca różnicę dwóch wartości 
   (sumę pierwszej wartości i wartości przeciwnej od drugiej)
   wartosc -> wartosc -> wartosc *)
let minus w v =
  plus w (przeciwna v);;

(* Funkcja zwracająca iloczyn dwóch wartości 
   wartosc -> wartosc -> wartosc *)
let rec razy w v =
  (* Funkcja pomocnicza - obliczanie iloczynu dla SumyZakresów(a, b) i drugiej wartości 
     wartosc (SumaZakresow) -> wartosc -> wartosc *)
  let razy_dla_suma_zakresow (SumaZakresow (a, b)) v = 
    (* dop -> zakres będący dopełnieniem Sumy Zakresów (a, b) *) 
    let dop = dopelnienie_suma_zakres (a, b) in
    (* m, n -> początek i koniec dop, procedury pomocnicze dla lepszego zapisu *)
    let m = zakres_od dop and n = zakres_do dop in
    match v with 
    (* Iloczn dowolnej sumy zakresow i 0. jest równy 0. *)
      Zakres (0., 0.) -> Zakres (0., 0.) |
      Zakres (c, d) -> 
      (* Zwracamy pusty zakres gdy mnożymy przez pusty zakres *)
      if (compare v empty = 0) then empty else 
        (* Jeśli w zakresie zawiera się 0, to dla dowolnej SumyZakresow iloczyn bedzie rowny (-inf, inf) *)
      if in_wartosc (Zakres (c, d)) 0. then 
        Zakres (neg_infinity, infinity)
      else if c > 0. then 
        let left = max (m *. d) (m *. c) and right = min (n *. d) (n *. c) in
        if left >= right then Zakres(neg_infinity, infinity)
        else dopelnienie_zakres (left, right) 
      else przeciwna (razy w (przeciwna v)) |
      SumaZakresow (c, d) ->
      if (in_wartosc (SumaZakresow (a, b)) 0.) then Zakres(neg_infinity, infinity)
      else 
        (* dop2 -> zakres będący dopełnieniem SumyZakresow (c, d) *)
        let dop2 = dopelnienie_suma_zakres (c, d) in
        (* x, y -> początek i koniec dop2, procedury pomocnicze dla lepszego zapisu *)
        let x = zakres_od dop2 and y = zakres_do dop2 in
        let left = max (m *. y) (n *. x) and right = min (n *. y) (m *. x) in
        if left >= right then Zakres(neg_infinity, infinity)
        else dopelnienie_zakres (left, right) in
  (* Funkcja pomocnicza - obliczanie iloczynu dla Zakresu(a, b) i drugiej wartości 
     wartosc (Zakres) -> wartosc -> wartosc *)
  let razy_dla_zakres (Zakres (a, b)) v =
    (* Zwracamy pusty zakres gdy mnożymy przez pusty zakres *)
    if (compare v empty = 0) then empty else 
      match v with
      (* obliczanie iloczynu Zakresu i SumyZakresow zostalo zapisane w funkcji razy_dla_suma_zakresow *)
        SumaZakresow (c, d) -> razy (SumaZakresow (c, d)) (Zakres (a, b)) |
        (* Mnozenie przez 0. -> Wynikiem będzie 0. *)
        Zakres (0., 0.) -> Zakres (0., 0.) |
        Zakres (c, d) -> 
        let left = min (min (a *. c) (a *. d)) (min (b *. c) (b *. d)) and
        right = max (max (a *. c) (a *. d)) (max (b *. c) (b *. d)) in
        Zakres (left, right) in
  if (compare w empty = 0) then empty else 
    match w with 
      SumaZakresow (a, b) -> razy_dla_suma_zakresow (SumaZakresow (a, b)) v |
      Zakres (0., 0.) -> 
      (* Zwracamy pusty zakres jeśli mnożymy przez pusty zakres *)
      if (compare v empty = 0) then empty
      else Zakres(0., 0.) |
      Zakres (a, b) -> razy_dla_zakres (Zakres (a, b)) v;;

(* Funkcja dzielaca dwie wartosci
   wartosc -> wartosc -> wartosc
   Jeśli v = Zakres(0., 0.), to zwracamy zbiór pusty -> Dzielenie przez 0
   W innym przypadku mnożymy w przez odwrotność v. *)
let podzielic w v =
  if (compare v empty = 0 || compare w empty  = 0) then empty else
    match v with
      Zakres(0., 0.) -> empty |
      (_) -> razy w (odwrotna v);;