-module(common.utils.crc).

-export([calculate/1, crc/2, test/0]).

%%% CRC16 CCITT Normalized Polynomial
-define(POLYNOMIAL, 16#1021).

crc(crc16_CCITT, X)-> crc(8, X, 16#1021). 

crc(NumberOfBits, X, Polinomial) -> crc(NumberOfBits, X, Polinomial, []).

crc(_NumberOfBits, [], _Polinomial, Res) -> Res;
		  
crc(NumberOfBits, [X | Rest], Polinomial, Res) -> 
		  crc(NumberOfBits, Rest, Polinomial, 
		  [calculate(NumberOfBits, X, Polinomial)] ++ Res).

calculate(X) -> calculate(8, X, ?POLYNOMIAL).

calculate(0, X, _Polinomial) -> X;
calculate(N, X, Polinomial) when X band 16#0001 == 1 ->
    calculate(N-1, Polinomial bxor (X bsr 1), Polinomial);
calculate(N, X, Polinomial) ->
    calculate(N-1, X bsr 1, Polinomial).

test() -> todo.
       
