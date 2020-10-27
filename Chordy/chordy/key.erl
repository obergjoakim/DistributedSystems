-module(key).
-export([generate/0,between/3]).

% doesnt work correctly, its not random, two different computers gets the same values with same seed
% returns int between 1 and 1000 000 000, 30 bits
%generate() -> random:uniform(1000000000).

generate() -> rand:uniform(1000000000).


between(_Key, From, From) -> true;
between(Key, From, To) ->
    case From < To of
        true -> 
            if Key =< To andalso Key > From -> true;
                true -> false
            end;
        % if From > To
        false -> 
            case Key =< To of
                    true -> true;
                    false -> 
                        case Key > From of
                            true -> true;
                            false -> false
                        end
                    end
                end.



