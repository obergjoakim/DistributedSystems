-module(key).
-export([generate/0,between/3]).

% returns int between 1 and 1000 000 000, 30 bits
generate() -> random:uniform(1000000000).

% will this work??
between(_Key, From, From) -> true;
between(Key, From, To) ->
    case From < To of
        true -> 
            if Key =< To andalso Key > From -> true;
                true -> false
            end;
        % if From > To
        false -> 
            case Key < To of
                    true -> true;
                    false -> 
                        case Key > From of
                            true -> true;
                            false -> false
                        end
                    end
                end.



