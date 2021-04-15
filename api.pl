:- module(api, [search/2]).

:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(uri)).

api_key('bcf6cda1-420b-4db2-a662-cc922ee0c588').
dict_url('https://www.dictionaryapi.com/api/v3/references/collegiate/json/voluminous?key=bcf6cda1-420b-4db2-a662-cc922ee0c588').

search(Query, Response) :-
    uri_encoded(query_value, Query, EncodedQuery),
    generate_url([("query", EncodedQuery)], RequestUrl),
    make_request(RequestUrl, Response). 

generate_url(Params, NewUrl) :- 
    add_query_params(dict_url, Params, NewUrl).

add_query_params(Url, [], Url).
add_query_params(Url, [(Key, Val)|Tail], NewUrl) :- 
	make_query_param(Key, Val, Param),
	string_concat(Url, Param, NextUrl),
	add_query_params(NextUrl, Tail, NewUrl).

make_query_param(Key, Val, Param) :-
	string_concat("&", Key, Front),
	string_concat("=", Val, Back),
	string_concat(Front, Back, Param).

make_request(Url, Response) :-
	http_get(Url, JsonResponse, []),
	atom_json_dict(JsonResponse, Response, []).
