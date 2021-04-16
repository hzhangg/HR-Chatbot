:- module(api, [search/2]).


:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(uri)).

api_key('?key=bcf6cda1-420b-4db2-a662-cc922ee0c588').
dict_url('https://www.dictionaryapi.com/api/v3/references/collegiate/json/').


%https://www.dictionaryapi.com/api/v3/references/collegiate/json/WORD?key=bcf6cda1-420b-4db2-a662-cc922ee0c588

search(Word, Response) :-
    generate_url(Word, RequestUrl),
    make_request(RequestUrl, Response). 

generate_url(Word, KeyUrl) :- 
	dict_url(Url),
	api_key(Key),
	string_concat(Url,  Word, WordUrl), %
	string_concat(WordUrl, Key, KeyUrl). % 
	

% example URL we need to feed to make_request:
% https://www.dictionaryapi.com/api/v3/references/collegiate/json/WORD?key=bcf6cda1-420b-4db2-a662-cc922ee0c588
% example of how to format WORD if it has spaces, e.g. "computer science":
% computer%20science
make_request(Url, Response) :-
	http_get(Url, JsonResponse, []),
	atom_json_dict(JsonResponse, Json, []),
	extractHead(Json, Dict),
	checkDict(Dict, Response).

checkDict(Dict, Result) :-
	is_dict(Dict),
	get_dict(shortdef, Dict, Value),
	atomic_list_concat(Value, ' ', Result).

checkDict(Dict, "None Found") :-
	not(is_dict(Dict)).

extractHead([H|_], H).



