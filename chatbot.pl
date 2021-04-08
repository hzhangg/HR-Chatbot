
% use halt . to stop program

% start program
hello() :-
    write("What is your name?"), nl,
    readln(Name),
    write("Which position are you applying for?"), nl(),
    readln(position),
    chatbot(position, [val('Name', Name)]).


% read line, succeeds when user is looking for a developer position
chatbot(position, Memory) :-
	write(Memory), nl(),
	dev_position(position, P),
	string_concat("So you want to be a ", P, Reply),
	write(Reply), nl(), flush_output(current_output),
	readln(Ln),
	chatbot(Ln, Memory).



% attempts to match input against developer position
dev_position(['developer'|_], 'Developer').
dev_position(['dev'|_], 'Developer').
dev_position([_|T],X) :-
    dev_position(T,X).