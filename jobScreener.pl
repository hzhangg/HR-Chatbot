% Start of our Project
:- dynamic prop/3.

ask() :-
    flush_output(current_output),
    write('\n Welcome to the HR Bot'), 
	write('\n I am a personal assistant bot. \n'),
	write('\n Select an option: \n
        1. Look for a Job Posting \n
        2. Create your Resume \n
        3. Update your Resume \n
        4. Show your Resume \n 
        5. Measure your Resume \n
        6. Save your Resume to a new file \n
        7. Quit \n'),
    readTerminal(Input),
    query(Input).

% ask() -> query() -> filterFunction() -> ask()

% ========================
% Options
% ========================

% Filter jobs to find one suited for a user based on their input 
query(Input) :-
  Input = '1',
  write('\n What location are you looking for? \n'),
  readTerminal(Location),
  write('\n What industry are you looking to work in? \n'),
  readTerminal(Industry),
  write('\n What job position are you looking for? \n'),
  readTerminal(Position),
  write('\n What salary do you desire? (dollars per hour) \n'),
  readTerminal(S),
  checkNumber(S, Salary),
  write('\n Are you looking for full time work? Enter \'1\' if so, \'0\' if not \n'),
  readTerminal(I1),
  checkNumber(I1, IsFullTime),
  write('\n Are you looking for remote work? Enter \'1\' if so, \'0\' if not \n'),
  readTerminal(I2),
  checkNumber(I2, IsRemote),
  jobFilter(Location, Industry, Position, Salary, IsFullTime, IsRemote).

% Create a users resume
query(Input) :-
  Input = '2', 
  write('\n What is your name? \n'),
  readTerminal(Username),
  checkUserQ2(Username).

% Update a users resume
query(Input) :-
  Input = '3',
  write('\n What is your name? \n'),
  readTerminal(Username),
  checkUserQ3(Username).

% Prints a users resume
query(Input) :- 
  Input = '4',
  write('\n What is your name? \n'),
  readTerminal(Username),
  print(Username),
  checkUserQ4(Username).

% Measures a users resume
query(Input) :- 
  Input = '5',
  write('\n What is your name? \n'),
  readTerminal(Username),
  checkUserQ5(Username).

% Saves a users resume to a new file
query(Input) :- 
  Input = '6',
  write('\n What is your name? \n'),
  readTerminal(Username),
  checkUserQ6(Username).

% Quits program
query(Input) :- 
  Input = '7',
  halt(0).

query(Input) :-
    not(member(Input, ['1', '2', '3', '4', '5', '6', '7'])),
    write('\n Invalid Input \n'),
    ask().

% ===========
% Parses user input
% =========
readTerminal(Out) :-
  flush_output(current_output),
  readln(Ln),
  atomic_list_concat(Ln, ' ', Out).

checkNumber('x', 'x').
checkNumber(In, Out) :-
    atom_number(In, Out).

checkNumber(In, Out2) :-
    not(atom_number(In, _)),
    write("\n Not a number. Try again \n"),
    readTerminal(In2),
    checkNumber(In2, Out2).  


% ===========
% Iterate over list of jobs
% =========

measureJobList(_, []).
measureJobList(Username, [J|T]) :-
    findQualifications(Username, ULangList, UProgList, UEd, UExpList), % Maybe move out
    findQualifications(J, JLangList, JProgList, JEd, JExpList),
    % Start measuring
    checkWithin(ULangList, JLangList, LangScore),
    checkWithin(UProgList, JProgList, ProgScore),
    measureEducation(UEd, JEd, EduScore),
    measureExperience(UExpList, JExpList, ExpScore),
    getTotalQualScore(J, TotalQ),
    TotalScore is LangScore + ProgScore + EduScore + ExpScore, 
    PercentQ is (TotalScore / TotalQ) * 100,
    write('\n You are '),
    write(PercentQ),
    write('% qualified to work job number '),
    write(J),
    write('! \n'),
    measureJobList(Username, T).


% ========================
% Measuring Functions
% ========================
getTotalQualScore(J, Jobtotal) :-
    findQualifications(J, LangL, ProgL, _, ExpL),
    length(LangL, LangSum),
    length(ProgL, ProgSum),
    length(ExpL, ExpSum),
    Jobtotal is LangSum + ProgSum + 1 + ExpSum.
    
% We can use for
% - Languages
% - Programs 
checkWithin([], _, 0).
checkWithin([H|T], List, Score) :- 
    member(H, List), % The comparator
    checkWithin(T, List, ScoreNew),
    Score is ScoreNew + 1.
    
checkWithin([H|T], List, Score) :-
    not(member(H, List)), % The comparator
    checkWithin(T, List, Score).

% We use for Education 
% ['HighSchool', 'Undergraduate', 'Masters']
measureEducation(UserEdu, 'Masters', 1) :-
    member(UserEdu, ['Masters']).
measureEducation(UserEdu, 'Undergraduate', 1) :- 
    member(UserEdu, ['Undergraduate', 'Masters']).
measureEducation(UserEdu, 'HighSchool', 1) :- 
    member(UserEdu, ['HighSchool','Undergraduate', 'Masters']).
measureEducation(_,_ , 0).


% We use for Experience
% base case
measureExperience([], _, 0).
% case where programming language is a job requirement and user does have enough experience
measureExperience([(P, E)|T], List, Score) :- 
    member((P, X), List), 
    atom_number(X, XN),
    atom_number(E, EN),
    XN =< EN, 
    measureExperience(T, List, ScoreNew), 
    Score is ScoreNew + 1. 
    
% case where programming language is a job requirement and user does NOT have enough experience
measureExperience([(P, E)|T], List, Score) :-
    member((P, X), List),
    atom_number(X, XN),
    atom_number(E, EN),
    EN < XN,
    measureExperience(T, List, Score).

% case where programming language is not a job requirement 
measureExperience([(P, _)|T], List, Score) :-
    not(member((P, _), List)),
    measureExperience(T, List, Score).


% Unused Function
compareYear(_, [], 0).
compareYear((P,E), [(P, X)| _], 1) :-
    E >= X.

compareYear((P,E), [(P, X)| T], Score) :-
    E < X,
    compareYear((P,E), T, Score).

compareYear((P,E), [(D, _)| T], Score) :-
    dif(P, D),
    compareYear((P,E), T, Score).

    
% ========================
% Program Helper Functions
% ========================

% ---------------------------
% Query 1: Job Posting Filter
% ---------------------------
jobFilter(Loc, Ind, Pos, Sal, Full, Rem) :-
    findall(J, prop(J, type, job), List),
    filterOut(location, Loc, List, L),
    filterOut(industry, Ind, L, I),
    filterOut(position, Pos, I, P),
    filterOut(salary, Sal, P, S),
    filterOut(isFulltime, Full, S, F),
    filterOut(isRemote, Rem, F, R),
    write('\n'),
    checkEmpty(R),
    write('\n'),
    write('\n'),
    ask().

% if x given, then ignore filter
filterOut(_, 'x', List, List).
filterOut(_, _, [], []).

% special case for salary, where the desired salary can be less than the one offered
filterOut(salary, Val, [H|T], [H|R]) :-
    prop(H, salary, S),
    Val =< S, 
    filterOut(salary, Val, T, R).

filterOut(Prop, Val, [H|T], [H|R]) :-
    prop(H, Prop, Val),
    filterOut(Prop, Val, T, R).

filterOut(Prop, Val, [H|T], R) :-
    not(prop(H, Prop, Val)),
    filterOut(Prop, Val, T, R).

checkEmpty([]):-
    write('\n No Jobs Matching Criteria \n').

checkEmpty(List):-
    showJobList(List).

% List of JobIds
showJobList([]).
showJobList([H|T]):-
    showJob(H),
    showJobList(T).

showJob(JobId):-
    findLocation(JobId, Loc),
    findIndustry(JobId, Ind),
    findDeadline(JobId, Dea),
    findPosition(JobId, Pos),
    findSalary(JobId, Sal),
    findIsFulltime(JobId, Ful),
    findIsRemote(JobId, Rem),
    findLanguages(JobId, LL),
    findPrograms(JobId, PL),
    findEducation(JobId, ED),
    findExperience(JobId, EL),
    write('\n   Job ID#:    '),
    write(JobId),
    write('\n'),
    printLocation(Loc),
    printIndustry(Ind),
    printPosition(Pos),
    printSalary(Sal),
    printIsFulltime(Ful),
    printIsRemote(Rem),
    printDeadline(Dea),
    write('\n'),
    write('-> Languages   '), write('Qualifications \n'),
    printLanguages(LL),
    write('-> Programs    '), write('Qualifications \n'),
    printPrograms(PL),
    write('-> Education   '), write('Qualification  \n'),
    printEducation(ED),
    write('-> Experience  '), write('Qualifications \n'),
    printExperience(EL).

% ---------------------------
% Query 2: Resume Creation
% ---------------------------
checkUserQ2(Username) :-
    findall(U, prop(U, type, applicant), Users),
    member(Username, Users),
    write('\n User Already Exists \n'),
    ask().

checkUserQ2(Username) :-
    findall(U, prop(U, type, applicant), Users),
    not(member(Username, Users)),
    creation(Username).  

creation(Username) :-
  write('\n What language do you speak? \n'),
  readTerminal(Language),
  write('\n What is a computer program you are most familiar with? \n'),
  readTerminal(Program),
  write('\n What is your highest level of education? \n'),
  readTerminal(Education),
  write('\n What programming language do you know best? \n'),
  readTerminal(PLanguage),
  write('\n How many years of experience do you have coding with it? \n'),
  readTerminal(Years),
  assertz(prop(Username, type, applicant)),
  assertz(prop(Username, language, Language)),
  assertz(prop(Username, programs, Program)),
  assertz(prop(Username, education, Education)),
  assertz(prop(Username, experience, (PLanguage, Years))),
  write('\n Resume created! \n'),
  ask().


% ---------------------------
% Query 3: Resume Updating
% ---------------------------

checkUserQ3(Username) :-
    findall(U, prop(U, type, applicant), Users),
    not(member(Username, Users)),
    write('\n User Not Found \n'),
    ask().

checkUserQ3(Username) :-
    findall(U, prop(U, type, applicant), Users),
    member(Username, Users),
    update(Username).  

update(Username) :-
  write('\n What would you like to add to your resume?: \n
        1. A spoken language \n 
        2. A computer program you are familar with \n
        3. A programming language you know \n'),
  readTerminal(Choice),
  updatePrompt(Username, Choice). 

% Helper to update a resume, adds a spoken language
updatePrompt(Username, Choice) :-
   Choice = '1', 
   write('\n What language would you like to add? \n'), 
   readTerminal(Language),
   assertz(prop(Username, language, Language)),
   write('\n Updated! \n'),
   ask().

% Helper to update a resume, adds a computer program
updatePrompt(Username, Choice) :-
   Choice = '2', 
   write('\n What program would you like to add? \n'), 
   readTerminal(Program),
   assertz(prop(Username, programs, Program)),
   write('\n Updated! \n'),
   ask().

% Helper to update a resume, adds a programming language
updatePrompt(Username, Choice) :-
   Choice = '3', 
   write('\n What programming language would you like to add? \n'), 
   readTerminal(PLanguage),
   write('\n How many years of experience do you have coding with it? \n'),
   readTerminal(Years),
   findall(V, prop(Username, experience, (PLanguage, V)), Vals),
   removeExperiences(Username, PLanguage, Vals),
   assertz(prop(Username, experience, (PLanguage, Years))),
   write('\n Updated! \n'),
   ask().

updatePrompt(Username, _) :-
    write('\n Invalid Input \n'),
    update(Username).

removeExperiences(_, _, []).
removeExperiences(Username, PLanguage, [H|T]) :-
    retract(prop(Username, experience, (PLanguage, H))),
    removeExperiences(Username, PLanguage, T).

% ---------------------------
% Query 4: Resume Showing
% ---------------------------

checkUserQ4(Username) :-
    findall(U, prop(U, type, applicant), Users),
    not(member(Username, Users)),
    write('\n User Not Found \n'),
    ask().

checkUserQ4(Username) :-
    findall(U, prop(U, type, applicant), Users),
    member(Username, Users),
    show(Username). 

show(Username) :-
  findQualifications(Username, LangList, ProgList, EdList, ExpList),
  write('\n Your languages: \n'),
  printList(LangList),
  write('\n Your known computer programs: \n'), 
  printList(ProgList),
  write('\n Your education: \n'), 
  write('     - '),
  write(EdList),
  write('\n'),
  write('\n Your programming languages and years of experience in each: \n'), 
  printList(ExpList),
  nl,
  ask().

% ---------------------------
% Query 5: Resume Measure
% ---------------------------

checkUserQ5(Username) :-
    findall(U, prop(U, type, applicant), Users),
    not(member(Username, Users)),
    write('\n User Not Found \n'),
    ask().

checkUserQ5(Username) :-
    findall(U, prop(U, type, applicant), Users),
    member(Username, Users),
    measure(Username). 

measure(Username) :-
  findall(J, prop(J, type, job), JList), %find all of the jobs
  measureJobList(Username, JList),
  nl,
  ask().

% ---------------------------
% Query 6: Save Resume to File
% ---------------------------

checkUserQ6(Username) :-
    findall(U, prop(U, type, applicant), Users),
    not(member(Username, Users)),
    write('\n User Not Found \n'),
    ask().

checkUserQ6(Username) :-
    findall(U, prop(U, type, applicant), Users),
    member(Username, Users),
    saveToFile(Username). 

saveToFile(Username) :-
  write('\n What do you want the file name to be? (Add .txt extension) \n'),
  readTerminal(FileName),
  open(FileName,write,Out),
  writeResume(Out, Username),
  close(Out),
  write('\n Resume saved to file named: '),
  write(FileName),
  nl,
  ask().

writeResume(Out, Username) :-
  write(Out, 'Resume for '),
  write(Out, Username),
  nl,
  findQualifications(Username, LangList, ProgList, EdList, ExpList),
  write(Out, '\n Your languages: \n'),
  writeList(Out, LangList),
  write(Out, '\n Your known computer programs: \n'), 
  writeList(Out, ProgList),
  write(Out, '\n Your education: \n'), 
  write(Out, '     - '),
  write(Out, EdList),
  write(Out, '\n'),
  write(Out, '\n Your programming languages and years of experience in each: \n'), 
  writeList(Out, ExpList).

% ========================
% Utility Functions
% ========================


listToSet([], []).
listToSet([H|T], X) :-
    member(H, T),
    listToSet(T, X).

listToSet([H|T], [H|X]) :-
    not(member(H, T)),
    listToSet(T, X).


% ========================
% Getter Functions
% ========================

% Helper to find all qualifications
findQualifications(ID, LangSet, ProgSet, Edu, ExpSet) :-
    findSetLanguages(ID, LangSet),
    findSetPrograms(ID, ProgSet),
    findEducation(ID, Edu),
    findSetExperience(ID, ExpSet).


% Filter Getters
findLocation(ID, V) :-
    prop(ID, location, V).

findIndustry(ID, V) :-
    prop(ID, industry, V).

findDeadline(ID, V) :-
    prop(ID, deadline, V).

findPosition(ID, V) :-
    prop(ID, position, V).

findSalary(ID, V) :-
    prop(ID, salary, V).

findIsFulltime(ID, V) :-
    prop(ID, isFulltime, V).   

findIsRemote(ID, V) :-
    prop(ID, isRemote, V).                 

% Scoring Getters
findLanguages(ID, LL) :-
    findall(V, prop(ID, language, V), LL).

findPrograms(ID, PL) :-
    findall(V, prop(ID, programs, V), PL). 

findEducation(ID, ED) :-
    prop(ID, education, ED).

findExperience(ID, EL) :-
    findall(V, prop(ID, experience, V), EL).
    
% Set Scoring Getters 
findSetLanguages(ID, Set) :-
    findLanguages(ID, List),
    listToSet(List, Set).

findSetPrograms(ID, Set) :-
    findPrograms(ID, List),
    listToSet(List, Set).

findSetExperience(ID, Set) :-
    findExperience(ID, List),
    listToSet(List, Set).            

% ========================
% Printer Functions
% ========================

% Generic for printing lists
% Base case, empty list
printList([]).
% Case where elements are in pairs (e.g. programming language and years of experience)
printList([(X, Y)|T]) :-
    write('     - '),
    write(X),
    write(', '),
    write(Y),
    write('\n'),
    printList(T).
% Case where elements are not in pairs
printList([H|T]) :-
    write('     - '),
    write(H),
    write('\n'),
    printList(T).

% Generic for writing lists
% Base case, empty list
writeList(_, []).
% Case where elements are in pairs (e.g. programming language and years of experience)
writeList(Out, [(X, Y)|T]) :-
    write(Out, '     - '),
    write(Out, X),
    write(Out, ', '),
    write(Out, Y),
    write(Out, '\n'),
    writeList(Out, T).
% Case where elements are not in pairs
writeList(Out, [H|T]) :-
    write(Out, '     - '),
    write(Out, H),
    write(Out, '\n'),
    writeList(Out, T).


% Print filter properties
printLocation(V) :-
    write('-> Location    '),
    write(V),
    write('\n').

printIndustry(V) :-
    write('-> Industry    '),
    write(V),
    write('\n'). 

printDeadline(V) :-
    write('-> Deadline    '),
    write(V),
    write('\n'). 

printPosition(V) :-
    write('-> Position    '),
    write(V),
    write('\n').  

printSalary(V) :-
    write('-> Salary      $'),
    write(V),
    write('/hr'),
    write('\n').  

printIsFulltime(0) :-
    write('-> Fulltime?   '),
    write('No'),
    write('\n').  
printIsFulltime(1) :-
    write('-> Fulltime?   '),
    write('Yes'),
    write('\n').   

printIsRemote(0) :-
    write('-> Remote?     '),
    write('No'),
    write('\n').  
printIsRemote(1) :-
    write('-> Remote?     '),
    write('Yes'),
    write('\n').    

% Print Scoring properties
% - Languages
% - Programs
% - Education 
% - Experience(s) 
printLanguages([]).
printLanguages([H|T]) :-
    write('               '), write('- '),
    write(H),
    write('\n'),
    printLanguages(T).

printPrograms([]).
printPrograms([H|T]) :-
    write('               '), write('- '),
    write(H),
    write('\n'),
    printPrograms(T).  

printEducation(ED) :-
    write('               '), write('- '),
    write(ED),
    write('\n').

printExperience([]).
printExperience([(PL,YOE)|T]) :-
    write('               '), write('- '),
    write(PL),
    write(', '),
    write(YOE),
    write(' Years'),
    write('\n'),
    printExperience(T). 


% ========================
% Resume Encoding
% ========================
% prop(person, property, value)
% type       is applicant
prop('Bob', type, applicant).
prop('Bob', language, 'English').
prop('Bob', programs, 'Microsoft Excel').
prop('Bob', programs, 'AWS').
prop('Bob', programs, 'MATLAB').
prop('Bob', education, 'Undergraduate').
prop('Bob', experience, ('C', '4')).
prop('Bob', experience, ('Python', '4')).


% prop(person, property, value)
prop('Alice', type, applicant).
prop('Alice', language, 'Chinese').
prop('Alice', programs, 'Microsoft Office').
prop('Alice', education, 'High School').
prop('Alice', experience, ('Python', '2')).

prop('David Poole', type, applicant).
prop('David Poole', language, 'English').
prop('David Poole', programs, 'Microsoft Excel').
prop('David Poole', programs, 'AWS').
prop('David Poole', programs, 'MATLAB').
prop('David Poole', education, 'Masters').
prop('David Poole', experience, ('Haskell', '100')).
prop('David Poole', experience, ('Prolog', '100')).
prop('David Poole', experience, ('C', '4')).
prop('David Poole', experience, ('Python', '4')).

% ========================
% Job Encoding
% ========================
% prop(jobId, property, value)
% JobId is 'XXX' 

% Filter Properties
% A posting has only one of each property
% type       is job
% location   is 'Location'
% industry   is 'Industry'
% deadline   is 'MM/DD/YY'
% position   is 'Position'
% salary     is 'Int', Represents $ per hour
% isFulltime is one of [1,0]
% isRemote   is one of [1,0]

prop(001, type, job).
prop(001, location, 'Vancouver').
prop(001, industry, 'Computer Science').
prop(001, deadline, 01/31/2022).
prop(001, position, 'Software Engineer').
prop(001, salary, 40).
prop(001, isFulltime, 1).
prop(001, isRemote, 1).

prop(002, type, job).
prop(002, location, 'Toronto').
prop(002, industry, 'Banking').
prop(002, deadline, 01/31/2022).
prop(002, position, 'Junior Quantitative Researcher').
prop(002, salary, 100).
prop(002, isFulltime, 1).
prop(002, isRemote, 1).

prop(003, type, job).
prop(003, location, 'Paris').
prop(003, industry, 'Marketing').
prop(003, deadline, 01/31/2022).
prop(003, position, 'SEO Engineer').
prop(003, salary, 40).
prop(003, isFulltime, 0).
prop(003, isRemote, 1).

prop(004, type, job).
prop(004, location, 'Toronto').
prop(004, industry, 'Customer Service').
prop(004, deadline, 01/31/2022).
prop(004, position, 'Junior Network Engineer').
prop(004, salary, 20).
prop(004, isFulltime, 1).
prop(004, isRemote, 1).

prop(005, type, job).
prop(005, location, 'Vancouver').
prop(005, industry, 'Health Care').
prop(005, deadline, 01/31/2022).
prop(005, position, 'Senior Biomedical Engineer').
prop(005, salary, 40).
prop(005, isFulltime, 1).
prop(005, isRemote, 1).

% Scoring Properties
% Aside from education, a posting may have multiple of the property
% language   is 'Language'
% programs   is 'NameOfProgram'
% education  is one of ['HighSchool', 'Undergraduate', 'Masters']
% experience is ('Programming Language', YearsOfExperience)

prop(001, language, 'English').
prop(001, programs, 'Microsoft Excel').
prop(001, education, 'Undergraduate').
prop(001, experience, ('C', '4')).
prop(001, experience, ('Python', '5')).

prop(002, language, 'English').
prop(002, programs, 'MATLAB').
prop(002, education, 'Masters').
prop(002, experience, ('Python', '4')).

prop(003, language, 'French').
prop(003, programs, 'Microsoft Excel').
prop(003, education, 'Undergraduate').
prop(003, experience, ('Python', '2')).

prop(004, language, 'English').
prop(004, programs, 'AWS').
prop(004, education, 'Undergraduate').
prop(004, experience, ('Perl', '1')).

prop(005, language, 'English').
prop(005, programs, 'Microsoft Excel').
prop(005, education, 'Masters').
prop(005, experience, ('Python', '5')).
