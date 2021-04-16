% Start of our Project
:- dynamic prop/3.
:- use_module(api).

% Call to run the program
main() :-
    printASCII(),
    write('\n I am a personal assistant bot. \n'),
    ask().

% Main menu, asks the user what they would like to do
ask() :-
    flush_output(current_output),
	write('\n Select an option: \n
        1. Search for a Job Posting \n
        2. Search for a Definition \n
        3. Create   Resume \n
        4. Update   Resume \n
        5. View     Resume \n 
        6. Measure  Resume \n
        7. Evaluate Resume \n
        8. Save     Resume \n
        9. Quit \n'),
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

% Searches for a word definition
query(Input) :-
  Input = '2', 
  write('\n What word are you curious about? \n'),
  readTerminal(Word),
  define(Word, Definition), % todo !!!
  nl, write('The definition of '), write(Word), write(' is: '),
  nl, write(Definition), nl,
  ask().

% Create a users resume
query(Input) :-
  Input = '3', 
  write('\n What is your name? \n'),
  readTerminal(Username),
  checkUserQ3(Username).

% Update a users resume
query(Input) :-
  Input = '4',
  write('\n What is your name? \n'),
  readTerminal(Username),
  checkUserQ4(Username).

% Prints a users resume
query(Input) :- 
  Input = '5',
  write('\n What is your name? \n'),
  readTerminal(Username),
  print(Username),
  checkUserQ5(Username).

% Measures a users resume
query(Input) :- 
  Input = '6',
  write('\n What is your name? \n'),
  readTerminal(Username),
  checkUserQ6(Username).

% Diffs users resume against all jobs 
query(Input) :-
  Input = '7',
  write('\n What is your name? \n'),
  readTerminal(Username),
  checkUserQ7(Username).

% Saves a users resume to a new file
query(Input) :- 
  Input = '8',
  write('\n What is your name? \n'),
  readTerminal(Username),
  checkUserQ8(Username).

% Quits program
query(Input) :- 
  Input = '9',
  halt(0).

query(Input) :-
    not(member(Input, ['1', '2', '3', '4', '5', '6', '7', '8', '9'])),
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


% ===================
% Recommend Functions
% ===================

% We can use for
% - Languages
% - Programs 
addAbsentLangProg([], _, D, D).
% case where qualified, dont add to Dict 
addAbsentLangProg([H|T], UserList, TempDict, ResDict) :- 
    member(H, UserList), 
    addAbsentLangProg(T, UserList, TempDict, ResDict).

% case where unqualified, add to Dict   
addAbsentLangProg([H|T], UserList, TempDict, ResDict) :-
    not(member(H, UserList)), 
    incrementKey(H, TempDict, ModDict),
    addAbsentLangProg(T, UserList, ModDict, ResDict).

% We can use for
% - Education 
% ['HighSchool', 'Undergraduate', 'Masters']
% case where qualified, dont add to Dict 
addAbsentEdu(_, 'Masters', D, D).
addAbsentEdu(JEdu, 'Undergraduate', D, D) :- 
    member(JEdu, ['HighSchool', 'Undergraduate']).
addAbsentEdu(JEdu, 'HighSchool', D, D) :- 
    member(JEdu, ['HighSchool']).

% case where unqualified, add to Dict 
addAbsentEdu(JEdu, UserEdu, TempDict, ModDict) :-
    dif(JEdu, UserEdu),
    incrementKey(JEdu, TempDict, ModDict). 

% We can use for
% - Experience 
addAbsentExperience([], _, D, D).
% case where qualified, dont add to Dict 
addAbsentExperience([(P, E)|T], UserList, TempDict, ResDict) :- 
    member((P, X), UserList), 
    atom_number(X, XN),
    atom_number(E, EN),
    EN =< XN, 
    addAbsentExperience(T, UserList, TempDict, ResDict).
    
% case where unqualified - lacks experience - , add to Dict 
addAbsentExperience([(P, E)|T], UserList, TempDict, ResDict) :-
    member((P, X), UserList),
    atom_number(X, XN),
    atom_number(E, EN),
    EN > XN,
    incrementKey((P,E), TempDict, ModDict),
    addAbsentExperience(T, UserList, ModDict, ResDict).

% case where unqualified - lacks lang, exp - , add to Dict 
addAbsentExperience([(P, E)|T], UserList, TempDict, ResDict) :-
    not(member((P, E), UserList)),
    incrementKey((P,E), TempDict, ModDict),
    addAbsentExperience(T, UserList, ModDict, ResDict).


% Dictionary Helpers
% - modifies the key val by + 1 or creates the key,val pair
incrementKey(Key, Dict, ModDict) :-
    member((Key, _), Dict),
    modifyDict(Key, Dict, ModDict).

incrementKey(Key, Dict, [(Key, 1)| Dict]) :-
    not(member((Key, _), Dict)).


modifyDict(_, [], []).
modifyDict(Key, [(DKey, Val) | Dict], [(Key, NewVal) | Dict]) :-
    not(dif(Key, DKey)),
    NewVal is Val + 1.

modifyDict(Key, [(DKey, Val) | Dict], [(DKey, Val) | ModDict]) :-
    dif(Key, DKey),
    modifyDict(Key, Dict, ModDict).




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
    write('\n   Job ID#:    '), write(JobId), write('\n'),
    printLocation(Loc),
    printIndustry(Ind),
    printPosition(Pos),
    printSalary(Sal),
    printIsFulltime(Ful),
    printIsRemote(Rem),
    printDeadline(Dea),
    write('\n'),
    write('-  Languages   '), write('Qualifications \n'),
    printLanguages(LL),
    nl,
    write('-  Programs    '), write('Qualifications \n'),
    printPrograms(PL),
    nl,
    write('-  Education   '), write('Qualification  \n'),
    printEducation(ED),
    nl,
    write('-  Experience  '), write('Qualifications \n'),
    printExperience(EL),
    nl.

% ---------------------------
% Query 2: HTTP Definitons
% ---------------------------

define(Word, Definition) :-
    parseWord(Word, CleanWord),
    search(CleanWord, Definition).

parseWord(Input, Result) :-
    atomic_list_concat(Words, ' ', Input),
    atomic_list_concat(Words, '%20', Result). 

% ---------------------------
% Query 3: Resume Creation
% ---------------------------
checkUserQ3(Username) :-
    findall(U, prop(U, type, applicant), Users),
    member(Username, Users),
    write('\n User Already Exists \n'),
    ask().

checkUserQ3(Username) :-
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
% Query 4: Resume Updating
% ---------------------------

checkUserQ4(Username) :-
    findall(U, prop(U, type, applicant), Users),
    not(member(Username, Users)),
    write('\n User Not Found \n'),
    ask().

checkUserQ4(Username) :-
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
% Query 5: Resume Showing
% ---------------------------

checkUserQ5(Username) :-
    findall(U, prop(U, type, applicant), Users),
    not(member(Username, Users)),
    write('\n User Not Found \n'),
    ask().

checkUserQ5(Username) :-
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
% Query 6: Resume Measure
% ---------------------------

checkUserQ6(Username) :-
    findall(U, prop(U, type, applicant), Users),
    not(member(Username, Users)),
    write('\n User Not Found \n'),
    ask().

checkUserQ6(Username) :-
    findall(U, prop(U, type, applicant), Users),
    member(Username, Users),
    measure(Username). 

measure(Username) :-
  findall(J, prop(J, type, job), JList), %find all of the jobs
  measureJobList(Username, JList),
  nl,
  ask().

measureJobList(_, []).
measureJobList(Username, [J|T]) :-
    findQualifications(Username, ULangList, UProgList, UEd, UExpList), % Maybe move out
    findQualifications(J, JLangList, JProgList, JEd, JExpList),
    % 'Start measuring'
    checkWithin(ULangList, JLangList, LangScore),
    checkWithin(UProgList, JProgList, ProgScore),
    measureEducation(UEd, JEd, EduScore),
    measureExperience(UExpList, JExpList, ExpScore),
    length(JLangList, JLangTot),
    length(JProgList, JProgTot),
    length(JExpList, JExpTot),
    getTotalQualScore(J, TotalQ),
    TotalScore is LangScore + ProgScore + EduScore + ExpScore, 
    % 'Calculations'
    PercentLang is (LangScore / JLangTot) *100,
    PercentProg is (ProgScore / JProgTot) *100,
    PercentEdu is (EduScore / 1) *100,
    PercentExp is (ExpScore / JExpTot) *100,
    PercentQ is (TotalScore / TotalQ) * 100,
    printQualPercent(J,PercentLang, PercentProg, PercentEdu, PercentExp, PercentQ),
    measureJobList(Username, T).

% ---------------------------
% Query 7: Resume Recommendation
% ---------------------------
checkUserQ7(Username) :-
    findall(U, prop(U, type, applicant), Users),
    not(member(Username, Users)),
    write('\n User Not Found \n'),
    ask().

checkUserQ7(Username) :-
    findall(U, prop(U, type, applicant), Users),
    member(Username, Users),
    recommend(Username). 

recommend(Username) :-
  findall(J, prop(J, type, job), JList), 
  recommendJobList( Username, JList, 
                    [], [], [], [], 
                    NDLang, NDProg, NDEdu, NDExp),
  % do print stuff 
  sort(NDLang, SDLang),
  sort(NDProg, SDProg),
  sort(NDEdu, SDEdu),
  sort(NDExp, SDExp),
  write('\n ------------------------------ \n'),
  write('\n Personalized Qualification Summary \n'),
  write('   Shown below are your missing qualifications \n'),
  write('   along with the amount of jobs that desire it. \n'),
  printHeaderDict('Languages'),
  checkEmptyDict(SDLang),
  printKeyDict(SDLang),nl,
  printHeaderDict('Programs'),
  checkEmptyDict(SDProg),
  printKeyDict(SDProg),nl,
  printHeaderDict('Education'),
  checkEmptyDict(SDEdu),
  printKeyDict(SDEdu),nl,
  printHeaderDict('Experience'),
  checkEmptyDict(SDExp),
  printPairDict(SDExp),nl,
  write('\n ------------------------------ \n'),
  nl,
  ask(). 

recommendJobList(_, [], LA, PR, ED, EX, LA, PR, ED, EX).
recommendJobList(Username, [J|T], ODLang, ODProg, ODEdu, ODExp, NDLang, NDProg, NDEdu, NDExp) :-
    findQualifications(Username, ULangList, UProgList, UEdu, UExpList), 
    findQualifications(J, JLangList, JProgList, JEdu, JExpList),
    % Start Building a Diff Qual Dictionary 
    addAbsentLangProg(JLangList, ULangList, ODLang, MDLang),
    addAbsentLangProg(JProgList, UProgList, ODProg, MDProg),
    addAbsentEdu(JEdu, UEdu, ODEdu, MDEdu),
    addAbsentExperience(JExpList, UExpList, ODExp, MDExp),
    recommendJobList(Username, T, 
                     MDLang, MDProg, MDEdu, MDExp,
                     NDLang, NDProg, NDEdu, NDExp).

% ---------------------------
% Query 8: Save Resume to File
% ---------------------------

checkUserQ8(Username) :-
    findall(U, prop(U, type, applicant), Users),
    not(member(Username, Users)),
    write('\n User Not Found \n'),
    ask().

checkUserQ8(Username) :-
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
    write('-  Location    '),
    write(V),
    write('\n').

printIndustry(V) :-
    write('-  Industry    '),
    write(V),
    write('\n'). 

printDeadline(V) :-
    write('-  Deadline    '),
    write(V),
    write('\n'). 

printPosition(V) :-
    write('-  Position    '),
    write(V),
    write('\n').  

printSalary(V) :-
    write('-  Salary      $'),
    write(V),
    write('/hr'),
    write('\n').  

printIsFulltime(0) :-
    write('-  Fulltime?   '),
    write('No'),
    write('\n').  
printIsFulltime(1) :-
    write('-  Fulltime?   '),
    write('Yes'),
    write('\n').   

printIsRemote(0) :-
    write('-  Remote?     '),
    write('No'),
    write('\n').  
printIsRemote(1) :-
    write('-  Remote?     '),
    write('Yes'),
    write('\n').    

% Print Scoring properties
% - Languages
% - Programs
% - Education 
% - Experience(s) 
printLanguages([]).
printLanguages([H|T]) :-
    write('               '), write('-> '),
    write(H),
    write('\n'),
    printLanguages(T).

printPrograms([]).
printPrograms([H|T]) :-
    write('               '), write('-> '),
    write(H),
    write('\n'),
    printPrograms(T).  

printEducation(ED) :-
    write('               '), write('-> '),
    write(ED),
    write('\n').

printExperience([]).
printExperience([(PL,YOE)|T]) :-
    write('               '), write('-> '),
    write(PL),
    write(', '),
    write(YOE),
    write(' Years'),
    write('\n'),
    printExperience(T). 

printQualPercent(JobId, LangP, ProgP, EduP, ExpP, TotalP) :-
    write('\n Qualification Summary: Job ID# '),write(JobId),write('\n'),
    write('\n Languages  '), write(LangP),write('%'),
    write('\n Programs   '), write(ProgP),write('%'),
    write('\n Education  '), write(EduP),write('%'),
    write('\n Experience '), write(ExpP),write('%'),
    write('\n Overall:   '), write(TotalP),write('%\n').

% Print Dictionary Diff properties
% - Languages
% - Programs
% - Education 
% - Experience(s) 
checkEmptyDict([]) :-
    write("\n   <<CATAGORY SATISFIED>>").
checkEmptyDict(_).

printHeaderDict(Property) :-
    write('\n '), write(Property),
    write('\n   #Jobs    Qualification').

printKeyDict([]).
printKeyDict([(Key, Val)| T]) :- 
    write('\n   '), write(Val),
    write('        '), write(Key),
    printKeyDict(T).

printPairDict([]).
printPairDict([((PL, Ex), Val)|T]) :-
    write('\n   '), write(Val),
    write('        '), write(PL), write(' '), write(Ex), write(' Years'),
    printPairDict(T). 

% Print welcome message
printASCII():-
    nl, 
    write('8   8  8                                     ""8""       8   8 8"""8       8""""8               
8   8  8 eeee e     eeee eeeee eeeeeee eeee    8  eeeee  8   8 8   8       8    8   eeeee eeeee 
8e  8  8 8    8     8  8 8  88 8  8  8 8       8e 8  88  8eee8 8eee8e      8eeee8ee 8  88   8   
88  8  8 8eee 8e    8e   8   8 8e 8  8 8eee    88 8   8  88  8 88   8 eeee 88     8 8   8   8e  
88  8  8 88   88    88   8   8 88 8  8 88      88 8   8  88  8 88   8      88     8 8   8   88  
88ee8ee8 88ee 88eee 88e8 8eee8 88 8  8 88ee    88 8eee8  88  8 88   8      88eeeee8 8eee8   88  
                                                                                                '),
    nl.

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
