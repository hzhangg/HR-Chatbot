% Start of our Project
:- dynamic prop/3.

ask() :-
    write("Welcome to the HR Bot"), 
	write('\n I am a personal assistant bot.'),
	write('\n Select an option: \n
        1. Look for a Job Posting \n
        2. Create your Resume \n
        3. Update your Resume \n
        4. Show your Resume \n 
        5. Measure your Resume \n'),
    read(Input),
    query(Input).

% ask() -> query() -> filterFunction() -> ask()

% ========================
% Options
% ========================

% Filter jobs to find one suited for a user based on their input 
query(Input) :-
  Input = 1,
  write('\n What location are you looking for? \n'),
  read(Location),
  write('\n What industry are you looking to work in? \n'),
  read(Industry),
  write('\n When can you apply by? In the form MM/DD/YY \n'),
  read(Deadline),
  write('\n What job position are you looking for? \n'),
  read(Position),
  write('\n What salary do you desire? (dollars per hour) \n'),
  read(Salary),
  write('\n Are you looking for full time work? Enter \'1\' if so, \'0\' if not \n'),
  read(IsFullTime),
  write('\n Are you looking for remote work? Enter \'1\' if so, \'0\' if not \n'),
  read(IsRemote),
  jobFilter(Location, Industry, Deadline, Position, Salary, IsFullTime, IsRemote).

% Create a users resume
query(Input) :-
  Input = 2, 
  write('\n What is your name? \n'),
  read(Username),
  write('\n What language do you speak? \n'),
  read(Language),
  write('\n What is a computer program you are most familiar with? \n'),
  read(Program),
  write('\n What is your highest level of education? \n'),
  read(Education),
  write('\n What programming language do you know best? \n'),
  read(PLanguage),
  write('\n How many years of experience do you have coding with it? \n'),
  read(Years),
  assertz(prop(Username, type, applicant)),
  assertz(prop(Username, language, Language)),
  assertz(prop(Username, programs, Program)),
  assertz(prop(Username, education, Education)),
  assertz(prop(Username, experience, (PLanguage, Years))),
  write('\n Resume created! \n'),
  ask().

% Update a users resume
query(Input) :-
  Input = 3,
  write('\n What is your name? \n'),
  read(Username),
  write('\n What would you like to add to your resume?: \n
        1. A spoken language \n 
        2. A computer program you are familar with \n
        3. A programming language you know \n'),
  read(Choice),
  updatePrompt(Username, Choice). 

% Prints a users resume
query(Input) :- 
  Input = 4,
  write('\n What is your name? \n'),
  read(Username),
  findLanguages(Username, LangList),
  write('\n Your languages: \n'),
  write(LangList),
  findPrograms(Username, ProgList),
  write('\n Your known computer programs: \n'), 
  write(ProgList),
  findEducation(Username, EdList),
  write('\n Your education: \n'), 
  write(EdList),
  findExperience(Username, ExpList),
  write('\n Your programming languages and years of experience in each: \n'), 
  write(ExpList),
  ask().

% Measures a users resume
query(Input) :- % todo !!!
  Input = 5,
  % something like: what is your name -> find user qualifications -> find all job scoring properties -> count how many of these the user has
  % print: jobs user is qualified for? skills user is missing that are in our KB? percentage of properies user has out of all properties?
  % just spitballing here
  write('\n What is your name? \n'),
  read(Username),
  findall(P, prop(Username, Q, V), List), % find all of the users qualifications
  nl.
  

% ========================
% Helper Functions
% ========================

jobFilter(Loc, Ind, Dead, Pos, Sal, Full, Rem) :-
    findall(J, prop(J, type, job), List),
    filterOut(location, Loc, List, L),
    filterOut(industry, Ind, L, I),
    filterOut(deadline, Dead, I, D),
    filterOut(position, Pos, D, P),
    filterOut(salary, Sal, P, S),
    filterOut(isFulltime, Full, S, F),
    filterOut(isRemote, Rem, F, R),
    write('Job Ids: '),
    write(R), % debugging
    showJobList(R),
    write('\n'),
    write('\n'),
    ask().

% if x given, then ignore filter
filterOut(_, x, List, List).
filterOut(_, _, [], []).
filterOut(Prop, Val, [H|T], [H|R]) :-
    prop(H, Prop, Val),
    filterOut(Prop, Val, T, R).

filterOut(Prop, Val, [H|T], R) :-
    not(prop(H, Prop, Val)),
    filterOut(Prop, Val, T, R).

showJobList([]).
showJobList([H|T]):-
    showJob(H),
    showJobList(T).

showJob(JobId):-
    findall((P,V), prop(JobId, P, V), List),
    write('\n Job Posting : '),
    write(JobId),
    write('\n'),
    printPropVal(List).

printPropVal([]).
printPropVal([(P,V)|T]):-
    member(P, [ location, industry, 
                deadline, position, 
                salary, isFulltime, isRemote]),
    write('-> '),
    write(P),
    write('  '),
    write(V),
    write('\n'),
    printPropVal(T).

printPropVal([(P,V)|T]):-
    not(member(P, [ location, industry, 
                deadline, position, 
                salary, isFulltime, isRemote])),
    printPropVal(T). 

% Helper to update a resume, adds a spoken language
updatePrompt(Username, Choice) :-
   Choice = 1, 
   write('\n What language would you like to add? \n'), 
   read(Language),
   assertz(prop(Username, language, Language)),
   write('\n Thank you! \n'),
   ask().

% Helper to update a resume, adds a computer program
updatePrompt(Username, Choice) :-
   Choice = 2, 
   write('\n What program would you like to add? \n'), 
   read(Program),
   assertz(prop(Username, programs, Program)),
   write('\n Thank you! \n'),
   ask().

% Helper to update a resume, adds a programming language
updatePrompt(Username, Choice) :-
   Choice = 3, 
   write('\n What programming language would you like to add? \n'), 
   read(PLanguage),
   write('\n How many years of experience do you have coding with it? \n'),
   read(Years),
   assertz(prop(Username, experience, (PLanguage, Years))),
   write('\n Thank you! \n'),
   ask().

findLanguages(ID, LL) :-
    findall(V, prop(ID, languages, V), LL).

findPrograms(ID, PL) :-
    findall(V, prop(ID, programs, V), PL). 

findEducation(ID, ED) :-
    prop(ID, education, ED).

findExperience(ID, EL) :-
    findall(V, prop(ID, experience, V), EL).

% ========================
% Resume Encoding
% ========================
% prop(person, property, value)
% type       is applicant
prop("Bob", type, applicant).
prop("Bob", language, "English").
prop("Bob", programs, "Microsoft Excel").
prop("Bob", programs, "AWS").
prop("Bob", programs, "MATLAB").
prop("Bob", education, "Undergraduate").
prop("Bob", experience, ("C", 4)).
prop("Bob", experience, ("Python", 4)).


% prop(person, property, value)
prop("Alice", type, applicant).
prop("Alice", language, "Chinese").
prop("Alice", programs, "Microsoft Office").
prop("Alice", education, "High School").
prop("Alice", experience, ("Python", 2)).

% ========================
% Job Encoding
% ========================
% prop(jobId, property, value)
% JobId is "XXX" 

% Filter Properties
% A posting has only one of each property
% type       is job
% location   is "Location"
% industry   is "Industry"
% deadline   is "MM/DD/YY"
% position   is "Position"
% salary     is "Int", Represents $ per hour
% isFulltime is one of [1,0]
% isRemote   is one of [1,0]

prop(001, type, job).
prop(001, location, "Vancouver").
prop(001, industry, "Computer Science").
prop(001, deadline, "01/31/2022").
prop(001, position, "Software Engineer").
prop(001, salary, 40).
prop(001, isFulltime, 1).
prop(001, isRemote, 1).

prop(002, type, job).
prop(002, location, "Toronto").
prop(002, industry, "Banking").
prop(002, deadline, "01/31/2022").
prop(002, position, "Junior Quantitative Researcher").
prop(002, salary, 100).
prop(002, isFulltime, 1).
prop(002, isRemote, 1).

prop(003, type, job).
prop(003, location, "Paris").
prop(003, industry, "Marketing").
prop(003, deadline, "01/31/2022").
prop(003, position, "SEO Engineer").
prop(003, salary, 40).
prop(003, isFulltime, 0).
prop(003, isRemote, 1).

prop(004, type, job).
prop(004, location, "Toronto").
prop(004, industry, "Customer Service").
prop(004, deadline, "01/31/2022").
prop(004, position, "Junior Network Engineer").
prop(004, salary, 20).
prop(004, isFulltime, 1).
prop(004, isRemote, 1).

prop(005, type, job).
prop(005, location, "Vancouver").
prop(005, industry, "Health Care").
prop(005, deadline, "01/31/2022").
prop(005, position, "Senior Biomedical Engineer").
prop(005, salary, 40).
prop(005, isFulltime, 1).
prop(005, isRemote, 1).

% Scoring Properties
% Aside from education, a posting may have multiple of the property
% language   is "Language"
% programs   is "NameOfProgram"
% education  is one of ["HighSchool", "Undergraduate", "Masters"]
% experience is ("Programming Language", YearsOfExperience)

prop(001, language, "English").
prop(001, programs, "Microsoft Excel").
prop(001, education, "Undergraduate").
prop(001, experience, ("C", 4)).

prop(002, language, "English").
prop(002, programs, "MATLAB").
prop(002, education, "Masters").
prop(002, experience, ("Python", 4)).

prop(003, language, "French").
prop(003, programs, "Microsoft Excel").
prop(003, education, "Undergraduate").
prop(003, experience, ("Python", 2)).

prop(004, language, "English").
prop(004, programs, "AWS").
prop(004, education, "Undergraduate").
prop(004, experience, ("Perl", 1)).

prop(005, language, "English").
prop(005, programs, "Microsoft Excel").
prop(005, education, "Masters").
prop(005, experience, ("Python", 5)).