agent:-
    write('insert a sentence to convert- '),
    perceive(Percepts),
    action(Percepts).

perceive(Percepts):-
    read(Percepts).

action(Input):-
    sentence(Input, Output),
    ruleFormat(Output, NP21, Verb, NP22),
    rule(NP21, Verb, NP22, Command),
    Command.

synonym(Synonyms):-
    write('instert a sentence tp convert- '),
    perceive(Percepts),
    input(Percepts, Percepts, Synonyms, Fixed),
    action(Fixed).

% loop through inputs are valid words
input([X|XS], All, Synonyms, Output):-
    thisW(X),
    input(XS, All, Synonyms, Output).

/* if Y is a word, it is the synonym for X, so replace X with
y in the output */
input([X|_], All, [Y|_], Output):-
    thisW(Y),
    replace(X, Y, All, Output).

% Y isn't a word,  try the other synonyms
input([X|XS], All, [_|YS], Output):-
    input([X|XS], All, YS, Output).


replace(_, _, [], []).
replace(X, Y, [X|XS], [Y|YS]) :- replace(X, Y, XS, YS).
replace(X, Y, [C|XS], [C|YS]) :- dif(C, X), replace(X, Y, XS, YS).

% must be in the dictionary
thisW(X):-
    noun(X);
    prep(X);
    det(X);
    verb(X);
    adj(X).

/* this need for rule out from sentence match all the inputs we handle */
ruleFormat(sentence(np(_, NP21), vp(verb(Verb), np(_, NP22))),
           NP21,
           Verb,
           NP22).

ruleFormat(sentence(np(_, NP21), vp(verb(Verb), pp(_, NP22))),
           NP21,
           Verb,
           NP22).

ruleFormat(sentence(np(_, NP21), vp(verb(Verb), pp(_, np(_, NP22)))),
           NP21,
           Verb,
           NP22).

rule(np2(adj(very), np2(adj(short), np2(noun(command)))),
     listing,
     np2(adj(current), np2(noun(directory))),
     write('ls')).

rule(np2(adj(current), np2(noun(directory))),
     viewed,
     np2(adj(more), np2(adj(fine), np2(noun(detail)))),
     write('ls -la')).

rule(np2(noun(command)),
     moving,
     np2(adj(higher), np2(noun(directory))),
     write('cd ..')).

rule(np2(noun(command)),
     moves,
     np2(noun(parent)),
     write('cd ..')).

rule(np2(noun(command)),
     prints,
     np2(adj(current), np2(noun(directory))),
     write('pwd')).

rule(np2(noun(command)),
     types,
     np2(noun(file)),
     write('cat 08226.txt')).

% S -> NP VP
sentence(Sentence, sentence(Noun_Phrase, Verb_Phrase)):-
    np(Sentence, Noun_Phrase, Rem),
    vp(Rem, Verb_Phrase).

% S -> VP
sentence(Sentence, sentence(Verb_Phrase)):-
    vp(Sentence, Verb_Phrase).

% NP -> Det NP2
np([X|XS], np(det(X), NP2), Rem):-
    det(X),
    np2(XS, NP2, Rem).

% NP -> NP2
np(X, Parse, Rem):-
    np2(X, Parse, Rem).

% NP2 -> Noun
np2([X|XS], np2(noun(X)), XS):-
    noun(X).

% NP2 -> Adj NP2
np2([X|XS], np2(adj(X), NP2), Rem):-
    adj(X),
    np2(XS, NP2, Rem).

% PP -> Prep NP
pp([X|XS], pp(prep(X), NP), Rem):-
    prep(X),
    np(XS, NP, Rem).

% VP -> Verb
vp([X|XS], vp(verb(X)), XS):-
    verb(X).

% VP -> Verb PP
vp([X|XS], vp(verb(X), PP)):-
    verb(X),
    pp(XS, PP, _).

% VP -> Verb NP
vp([X|XS], vp(verb(X), NP)):-
    verb(X),
    np(XS, NP, _).



% VP -> Verb Adverb NP
vp([X,Y|XS], vp((verb(X), adverb(Y)), NP)):-
    verb(X),
    adverb(Y),
    np(XS, NP, _).

% VP -> Verb Adverb
vp([X,Y], vp(verb(X), adverb(Y))):-
    verb(X),
    adverb(Y).

% dictionary
noun('command').
noun('directory').
noun('file').
noun('detail').
noun('08226txt').
noun('parent').
prep('and').
prep('in').
prep('to').
det('a').
det('the').
verb('listing').
verb('prints').
verb('types').
verb('moves').
verb('moving').
verb('viewed').
adj('higher').
adj('very').
adj('more').
adj('fine').
adj('short').
adj('current').
