offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),
bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,
50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,
100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).

perm([],[]).
perm([X|Y],Z) :- perm(Y,W), remove2(X,Z,W).



remove2(X,[X|R],R).
remove2(X,[F|R],[F|S]) :-
X\=F,
remove2(X,R,S).

allSubsets([], []).
allSubsets([H|T], [H|T1]):- allSubsets(T, T1).
allSubsets([_|T], T1):- allSubsets(T, T1).

possibleSubset([],[]).

possibleSubset([H|T],R):- allSubsets([H|T],L),perm(L,R).
replace_all([],_,_,[]).
replace_all([X|T],X,Y,[Y|T2]) :- replace_all(T,X,Y,T2).
replace_all([H|T],X,Y,[H|T2]) :- H \= X, replace_all(T,X,Y,T2).


   

choosePreferences(L,R):-
                     allSubsets(L,R1),
                     member(activity(X),R1),
                     allSubsets(X,R2),
                     
                     replace_all(R1,activity(X),activity(R2),R).


choosePreferences(L,R):-
                     allSubsets(L,R),
                     \+ member(activity(_),R).


preferenceSatisfaction1helper([],L,_,S):-
              max_list(L,S).
    
preferenceSatisfaction1helper([H|T],L,C,S):- 
               customerPreferredActivity(C,H,S1),
               append(L,[S1],L1),
               preferenceSatisfaction1helper(T,L1,C,S). 
                                         

preferenceSatisfaction1(O, C , P , S):-
     
   O=offer(_,A1,_,_,_,_,_,_),    
   member(activity(X),P),
   allSubsets(A1,X),
   preferenceSatisfaction1helper(X,[],C,S).
       
   
preferenceSatisfaction1(_ , _ , P , 0):-
 \+ member(activity(_),P).    


preferenceSatisfaction2(O , C , P , S):-
    offerAccommodation(O,A1),
    member(accommodation(_),P),
    customerPreferredAccommodation(C,A1,S).

preferenceSatisfaction2(_ , _ , P , 0):-
  \+ member(accommodation([_]),P).


preferenceSatisfaction3(O , C , P , S):-
   offerMean(O,A1),
    member(means(_),P),
    customerPreferredMean(C,A1,S).
preferenceSatisfaction3(_ , _, P , 0):-
 \+ member(means([_]),P).

preferenceSatisfaction(O,C,P,S):-
    preferenceSatisfaction1(O,C,P,Sum1),!,
    preferenceSatisfaction2(O,C,P,Sum2),!,
    preferenceSatisfaction3(O,C,P,Sum3),!,
    S is Sum1 + Sum2 + Sum3.

overlapPeriod(period(P1S,P1E),period(P2S,P2E)):-
      dategreater(P2S,P1S),   
      dategreater(P2E,P1S),
      dategreater(P1E,P2S),   
      dategreater(P1E,P2E).
       
overlapPeriod(period(P1S,P1E),period(P2S,P2E)):-
    dategreater(P2S,P1S),   
    dategreater(P1E,P2S),
    dategreater(P2E,P1S),   
    dategreater(P2E,P1E).
 
overlapPeriod(period(P1S,P1E),period(P2S,P2E)):-
     dategreater(P1S,P2S),   
     dategreater(P1E,P2S),
     dategreater(P2E,P1S),   
     dategreater(P1E,P2E).

overlapPeriod(period(P1S,P1E),period(P2S,P2E)):-
     dategreater(P1S,P2S),   
     dategreater(P1E,P2S),
     dategreater(P2E,P1S),   
     dategreater(P2E,P1E).
dategreater(Y1-M1-D1,Y2-M2-D2):-
         Date1 is Y1*10000+M1*100+D1,
         Date2 is Y2*10000+M2*100+D2,
         Date1 >=Date2.

getAccommodation([H|T],Y) :- (H = accommodation(X), Y = X) ; (getAccommodation(T,Y)).
getActivity([H|T],Y) :- (H = activity(X), Y = X) ; (getActivity(T,Y)).
getDestination([H|T],Y) :- (H = dest(X), Y = X) ; (getDestination(T,Y)).
getPeriod([H|T],Y) :- (H = period(X), Y = X) ; (getPeriod(T,Y)).
getBudget([H|T],Y) :- (H = budget(X), Y = X) ; (getBudget(T,Y)).
getMean([H|T],Y) :- (H = means(X), Y = X) ; (getMean(T,Y)).
getOfferPeriod(Pref,O):-
    (   member(period(_),Pref),
    O=offer(_,_,_,_,_,P1,_,_),
    getPeriod(Pref,P2),
   overlapPeriod(P2,P1)),!;
    (   
    \+ member(period(_),Pref),
             O=offer(_,_,_,_,_,_,_,_)     ).
    

getOfferAccommodation(Pref,O):-
   (    member(accommodation(_),Pref),
    getAccommodation(Pref,A2),
    offerAccommodation(O,A2));
    (   \+ member(accommodation(_),Pref),
    offerAccommodation(O,_)).

getOfferMeans(Pref,O):-
    (   member(mean(_),Pref),
    getMean(Pref,A2),
    offerMean(O,A2));
    (   \+ member(mean(_),Pref),
    offerMean(O,_)).
getOfferActivity(Pref,O):-
    (   member(activity(_),Pref),
    O=offer(_,A1,_,_,_,_,_,_),
    getActivity(Pref,A2),
    allSubsets(A1,A2));
  (   \+ member(activity(_),Pref),
    O=offer(_,_,_,_,_,_,_,_)).
getOfferDest(Pref,O):-
    (   member(dest(_),Pref),
    O=offer(D1,_,_,_,_,_,_,_),
    getDestination(Pref,D2),
    D1==D2);
  (   \+ member(dest(_),Pref),
   O=offer(_,_,_,_,_,_,_,_)).
getOfferbudget(Pref,O):-
    (   member(budget(_),Pref),
    O=offer(_,_,B1,_,_,_,_,_),
    getBudget(Pref,B2),
    B2>=B1);
  (   \+ member(budget(_),Pref),
    O=offer(_,_,_,_,_,_,_,_)).

getOffer([],O):- offerMean(O,_). 
getOffer(Pref,O):- 
    offerMean(O,_),
    getOfferPeriod(Pref,O),
    getOfferbudget(Pref,O),
    getOfferActivity(Pref,O),
    getOfferDest(Pref,O),
    getOfferMeans(Pref,O),
    getOfferAccommodation(Pref,O).
recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
    choosePreferences(Prefs,ChosenPrefs),
    getOffer(ChosenPrefs,O).
    

insertion_sort(List,Sorted):-
    i_sort(List,[],Sorted).
i_sort([],X,X).
i_sort([H|T],Accumulator,Sorted):-
    insert(H,Accumulator,N)
    ,i_sort(T,N,Sorted).
insert([H,S],[[H1,S1]|T],[[H1,S1]|NT]):-S>S1,insert([H,S],T,NT).
insert([H,S],[[H1,S1]|T],[[H,S],[H1,S1]|T]):-S=<S1.
insert([H,S],[],[[H,S]]).

removeHelper(C,0,C).
removeHelper([_|T],X,L):- X\==0,
                         X1 is X-1,
                         removeHelper(T,X1,L).

append1([],L,L).
append1([H|T],L,[H|T1]):- append1(T,L,T1).

recommendOffer1([],[]).
recommendOffer1([[HC,_]|T],[HC|L]):-    
                                  recommendOffer1(T,L).

helperr([], [], O, C,KLP) :- 
                               (    length(C,L1),
                                O=offer(_,_,_,_,_,_,_,N),
                                  
                                L1=<N,                                          
                                recommendOffer1(C,KLP));
                                (length(C,L1),
                                O=offer(_,_,_,_,_,_,_,N),   
                                L1>N,
                                insertion_sort(C,CS),
                                removeHelper(CS,L1-N,C1),!,
                                recommendOffer1(C1,KLP)).






helperr([HC|TC],[HP|TP],O, C,KLP) :-       
                                          choosePreferences(HP,P1),
                                          preferenceSatisfaction(O, HC, P1, S),
                                          append1(C,[[HC,S]],T1),!,
                                          helperr(TC,TP,O,T1,KLP).
                                         

recommendOffer(X,Y,O,KLP):-
    offerMean(O,_),           
    helperr(X,Y,O,_,KLP).