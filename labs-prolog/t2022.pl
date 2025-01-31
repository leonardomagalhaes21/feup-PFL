% dish(Name, Price, IngredientGrams).
dish(pizza,         2200, [cheese-300, tomato-350]).
dish(ratatouille,   2200, [tomato-70, eggplant-150, garlic-50]).
dish(garlic_bread,  1600, [cheese-50, garlic-200]).

:- dynamic ingredient/2.

% ingredient(Name, CostPerGram).
ingredient(cheese,   4).
ingredient(tomato,   2).
ingredient(eggplant, 7).
ingredient(garlic,   6).


get_len([], 0).
get_len([_|T], N) :-
    get_len(T, A),
    N is A + 1.


% count_ingredients(?Dish, ?NumIngredients)
count_ingredients(Dish, NumIngredients) :-
    dish(Dish, _, IngredientGrams),
    get_len(IngredientGrams, NumIngredients).


% ingredient_amount_cost(?Ingredient, +Grams, ?TotalCost)
ingredient_amount_cost(Ingredient, Grams, TotalCost) :-
    ingredient(Ingredient, Cost),
    TotalCost is Cost * Grams.



dish_cost([], 0).
dish_cost([_-C|T], N) :-
    dish_cost(T, A),
    N is A + C.

% dish_profit(?Dish, ?Profit)
dish_profit(Dish, Profit) :-
    dish(Dish, Price, L),
    dish_cost(L, Cost),
    Profit is Price - Cost.

% update_unit_cost(+Ingredient, +NewUnitCost)
update_unit_cost(Ingredient, NewUnitCost) :-
    retractall(ingredient(Ingredient, _)),
    asserta(ingredient(Ingredient, NewUnitCost)).


% most_expensive_dish(?Dish, ?Price)
most_expensive_dish(Dish, Price) :-
    dish(Dish, Price, _),
    \+ (dish(_, P, _), P > Price).


%consume_ingredient(+IngredientStocks, +Ingredient, +Grams, ?NewIngredientStocks)
consume_ingredient(IngredientStocks, Ingredient, Grams, NewIngredientStocks) :-
    append(Pre,[Ingredient-P | Pos], IngredientStocks),
    Grams < P,
    A is P - Grams,
    append(Pre, [Ingredient-A | Pos], NewIngredientStocks).






%count_dishes_with_ingredient(+Ingredient, ?N)
count_dishes_with_ingredient(Ingredient, N) :-
    get_dishes(Ingredient, [], Dishes),
    length(Dishes, N).



get_dishes(Ingredient, Acc, Dishes) :-
    dish(D, _, Ingredients),
    \+ member(D, Acc),
    member(Ingredient-_, Ingredients),
    append(Acc, [D], Acc1),
    get_dishes(Ingredient, Acc1, Dishes), !.

get_dishes(_, Acc, Acc).









get_ingredients([],[]) :- !.
get_ingredients([X-_|T], [X|T2]) :-
    get_ingredients(T,T2).


%list_dishes(?DishIngredients)
list_dishes(DishIngredients) :-
    findall(
        Name-Ingredients,
        (
        dish(Name,_, I),
        get_ingredients(I, Ingredients)
        ),
        DishIngredients
    ).




lucra([],[]) :- !.
lucra([_-B|T], [B|T2]) :-
    lucra(T, T2).

rev([], []) :- !.
rev([H|T], L) :-
    rev(T, A),
    append(A, [H], L).


% most_lucrative_dishes(?Dishes)
most_lucrative_dishes(Dishes) :-
    findall(
        P-D,
        (
            dish(D, _ ,_),
            dish_profit(D, P)
        ),
        L
    ),
    sort(L, A),
    lucra(A, B),
    rev(B, Dishes).

