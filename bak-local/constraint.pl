:- dynamic op/3.
% Arrow operator
op(200, xfy, '=>').

% Logic of Map function
% Using type inference.
?- Map = F => L => Result,
|   L = [_], Result = [_],
|   L = [E], Es = [E],
|   Map = F => Es => FEs,
|   F = E => FE, 
|   Result = [FE], FEs = [FE].

% Logic for Filter function
?- Filter = F => L => Result,
|   L = [_],
|   L = [X], Xs = [X],
|   F = X => bool,
|   Result = [X], Fxs = [X],
|   Result = Fxs.