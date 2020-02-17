%---------------------------------------------------------------------------%
:- module types.

:- interface.

% elem is discriminated union of 2 operations:
%   step(a, b) -- is a step forward on a * x + b * y, where (x,y) is some basis.
%     This is needed because x and y can be non-reductible to each other via rational number.
%     In a classic tangram we have x = 1 and y = square root of 2 (or some multiple of it, actually 0.5 * sqrt(2)).
%  turn(degree) -- is a turn left for a specified number of degrees.
:- type elem ---> step(int,int); turn(int).

% left is a turn left specified in degrees.
:- func left(int) = elem.

% right is a turn right specified in degrees.
:- func right(int) = elem.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module int.

left(A) = turn(A mod 360).
right(A) = turn((-A) mod 360).
%---------------------------------------------------------------------------%