#!/usr/bin/env swipl
factorial(0, 1).

factorial(N, V) :-
  N > 0,
  N1 is N - 1,
  factorial(N1, R),
  V is R * N.

gcdPriv(A, 0, A).

gcdPriv(A, B, C) :-
  B1 is A mod B,
  A1 is B,
  gcdPriv(A1, B1, C1),
  C is C1.

gcd(A, B, C) :- gcdPriv(abs(A), abs(B), C).

extendedEuclideanPriv(A, 0, X, Y, R, S, XR, YR, AR) :-
  XR is X,
  YR is Y,
  AR is A.
extendedEuclideanPriv(A, B, X, Y, R, S, XR, YR, AR) :-
  C is A mod B,
  Q is A div B,
  NEWR is (X - Q * R),
  NEWS is (Y - Q * S),
  extendedEuclideanPriv(B, C, R, S, NEWR, NEWS, V1, V2, V3),
  XR is V1,
  YR is V2,
  AR is V3.

extendedEuclidean(A, B, X, Y, G) :-
  A1 is abs(A),
  B1 is abs(B),
  extendedEuclideanPriv(A1, B1, 1, 0, 0, 1, X, Y, G).

solveLDE(A, B, C, X, Y) :-
  extendedEuclidean(A, B, X0, Y0, G),
  ((abs(C) mod G) > 0 -> false ;
  X is X0 * (C div G),
  Y is Y0 * (C div G)).