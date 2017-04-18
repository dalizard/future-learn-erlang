-module(shapes).
-export([perimeter/1, area/1, enclose/1]).

-include("shapes.hrl").

perimeter(Shape) when is_record(Shape, square) ->
  #square{a = Side} = Shape,
  Side * 4;
perimeter(Shape) when is_record(Shape, circle) ->
  #circle{r = Radius} = Shape,
  Radius * math:pi() * 2;
perimeter(Shape) when is_record(Shape, rectangle) ->
  #rectangle{a = A, b = B} = Shape,
  A * 2 + B * 2;
perimeter(Shape) when is_record(Shape, triangle) ->
  #triangle{a = A, b = B, c = C} = Shape,
  A + B + C.

area(Shape) when is_record(Shape, square) ->
  #square{a = Side} = Shape,
  math:pow(Side, 2);
area(Shape) when is_record(Shape, circle) ->
  #circle{r = Radius} = Shape,
  math:pi() * math:pow(Radius, 2);
area(Shape) when is_record(Shape, rectangle) ->
  #rectangle{a = A, b = B} = Shape,
  A * B;
area(Shape) when is_record(Shape, triangle) ->
  #triangle{a = A, b = B, c = C} = Shape,
  S = (A + B + C) / 2,
  math:sqrt(S * (S - A) * (S - B) * (S - C)).

enclose(Shape) when is_record(Shape, square) ->
  #square{a = A} = Shape,
  #rectangle{a = A, b = A};
enclose(Shape) when is_record(Shape, circle) ->
  #circle{r = Radius} = Shape,
  #rectangle{a = Radius * 2, b = Radius * 2};
enclose(Shape) when is_record(Shape, rectangle) ->
  Shape;
enclose(Shape) when is_record(Shape, triangle) ->
  #triangle{a = A, b = B, c = C} = Shape,
  [A, B, _] = lists:sort([A, B, C]),
  #rectangle{a = A, b = B}.
