-module(geometry).
-export([area/1]).

area({rectangle,Width,Ht})	-> Width * Ht;
area({circle,R}) when R > 0	-> 3.141592 * R * R;
area({square,X}) when X > 0	-> X * X.
