% Fresh by Devin Torres <devin@devintorres.com>

-module(fresh_util).

-export([expire_cookie/1]).

expire_cookie(Key) ->
    mochiweb_cookies:cookie(Key, "expired", [{path, "/"}, {max_age, 0}]).
