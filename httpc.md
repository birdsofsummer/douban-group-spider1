```erlang

    A="https://www.baidu.com".
    inets:start(),
    ssl:start().
    % httpc:which_cookies().
    %[{session_cookies,[]}]

    httpc:set_options([{cookies,enabled}]),
    httpc:request(get, {A,[]},[],[]).   
    % httpc:which_cookies().

```
[{session_cookies,
[
 {http_cookie,".baidu.com",false, "BAIDUID","016B21565063AD2BBA7DC14A581E529E:FG=1",undefined, session,"/",false,false,"0"},
 {http_cookie,".baidu.com",false,"BIDUPSID", "016B21565063AD2BBA7DC14A581E529E",undefined,session,"/", false,false,"0"},
 {http_cookie,".baidu.com",false,"PSTM","1571188486", undefined,session,"/",false,false,"0"}
]
}]
