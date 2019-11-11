-module(douban).
%-include_lib("eunit/include/eunit.hrl").
%-compile(export_all).
-export([
         http_get/1,
         test1/0,
         test2/0,
         test3/0,
         db_list/0,
         db_query/1,
         db_insert/2,
         db_delete/1,
         db_c/1,
         db_d/1,
         db_r/1,
         join/2,
         join/3,
         to_qs/1,
         save_html/2,
         save_htmls/2,
         test_save_htmls/2,
         html_to_json/1,
         html_to_zip/1,
         douban_groups0/2,
         douban_groups2/0,
         start/2, 
         ping/2, 
         pong/0,
         save_to_db/0
        ]).

-import(rand,[normal/0]).
-import(rfc4627,[encode/1,decode/1]).

-record(position,{x,y}).
-record(html,{url,body,msg,code,ok}).
-record(html1,{url,group,no,body,msg,code,ok}).

-define(CurlHeaders,[ {"user-agent", "curl/7.61.1"} ,{"accept", "*/*"} ]).
-define(Myheader, [
         { "connection","Keep-Alive"},
         { "pragma","no-cache"},
 %       { "User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:67.0) Gecko/20100101 Firefox/67.0" },
 %       { "Content-Type", "application/x-www-form-urlencoded" },
         { "Content-Type", "text/html"},         
         { "Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" },
         { "Accept-Language", "zh-CN,en-US;q=0.7,en;q=0.3" },
         { "Connection", "keep-alive" },
         { "Upgrade-Insecure-Requests", "1" },
         { "Pragma", "no-cache" },
         { "Cache-Control", "no-cache" }
       ]).


-define(MyUA, [
	"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/22.0.1207.1 Safari/537.1",
	"Mozilla/5.0 (X11; CrOS i686 2268.111.0) AppleWebKit/536.11 (KHTML, like Gecko) Chrome/20.0.1132.57 Safari/536.11",
	"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.6 (KHTML, like Gecko) Chrome/20.0.1092.0 Safari/536.6",
	"Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.6 (KHTML, like Gecko) Chrome/20.0.1090.0 Safari/536.6",
	"Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/19.77.34.5 Safari/537.1",
	"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/536.5 (KHTML, like Gecko) Chrome/19.0.1084.9 Safari/536.5",
	"Mozilla/5.0 (Windows NT 6.0) AppleWebKit/536.5 (KHTML, like Gecko) Chrome/19.0.1084.36 Safari/536.5",
	"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1063.0 Safari/536.3",
	"Mozilla/5.0 (Windows NT 5.1) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1063.0 Safari/536.3",
	"Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; Trident/4.0; SE 2.X MetaSr 1.0; SE 2.X MetaSr 1.0; .NET CLR 2.0.50727; SE 2.X MetaSr 1.0)",
	"Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1062.0 Safari/536.3",
	"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1062.0 Safari/536.3",
	"Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; 360SE)",
	"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1061.1 Safari/536.3",
	"Mozilla/5.0 (Windows NT 6.1) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1061.1 Safari/536.3",
	"Mozilla/5.0 (Windows NT 6.2) AppleWebKit/536.3 (KHTML, like Gecko) Chrome/19.0.1061.0 Safari/536.3",
	"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.24 (KHTML, like Gecko) Chrome/19.0.1055.1 Safari/535.24",
	"Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/535.24 (KHTML, like Gecko) Chrome/19.0.1055.1 Safari/535.24",
    "Mozilla/5.0 (X11; Linux x86_64; rv:70.0) Gecko/20100101 Firefox/70.0"
   ]).

-define(DB,"http://127.0.0.1:5984/").
-define(Baidu,"http://www.baidu.com").
-define(Douban,"http://www.douban.com").
-define(Echo,"https://service-9rx17sto-1252957949.ap-hongkong.apigateway.myqcloud.com/release/douban/echo").
-define(GID,["259104","blabla","gua"]).
-define(GID1,
        [
              {"259104","鹅们栖息地","https://img1.doubanio.com/view/group/sqxs/public/105835b9c5e3089.jpg"},
              {"blabla":"鹅组","https://img3.doubanio.com/view/group/sqxs/public/580e4e9f613dd6d.jpg"},
              {"gua","自由吃瓜基地","https://img3.doubanio.com/view/group/sqxs/public/1c0a5162bddf475.jpg"}
        ]
       ).

%https://www.jianshu.com/p/8b18a2c8f40f
encode1(D)->encode({obj,D}).
record_values(A)-> lists:map(fun(X)->element(X,A) end,lists:seq(2,tuple_size(A))).
html_to_zip(R)->
    K=record_info(fields,html),
    lists:zip(K,record_values(R)).
html_to_json(R)-> encode({obj,html_to_zip(R)}).

html_to_zip1(R)->
    K=record_info(fields,html1),
    lists:zip(K,record_values(R)).
html_to_json1(R)-> encode({obj,html_to_zip1(R)}).

write1(Name,C)->file:write_file(Name,C).
write(Name,C)->file:write_file(Name,[["\t",Y,"\n"]||Y<-C]).


join(C,P)->[lists:concat([A,P,B])||{A,B}<-C].
join(C,P1,P2)->lists:foldr(fun(X,Y)->
                                   case Y of 
                                       ""-> X; 
                                       _-> X++P2++Y 
                                   end 
                           end,"",join(C,P1)).
to_qs(X)->case  X
          of
              []->"";
              _->"?" ++ join(X,"=","&")
          end.


rnd()->normal().
rnd1(Z)->fun()->L=length(Z),
                N=abs(trunc(length(Z)*rand:normal())),
                if 
                    (N>L) or ( N < 1 ) -> 1  ;
                    true ->N
                end
         end.
rnd_nth(Z)-> F= rnd1(Z),
             lists:nth(F(),Z).

number_to_str(N)->lists:flatten(io_lib:format("~p", [N])).
rnd_number()->number_to_str(rnd()).
number(N)->lists:sublist(rnd_number(),4,N).
%number1(N)->string:slice(rnd_number(),3,N).
word()->
    A=lists:seq(1,26),
    [X+96||X<-A]++[X+64||X<-A].

shuffle(X,N)->
    W=lists:sort(fun(A,B)->rand:normal()>1 end,X),
    lists:sublist(W,N).


% C=[ {"ll","118282"}, {"bid","buLesCJtHig"}],
fake_cookie()->
    B=shuffle(word(),11),
    L=number(6),
    [{"ll",L}, {"bid",B}].

list_to_cookie(C)->lists:concat([K++"="++V++"; "||{K,V}<-C]).
find_cookie(H)->[X||{"set-cookie",X}<-H].
% "ccc=1; ddd=x; "
find_cookie_str(H)-> lists:concat([hd(string:split(X,";"))++"; "||{"set-cookie",X}<-H]).


%Method=[head,get,put,psot,trace,options,delete].
ajax_form(Method,Headers,Url,Data)->
	inets:start(),
	ssl:start(),
	case httpc:request(Method, {Url,Headers,"application/x-www-form-urlencoded", Data},[],[]) 
    of
		{ok, {_,_,Body}}->io:fwrite("~ts",[Body]), Body;
		{error, Reason}->io:format("error cause ~p~n",[Reason])
	end.

parse_json({ok,{R,H,B}})->{ok,{R,H,decode(B)}}.

ajax_json(Method,Headers,Url,Data)->
	inets:start(),
	ssl:start(),
    httpc:set_options([{cookies,enabled}]),
    H=[{"Content-type","application/json"}],
    case Method
    of 
        get->R=httpc:request(get, {Url,Headers},[],[]),parse_json(R);
        delete->R=httpc:request(get, {Url,Headers},H,[]),parse_json(R);
        _-> R=httpc:request(Method, {Url,Headers,"application/json",Data},[],[]),
          parse_json(R)
    end.

ajax_json(Method,Url,Data)->ajax_json(Method,[],Url,Data).
ajax_json(Method,Url)->ajax_json(Method,[],Url,"").
ajax_json(Url)->ajax_json(get,[],Url,"").
ajax_json_d(Url)->ajax_json(delete,[],Url,"").


% couchdb
db_query(U)-> ajax_json(get,[],?DB++U,"").
db_insert(U,D)-> ajax_json(put,[],?DB++U,D).
db_delete(U)-> ajax_json(delete,[],?DB++U,"").
db_list()-> db_query("_all_dbs").
db_utils()-> db_query("_utils").

db_c(N)->ajax_json(put,?DB++N).
db_d(N)->ajax_json(delete,?DB++N).
db_r(N)->ajax_json(?DB++N).



http_get(U)->
    inets:start(),
    ssl:start() ,
    httpc:set_options([{cookies,enabled}]),
    Ua=rnd_nth(?MyUA),
    H=[
       { "Referer", U },
       {"User-Agent",Ua} 
       |  ?Myheader ],
    httpc:request(get, {U, H}, [], []).

%   PostBody = "{ \"title\": \"The Title\", \"content\": \"The Content\" }",
%   Url = "http://some.url/endpoint",

http_post(Url,PostBody)->
    ssl:start(),
    application:start(inets),
    H=[],
    httpc:request(post, 
        {Url, H, 
        "application/x-www-form-urlencoded",
        PostBody
        }, [], []).

save_html(U,ID)-> 
        Url=lists:concat(["test/",ID]),
        {ok,{{_Version,Code,Msg},_Headers,Body}} =http_get(U),
        D=#html{url=list_to_atom(U),msg=list_to_atom(Msg),body=list_to_atom(Body),code=Code,ok=ok},
        D1= html_to_json(D),
 %       io:format("~p~n",[D1]).
        [Code,Url,D1].

save_html1(U,ID,GID,NO)->
        Url=lists:concat(["test/",ID]),
        {ok,{{_Version,Code,Msg},_Headers,Body}} =http_get(U),
        D=#html1{
             url=list_to_binary(U),
             msg=list_to_binary(Msg),
             body=list_to_binary(Body),
             code=Code,
             ok=ok,
             group=list_to_binary(GID),
             no=NO
        },
        D1= html_to_json1(D),
 %       io:format("~p~n",[D1]).
        [Code,Url,D1].

save_htmls([],ID)-> {ID,done};
save_htmls([H|T],ID)->
           [Code,Url,D1] = save_html(H,ID),
           case Code of
               200 -> 
                    R=db_insert(Url,D1),
                    io:format("~p~n",[R]),
                    save_htmls(T,ID+1);
               _ -> {ID,Code}
           end.

% lists:foldr(fun(X,[{C,D}|T])->[{C+1,X},{C,D}|T] end,[{ID,hd(A2)}],tl(A2)).
douban_page(GID,N)->
    Prefix="https://www.douban.com/group/",
    Suffix="/discussion?start=",
    Step=25,
    lists:concat([Prefix,GID,Suffix,Step*N]) .
    %lists:concat(["https://www.baidu.com?q=/",GID,Suffix,Step*N])
    
douban_group(GID,P1,P2)->
    [ douban_page(GID,X)  || X <- lists:seq(P1,P2)].

douban_groups(P1,P2)-> [ douban_group(X,P1,P2)  || X<- ?GID ].
douban_groups1() -> douban_groups(0,80).

douban_group1(GID,P1,P2)->
    [ {GID,X,douban_page(GID,X)}  || X <- lists:seq(P1,P2)].
douban_groups0(P1,P2)-> lists:flatten([ douban_group1(X,P1,P2)  || X<- ?GID ]).
douban_groups2()-> douban_groups0(0,80).

%%% run
ping({0,_,_}, {Pong_PID,Db_PID}) ->
    Pong_PID ! finished,
    Db_PID! finished,
    io:format("ping finished~n", []);
ping({N,ID,[ {GID,NO,U}|T] },{Pong_PID,Db_PID} ) ->
    Pong_PID ! {start, self()},
    receive
        {stop,Why} ->
            Pong_PID ! {stop,Why},
            io:format("stop download ~p~n",[Why]);
        download ->
            [Code,Url,D1] = save_html1(U,ID,GID,NO),
            Db_PID! {db,N,[ID,Code,Url,D1,GID,NO,U],self() },
            io:format("download ~p~n", [[{N,U},Pong_PID]])
    end,
    timer:sleep(3000),
    ping({N - 1,ID+1,T}, {Pong_PID,Db_PID}).

save_to_db()->
    receive
        finished ->
            io:format("finished~n", []);
        {stop,Why}->
            io:format("stop db~p ~n", [Why]);
        {db,N,[ID,Code,Url,D1,GID,NO,U], Download_PID}->
               case Code of
                   200 -> 
                        db_insert(Url,D1),
                        io:format("ccccc ~p~n",[[ID,Code,Url,GID,NO,U]]) ;
                        %io:format("~p~n",[R]) ;
                   _ ->
                       Err= {stop,{ID,Code}},
                       Download_PID ! Err,
                       self()!Err
                end,
                io:format("save to db ~p~n", [[N,[ID,Code,Url,D1]]]),
                save_to_db()
    end.

pong() ->
    receive
        finished ->
            io:format("finished~n", []);
        {stop,Why}->
            io:format("stop pong~p ~n", [Why]);
        {start, Download_PID}->
            Download_PID ! download,
            io:format("download ~p~n", [Download_PID]),
            pong()
    end.

start(N,ID) ->
    Db_PID=spawn(douban,save_to_db,[]),
    Pong_PID = spawn(douban, pong, []),
    D=douban_groups0(0,N),
    io:format("begin ~p~n", [D]),
    N1=length(D),
    spawn(douban, ping, [{N1,ID,D}, {Pong_PID,Db_PID}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%test
test_to_qs()->A=[{x,1},{x,2}],
              B=[],
              to_qs(A),
              to_qs(B) .

test_db()->
    U="/test/5",
    U1="test/1?rev=1-cd90201763f897aa0178b7ff05eb80cb",
    D=[{x,1},{y,2}],
    db_insert(U,D),
    db_query(U1),
    db_delete(U),
    db_query(U).

test_save_htmls(N,ID)->
    U0= "http://localhost/?id=",
    U= [lists:concat([U0,X]) ||X<-lists:seq(1,N)],
    save_htmls(U,ID).

test1()-> {ok, {{Version, 200, ReasonPhrase}, Headers, Body}}=http_get(?Douban).
test2()->
    {ok,{_,Headers,Body}}=http_get(?Echo),
    {ok,{obj,Data},_}=decode(Body),
    Data.

test3()->http_get(?Baidu).

test_find_cookie()->
    H= [
         {"connection","Keep-Alive"},
         {"date","Wed, 31 Jul 2019 11:51:21 GMT"},
         {"pragma","no-cache"},
         {"accept-ranges","bytes"},
         {"etag","\"588604dd-94d\""},
         {"server","bfe/1.0.8.18"},
         {"content-length","2381"},
         {"content-type","text/html"},
         {"last-modified","Mon, 23 Jan 2017 13:27:57 GMT"},
         {"set-cookie","BDORZ=27315; max-age=86400; domain=.baidu.com; path=/"},
         {"set-cookie","ddd=x;ss"}
       ],
    A=find_cookie(H),
    B=find_cookie_str(H).


list2json(L)->L.
parse_position(P)->
    P1=#position{x=1,y=2},
    P2=#position{x=11,y=22},
    L=[P1,P2],
    list2json(L).

