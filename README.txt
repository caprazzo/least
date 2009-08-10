{ok,[
		[
			[{fetch,"['queen','mickael jackson',-]"}, nil],
			[
				{oper,1,"+"},
				[
					[{fetch,"['radiohead','beck',-]"},nil],
				nil
			]
		]
	]
]
}

{ok,[	
		{
			{
				{artist,"queen"},
				{
					{oper,1,"-"},
					{artist,"mickael jackson"}
				}
			},
			{
				{oper,1,"+"},
				{
					{artist,"radiohead"},
					{
						{oper,1,"-"},
						{artist,"beck"}
					}
				}
			}
		}
	]
}

{ok,[	
		{calc,
			{calc, {calc,"queen"}, {oper,"-"}, {calc,"mickael jackson"}},
			{oper,"+"},
			{calc, {calc,"radiohead"}, {oper,"-"}, {calc,"beck"}}
		}
	]
}


{ok,[
		{
			{calc,
				{calc,1,"queen"},
				{oper,1,"-"},
				{calc,1,"mickael jackson"}
			},
			{oper,1,"+"},
			{calc,
				{calc,1,"radiohead"},
				{oper,1,"-"},
				{calc,1,"beck"}
			}
		}
	]
}

% PARSE GOAL:
% {ok,[	
%		{calc,
%			{calc, {calc,"queen"}, {oper,"-"}, {calc,"mickael jackson"}},
%			{oper,"+"},
%			{calc, {calc,"radiohead"}, {oper,"-"}, {calc,"beck"}}
%		}
%	]
%}

LAST:
{ok,[
		{calc,
			{calc, {calc,"queen"}, {oper,"-"}, {calc,"mickael jackson"}},
			{oper,"+"},
			{calc,{calc,"radiohead"},{oper,"-"},{calc,"beck"}}
		}
	]
}
		   

{ok,[
		{calc,
			{calc,{calc,1,"queen"}, {oper,1,"-"}, {calc,1,"mickael jackson"}},
			{oper,1,"+"},
			{calc, {calc,1,"radiohead"}, {oper,1,"-"}, {calc,1,"beck"}}
		}
	]
}

{ok,[
		[
			{calc,
				{
					{calc,1,"queen"},{oper,1,"-"}, {calc,1,"mickael jackson"}
				}
			},
			[
				{oper,1,"+"},
				{calc, 
					{
						{calc,1,"radiohead"},
						{oper,1,"-"},
						{calc,1,"beck"}
					}
				}
			]
		]
	]
}

{ok,[
		[
			{calc,2,
				{calc,1,"queen"},{oper,1,"-"},{calc,1,"mj"}
			},
      		[
      			[
      				{oper,1,"+"}
      			],
       			{calc,2,
       				{calc,1,"pink"},{oper,1,"-"},{calc,1,"floyd"}
       			}
       		]
       	]
	]
}

{ok,[
		[
			{calc,2,
				{calc,1,"queen"},{oper,1,"-"},{calc,1,"mj"}
			},
      		[
      			{oper,1,"+"},
       			{calc,2,
       				{calc,1,"pink"},{oper,1,"-"},{calc,1,"floyd"}
       			}
       		]
       	]
	]
}

{ok,[
		[
			{calc,2,
				{calc,1,"queen"},{oper,1,"-"},{calc,1,"mj"}
			},
   			{oper,1,"+"}|
   			{calc,2,
   				{calc,1,"pink"},{oper,1,"-"},{calc,1,"floyd"}
   			}
   		]
   	]
   }

((queen-mj)+(pink-floyd))

{calc, {calc}, {op}, {calc}}

{calc, 1 {
	{calc, {calc, 1, "queen"},{oper,1,"-"},{calc,1,"mj"}},
	{oper, 1, "+"}
	{calc, {calc, 1, "pink"}, {oper,1,"-"},{calc,1,"floyd"}}
}

{ok,[
		[
			[
				{calc,1,"queen"},{oper,1,"-"},{calc,1,"mj"}
			],
      		[
      			{oper,1,"+"}, {calc,1,"pink"}, {oper,1,"-"},
       			{calc,1,"floyd"}
       		]
       	]
	]
}

Proplist: [{"name",<<"Traveling Wilburys">>},
           {"mbid",<<"4387b544-e4d0-4f18-b74e-d4a5ba1582ab">>},
           {"match",<<"6.9">>},
           {"url",<<"www.last.fm/music/Traveling+Wilburys">>},
           {"image",
            [{obj,[{"#text",
                    <<"http://userserve-ak.last.fm/serve/34/2245094.jpg">>},
                   {"size",<<"small">>}]},
             {obj,[{"#text",
                    <<"http://userserve-ak.last.fm/serve/64/2245094.jpg">>},
                   {"size",<<"medium">>}]},
             {obj,[{"#text",
                    <<"http://userserve-ak.last.fm/serve/126/2245094.jpg">>},
                   {"size",<<"large">>}]},
             {obj,[{"#text",
                    <<"http://userserve-ak.last.fm/serve/252/2245094.jpg">>},
                   {"size",<<"extralarge">>}]},
             {obj,[{"#text",
                    <<"http://userserve-ak.last.fm/serve/500/2245094/Traveling+Wilburys.jpg">>},
                   {"size",<<"mega">>}]}]},
           {"streamable",<<"1">>}]