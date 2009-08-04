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
