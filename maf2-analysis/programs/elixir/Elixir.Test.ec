[{attribute,2,file,{"Elixir.Test.ex",2}},
 {attribute,2,module,'Elixir.Test'},
 {attribute,2,compile,[no_auto_import]},
 {attribute,2,export,[{'__info__',1},{calc,1},{doSomething,1}]},
 {attribute,2,spec,
     {{'__info__',1},
      [{type,2,'fun',
           [{type,2,product,
                [{type,2,union,
                     [{atom,2,attributes},
                      {atom,2,compile},
                      {atom,2,functions},
                      {atom,2,macros},
                      {atom,2,md5},
                      {atom,2,exports_md5},
                      {atom,2,module},
                      {atom,2,deprecated},
                      {atom,2,struct}]}]},
            {type,2,any,[]}]}]}},
 {function,0,'__info__',1,
     [{clause,0,[{atom,0,module}],[],[{atom,0,'Elixir.Test'}]},
      {clause,0,
          [{atom,0,functions}],
          [],
          [{cons,0,
               {tuple,0,[{atom,0,calc},{integer,0,1}]},
               {cons,0,
                   {tuple,0,[{atom,0,doSomething},{integer,0,1}]},
                   {nil,0}}}]},
      {clause,0,[{atom,0,macros}],[],[{nil,0}]},
      {clause,0,[{atom,0,struct}],[],[{atom,0,nil}]},
      {clause,0,
          [{atom,0,exports_md5}],
          [],
          [{bin,0,
               [{bin_element,0,
                    {string,0,
                        [210,104,123,97,207,31,237,249,196,37,186,176,155,235,
                         178,214]},
                    default,default}]}]},
      {clause,0,
          [{match,0,{var,0,'Key'},{atom,0,attributes}}],
          [],
          [{call,0,
               {remote,0,{atom,0,erlang},{atom,0,get_module_info}},
               [{atom,0,'Elixir.Test'},{var,0,'Key'}]}]},
      {clause,0,
          [{match,0,{var,0,'Key'},{atom,0,compile}}],
          [],
          [{call,0,
               {remote,0,{atom,0,erlang},{atom,0,get_module_info}},
               [{atom,0,'Elixir.Test'},{var,0,'Key'}]}]},
      {clause,0,
          [{match,0,{var,0,'Key'},{atom,0,md5}}],
          [],
          [{call,0,
               {remote,0,{atom,0,erlang},{atom,0,get_module_info}},
               [{atom,0,'Elixir.Test'},{var,0,'Key'}]}]},
      {clause,0,[{atom,0,deprecated}],[],[{nil,0}]}]},
 {function,5,calc,1,
     [{clause,5,[{integer,5,42}],[],[{atom,5,ok}]},
      {clause,7,
          [{var,7,'_list@1'}],
          [[{call,7,
                {remote,7,{atom,7,erlang},{atom,7,is_list}},
                [{var,7,'_list@1'}]}]],
          [{match,8,{cons,8,{var,8,'_h@1'},{var,8,'_'}},{var,8,'_list@1'}},
           {op,9,'+',
               {op,9,'+',{var,9,'_h@1'},{integer,9,10}},
               {call,9,{remote,9,{atom,9,rand},{atom,9,uniform}},[]}}]}]},
 {function,12,doSomething,1,
     [{clause,12,
          [{var,12,'_pid@1'}],
          [],
          [{call,13,
               {remote,13,{atom,13,erlang},{atom,13,send}},
               [{var,13,'_pid@1'},{tuple,13,[{atom,13,message}]}]}]}]}]