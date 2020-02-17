{
module GrammarParser where
import              GrammarLexer
import              Grammar
import qualified    Data.Map.Strict as Map
}

%name                   parseGrammar
%tokentype              {GrammarToken}
%error                  {parseError}

%token NAME             {TokenNameToken $$}
%token EQ               {EqToken}
%token REGEX            {RegexToken $$}
%token CODE             {CodeToken $$}
%token SEMI             {SemiColToken}

%token PROC             {ProcToken}

%token NONTERM          {NonTermToken $$}
%token ARR              {ArrowToken}
%token EPS              {EpsToken}
%token DEL              {DelimToken}

%%

grammar
    : CODE tokens PROC rules CODE   {($1, $2, $4, $5)}

tokens --better eliminate right recursion
    : token tokens                  {$1 : $2}
    | {- empty -}                   {[]}

token
    : NAME EQ REGEX CODE CODE       {Token $1 $3 (Just ($4, $5))}
    | NAME EQ REGEX SEMI            {Token $1 $3 Nothing}

rules
    : rule rules                    {$1 : $2}
    | {- empty -}                   {[]}

rule
    : NONTERM ARR prod CODE         {($1, ($3, $4))}

prod
    : elem prod                     {$1 : $2}
    | {- empty -}                   {[]}

elem
    : NONTERM                       {NT $1}
    | NAME                          {T $1}

{
    parseError _ = error "Parse error"
}
