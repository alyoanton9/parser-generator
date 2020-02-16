{
module GrammarLexer where
}

%wrapper "basic"

$lowletter  = a-z
$highletter = A-Z
$digit      = 0-9
$under      = _
$quote      = "
$apost      = '

tokens :-
    $white+                                     ;
    $lowletter [$lowletter $under]*             { TokenNameToken }
    \=                                          { \_ -> EqToken }
    $quote(.*)$quote                            { RegexToken . tail . init }
    \{(\n | [^\}\{])*\}                         { CodeToken . tail . init }
    \;                                          { \_ -> SemiColToken }
    "%%"                                        { \_ -> ProcToken }

    "->"                                        { \_ -> ArrowToken }
    $highletter [$highletter $digit $apost]*    { NonTermToken }
    "eps"                                       { \_ -> EpsToken }
    \;                                          { \_ -> DelimToken }

{
data GrammarToken   = TokenNameToken String | EqToken | RegexToken String
                    | CodeToken String | SemiColToken | ProcToken
                    | ArrowToken | NonTermToken String | EpsToken | DelimToken
                    deriving Eq
}
