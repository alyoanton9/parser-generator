# Parser Generator

Program, that generates parsers from LL1 input grammar with synthesized attributes.

Reads input grammar with following structure:

  - Header: module name + imports

    Example:

    ```hs
    module Parser (parse) where
    import Ast (Expression(..))
    ```

    \* `Ast` usually represents attributes
    
  - Tokens: `token name = token value {regexp's usage} {constructor's name}`
  
    Example:
    
    ```hs
    plus = "\+"         { const PlusT }     { PlusT }
    minus = "-"         { const MinusT }    { MinusT }
    star = "\*"         { const StarT }     { StarT }
    slash = "\\"        { const SlashT }    { SlashT }
    op = "\("           { const OpParT }    { OpParT }
    cl = "\)"           { const ClParT }    { ClParT }
    n = "[0-9]+"        { NumT . read }     { NumT $$ }
    ws = "[[:space:]]+" ;
    ```
    
   - `"%%"` to separate tokens from grammar rules
  
   - Grammar rules: `fromTerminal -> [toTerminal]  {attributes' actions}`
              
     Example:
     
     ```hs
     E -> T E'           { $2 $1 }
     E' -> plus T E'     { $3 . flip Add $2 }
     E' -> minus T E'    { $3 . flip Sub $2 }
     E' ->               { id }
     T -> F T'           { $2 $1 }
     T' -> star F T'     { $3 . flip Mul $2 }
     T' -> slash F T'    { $3 . flip Div $2 }
     T' ->               { id }
     F -> n              { Num $1 }
     F -> op E cl        { $2 }
     F -> minus F        { Neg $2 }
     ```
              
   - `data Token` definition
   
     Example:
    
     ```hs
      {
        data Token
          = OpParT
          | ClParT
          | PlusT
          | MinusT
          | StarT
          | SlashT
          | NumT Int
          deriving Eq
      }
     ```
              
      \* `../assets/Common.hs` defines parser's instances (common for each generated parser) 
             
