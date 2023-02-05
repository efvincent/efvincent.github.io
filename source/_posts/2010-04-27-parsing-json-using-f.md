---
date: 2010-04-27 18:29:49
title: Parsing Json using F#
categories:
  - Functional
  - F#
tag:
  - f-sharp
  - functional
  - json
  - parser
---

I was in one of those curious moods the other day, and decided to check out the API for the service behind my RSS reader, [FeedDemon](http://www.newsgator.com/Individuals/FeedDemon/Default.aspx). I like this reader because it keeps my subscriptions and read / unread data synchronized across machines and my iPhone via an app called [NetNewsWire](http://netnewswireapp.com/iphone/).

<!-- more -->

Well it ends up that Google has gobbled up the service and it’s now Google Reader. Fair enough. It means you can also read your RSS feeds on the web at the Google reader site. Whatevs. It also turns out that Google has not publicized the API for that service yet. Yuck. But there are a couple of people out there (like [here](http://code.google.com/p/pyrfeed/wiki/GoogleReaderAPI) and [here](http://blog.martindoms.com/2009/08/15/using-the-google-reader-api-part-1/)) who have picked it apart using fiddler or some such thing.

Well to put this meandering story to an end, I was playing with the API and it seems that some of the methods only return JSON. Others return XML, and others still are switchable. Messy. No wonder it’s not public yet. So that brings me to the point of this post… Parsing Json using F#.

### Why F#?

I’ve avoided posting about F# to date, even though I love “playing” with it; I feel that posts about C# are more relevant. People are using F#, but I’ve never had the chance to use it on the job, and I don’t know anyone personally who has either. But since it’s _mainstream_ now, what the heck. Plus parsing is one of those tasks that’s right up F#’s alley. If you haven’t worked with a functional language since college (or ever), give it a whirl. It’s refreshingly different from pure, straight, intensely object oriented thinking.

### Json, Briefly

Most everyone knows what Json is by now. It’s just a tad more horrifying in real life as the supernatural killer from the Friday the 13th movies with whom it homophonetically shares its name. It’s a text format for representing data that can be evaluated in that wonderfully fast and lose language of the web, JavaScript, resulting in actual JavaScript objects. Read about it from the [experts here](http://json.org). Here’s an example of some awesome [Json](http://json.org):

```json
{
  "glossary": {
    "title": "example glossary",
    "GlossDiv": {
      "title": "S",
      "GlossList": {
        "GlossEntry": {
          "ID": "SGML",
          "SortAs": "SGML",
          "GlossTerm": "Standard Generalized Markup Language",
          "Acronym": "SGML",
          "Abbrev": "ISO 8879:1986",
          "GlossDef": {
            "para": "A meta-markup language, used to create markup languages such as DocBook.",
            "GlossSeeAlso": ["GML", "XML"]
          },
          "GlossSee": "markup"
        }
      }
    }
  }
}
```

### The Tokenizer

I’m no parsing / compiler / language expert, but I do know that tokenizing is the first step. This is when we run through the string and create tokens; identifying the open and close braces, the name value pairs, etc.

First thing we want to look at is the Token type, which is a discriminated union. This type defines the _logical_ things that are in the Json string. For example, an open quote, some characters, and a close quote is a string.

```ruby
type Token =
  | OpenBracket | CloseBracket
  | OpenArray | CloseArray
  | Colon | Comma
  | String of string
  | Number of string
```

For this simple parser, the tokens are as above, open and close bracket and square bracket (aka array), colon, comma, numbers, and strings. This is a good reason to start with Json, it’s painfully simple. The tokenize function below turns the string into a list of tokens.

```ruby
let tokenize source =

  let rec parseString acc = function
    | '\\' :: '"' :: t -> // escaped quote
                          parseString (acc + "\"") t
    | '"' :: t -> // closing quote terminates
                  acc, t
    | c :: t -> // otherwise accumulate
                parseString (acc + (c.ToString())) t
    | _ -> failwith "Malformed string."

  let rec token acc = function
    | (')' :: _) as t -> acc, t // closing paren terminates
    | (':' :: _) as t -> acc, t // colon terminates
    | (',' :: _) as t -> acc, t // comma terminates
    | w :: t when Char.IsWhiteSpace(w) -> acc, t // whitespace terminates
    | [] -> acc, [] // end of list terminates
    | c :: t -> token (acc + (c.ToString())) t // otherwise accumulate chars

  let rec tokenize' acc = function
    | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t   // skip whitespace
    | '{' :: t -> tokenize' (OpenBracket :: acc) t
    | '}' :: t -> tokenize' (CloseBracket :: acc) t
    | '[' :: t -> tokenize' (OpenArray :: acc) t
    | ']' :: t -> tokenize' (CloseArray :: acc) t
    | ':' :: t -> tokenize' (Colon :: acc) t
    | ',' :: t -> tokenize' (Comma :: acc) t
    | '"' :: t -> // start of string
      let s, t' = parseString "" t
      tokenize' (Token.String(s) :: acc) t'
    | '-' :: d :: t when Char.IsDigit(d) -> // start of negative number
        let n, t' = token ("-" + d.ToString()) t
        tokenize' (Token.Number(n) :: acc) t'
    | '+' :: d :: t | d :: t when Char.IsDigit(d) -> // start of positive number
        let n, t' = token (d.ToString()) t
        tokenize' (Token.Number(n) :: acc) t'
    | [] -> List.rev acc // end of list terminates
    | _ -> failwith "Tokinzation error"

  tokenize' [] source
```

We don’t have time for the full treatment of F#. If there’s any interest, I’ll post up some tutorials or at least links to some of the very many existing good tutorials out there already. For now, assume we have an understanding of the syntax, and the logic of tokenizing and parsing is what we’re after here.

One of the first things we see is that _tokenize_ defines a few functions within the function. Great feature of F#, allowing definitions of inner functions; it allows the coder to partition logic without having function explosion.

The driving inner function is _tokenize’_, whose signature is (Token list –> char list –> Token list). This means it takes a Token list and a char list, and returns a Token list. The first token list (acc) is an accumulator. This list is built up as the procedure calls itself recursively. This is a common pattern in functional languages, to “thread” an accumulator through recursive calls. The second parameter is a char list, or a list of characters from the source string.

We don’t see the char list explicitly because of the use of the _function_ keyword at like 20, this keyword means that the last parameter (the char list) is implicitly used in a match statement, the cases of which follow after the function keyword. The syntax would be equivalent to:

```ruby
let rec tokenize' acc sourceChars =
  match sourceChars with
  | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t   // skip whitespace
  | '{' :: t -> tokenize' (OpenBracket :: acc) t
  | '}' :: t -> tokenize' (CloseBracket :: acc) t
```

The idea that the sourceChars variable being passed immediately to the match..with construct is so common in F# that the _function_ keyword is used as a contraction, allowing the elimination of what is an unnecessary variable. In many cases, this construct allows for a one line function definition.

Most of the cases in the block matches a character which in turn maps to a token, which is appended to the accumulator (acc : Token list), and passed to a recursive call of tokenize’. This way the tokens are built up (in reverse order) as the string is traversed. This is seen in lines 22-27. Line 21 skips whitespace by calling tokenize’ without adding a token to the accumulator first.

Things are a bit more interesting at lines 28, 31, and 34. Line 28 detects the beginning of a string, and starts a new recursive thread with the _parseString_ function, the signature of which is (string –> char list –> string _ char list). So the accumulator is a string, the “work” is being done on a char list, to which is passed our source char list, and the return is a tuple of string _ char list, which is our parsed string and the rest of the source char list. One (of I’m sure **many**) optimizations that could be made is to use a mutable StringBuilder as the accumulator in parseString.

The _token_ function on line 12 does a similar job for non-string literals (numbers in this case, there are no bools in Json).

Lastly, line 37 handles the case where we run out of source characters. The Token list we’ve been accumulating is backwards; we’ve been “appending” to the front of the list all this time. This is a common pattern. At the end, we return the reverse of the list. When the Json at the top is put through the tokenizer, we get this:

```ruby
> let tk = tokenize source;;

val tk : Token list =
  [OpenBracket; String "glossary"; Colon; OpenBracket; String "title"; Colon;
   String "example glossary"; Comma; String "GlossDiv"; Colon; OpenBracket;
   String "title"; Colon; String "S"; Comma; String "GlossList"; Colon;
   OpenBracket; String "GlossEntry"; Colon; OpenBracket; String "ID"; Colon;
   String "SGML"; Comma; String "SortAs"; Colon; String "SGML"; Comma;
   String "GlossTerm"; Colon; String "Standard Generalized Markup Language";
   Comma; String "Acronym"; Colon; String "SGML"; Comma; String "Abbrev";
   Colon; String "ISO 8879:1986"; Comma; String "GlossDef"; Colon; OpenBracket;
   String "para"; Colon;
   String
     "A meta-markup language, used to create markup languages such as DocBook.";
   Comma; String "GlossSeeAlso"; Colon; OpenArray; String "GML"; Comma;
   String "XML"; CloseArray; CloseBracket; Comma; String "GlossSee"; Colon;
   String "markup"; CloseBracket; CloseBracket; CloseBracket; CloseBracket;
   CloseBracket]
```

This list of tokens is an intermediate step. From here, we could go in several directions. For my purposes, I wanted to see it as XML. To get there, I created a parser that takes the token list and returns an XElement.

```ruby
let parseToXml source =
let map = function
  | Token.Number(n) -> n.ToString()
  | Token.String(x) -> x
  | v -> failwith "Syntax Error, unrecognized token in map()"

let rec parseValue (acc:XElement) = function
  | OpenBracket :: t ->
    let newElement, t' = parseElement acc t
    newElement, t'
  | OpenArray :: t ->
    let name = acc.Name.LocalName
    if name.EndsWith("ies") && name.Length > 3 then
      let childName = name.Substring(0, name.Length - 3) + "y"
      let newListElement, t' = parseArray childName acc t
      newListElement, t'
    elif name.EndsWith("s") && name.Length > 1 then
      let childName = name.Substring(0, name.Length - 1)
      let newListElement, t' = parseArray childName acc t
      newListElement, t'
    else
      let childName = acc.Name.LocalName
      acc.Name <- XName.Get(childName + "s")
      let newListElement, t' = parseArray childName acc t
      newListElement, t'
  | h :: t ->
    acc.Value <- map(h)
    acc, t
  | _ -> failwith "bad value"

and parseArray name acc = function
  | Comma :: t -> parseArray name acc t
  | CloseArray :: t ->  acc, t
  | t ->
    let newElement = XElement(XName.Get(name))
    let acc', t' = parseValue newElement t
    acc.Add(acc')
    parseArray name acc t'

and parseElement (acc : XElement) = function
  | Comma :: t ->
    parseElement acc t
  | Token.String(n) :: Colon :: t ->
    let newElement = XElement(XName.Get(n))
    let v, t' = parseValue newElement t
    acc.Add(v)
    parseElement acc t'
  | CloseBracket :: t ->
    acc, t
  | _ -> failwith "Malformed JSON object"

let root = XElement(XName.Get("root"))
let tokens = tokenize source

match tokens with
  | OpenBracket :: t ->
    let result, t' = parseElement root t
    result
  | _ -> failwith "Json did not begin with an object"
'
```

There is still syntax checking happening in the parse step. The tokenizer would have found illegal tokens, but the parse function will find illegal Json structure. It works in a similar way to the tokenizer, except instead of a raw stream of characters, we’ve got a stream of tokens, which is examined one by one and passed to a set of recursive functions which accumulates the XML, in this case as an XElement.

parseToXml defines inner functions _map_, _parseValue_, _parseArray_, and _parseElement._ It then gets to work by creating a root XElement, and getting the Token list. Json should start with an OpenBracket token. Anything else and we failwith an error (like C#’s throw). When we find that OpenBracket, the recursion begins with a call to parseElement. The root XElement serves as the accumulator, and the Token list is the “work” to be done.

You should be able to see what’s happening if you’re somewhat comfortable with F#. The parseValue function does some extra work to wrap arrays in nodes where the singularity / plurality of the node makes it read a bit better (in English, most of the time). The other point of interest is that the accumulator is a mutable object in parseToXml, unlike the Token list was in tokenize. At lines 37 and 46, the newly created elements are added to the accumulator using the Add(XElement) method, which mutates the accumulator and keeps using it. This is a departure from “purer” functional techniques, but that’s what’s cool about F#, you can make those departures where it makes sense. In this case, leveraging the XElement class of the .NET framework was worth it. Consuming this class from C# is that much easier and more intuitive.

After all is said and done, this is the XML that is emitted.

```HTML
<root>
  <glossary>
    <title>example glossary</title>
    <GlossDiv>
      <title>S</title>
      <GlossList>
        <GlossEntry>
          <ID>SGML</ID>
          <SortAs>SGML</SortAs>
          <GlossTerm>Standard Generalized Markup Language</GlossTerm>
          <Acronym>SGML</Acronym>
          <Abbrev>ISO 8879:1986</Abbrev>
          <GlossDef>
            <para>A meta-markup language, used to create markup languages such as DocBook.</para>
            <GlossSeeAlsos>
              <GlossSeeAlso>GML</GlossSeeAlso>
              <GlossSeeAlso>XML</GlossSeeAlso>
            </GlossSeeAlsos>
          </GlossDef>
          <GlossSee>markup</GlossSee>
        </GlossEntry>
      </GlossList>
    </GlossDiv>
  </glossary>
</root>
```

It isn’t the most sophisticated, performant, or I’m sure accurate Json parser out there. But it does the trick for my immediate need, is a good starting point for more sophistication later if needed, and was an excellent exercise in parsing and F#. It was fun to write, and hopefully it stimulates some curiosities. Let me know if you’ve got any questions or comments.
