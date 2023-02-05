---
title: Switching to Hexo
date: 2022-07-28 15:15:28
mathjax: true
categories:
  - [cat1]
tags:
  - Hexo
  - Blogging
---

I'm switching blog site generators to [Hexo](https://hexo.io). We'll see how it goes...

## Testing Math Rendering

### Syntax of $\Huge\mathcal{A}$

Way 2 of 3, BNF style

$$
\begin{gather}
\frac{\mathcal{i} \in \mathbb{Z}}
{\mathcal{i} \in \mathcal{A}}
\tag{1} \\ \\
\frac{\mathcal{e} \in \mathcal{A}}
{Pred(\mathcal{e}) \in \mathcal{A}}
\tag{2} \\ \\
\frac{\mathcal{e} \in \mathcal{A}}
{Succ(\mathcal{e}) \in \mathcal{A}}
\tag{3} \\ \\
\frac
{\mathcal{e}_1 \in\mathcal{A} \qquad\mathcal{e}_2 \in \mathcal{A}}
{Plus(\mathcal{e}_1, \mathcal{e}_2) \in \mathcal{A}}
\tag{4}\\ \\
\frac
{\mathcal{e}_1 \in \mathcal{A} \qquad \mathcal{e}_2 \in \mathcal{A}}
{Mult(\mathcal{e}_1, \mathcal{e}_2) \in \mathcal{A}}
\tag{5}
\end{gather}
$$

Some more mathyness

This is inline math: $\left( \sum*{k=1}^n a_k b_k \right)^2 \leq \left( \sum*{k=1}^n a*k^2 \right) \left( \sum*{k=1}^n b_k^2 \right)$ - is that what you're looking for?

And another little bit of what should be standalone latex:

$$
i\hbar\frac{\partial}{\partial t}\psi=-\frac{\hbar^2}{2m}\nabla^2\psi+V\psi
$$

how's that look?

$$\Huge\boxed{\Downarrow \> \subseteq \mathcal{A} \times \mathbb{Z}}$$

Natural Semantics

## Here's some Haskell Code

Just want to see how code formatting works with Hexo ... this is a block of Haskell code. Does it have syntax highlighting?

```haskell Haskell Parser
-- | parens - extends a parser to perform it's parse of a sequence of characters
-- contained in parenthesis
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser Text
identifier = do
  v <- (:) <$> letterChar <*> many alphaNumChar <?> "identifier"
  pure $ T.pack v

var :: Parser Term
var = do
  v <- lexeme identifier
  asks (getLocalIdx v) >>= \case
    Just n  -> pure $ Var n
    Nothing -> asks (getGlobalTerm v) >>= \case
      Just t  -> pure t
      Nothing -> fail $ "no local or global variable named " <> T.unpack v

lambda :: Parser Term
lambda = do
  choice [char '\\', char '/']
  ws
  boundVar <- lexeme identifier <?> "identifier"
  char '.'
  ws
  -- @local (insertLocal boundVar)@ adds @boundVar@ to the list of
  -- locals from this point forward
  body <- local (insertLocal boundVar) term
  pure $ Lam boundVar body
```

$$
$$
