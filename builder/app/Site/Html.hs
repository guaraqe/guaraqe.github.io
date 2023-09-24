{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Html
  ( addClasses,
  )
where

import Text.Blaze.Html
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Internal
import Text.Blaze.Renderer.String (fromChoiceString)

addClasses :: Html -> Html
addClasses = recurse addClasses'

addClasses' :: MarkupM a -> MarkupM a
addClasses' html =
  case html of
    Parent tag _ _ _ ->
      case getText tag of
        "a" -> html ! A.class_ "link link-hover text-zinc-600"
        "ul" -> html ! A.class_ "list-disc list-outside mt-1 mb-3 pl-5"
        "ol" -> html ! A.class_ "list-decimal list-outside mt-1 mb-3 pl-5"
        "h1" -> html ! A.class_ "text-2xl mt-5 mb-5"
        "h2" -> html ! A.class_ "text-xl mt-5 mb-5"
        "h3" -> html ! A.class_ "text-l mt-5 mb-5"
        "p" -> html ! A.class_ "mb-3"
        "hr" -> html ! A.class_ "mb-3"
        "table" -> html ! A.class_ "table border-collapse mb-3"
        "th" -> html ! A.class_ "border"
        "td" -> html ! A.class_ "border"
        _ -> html
    Leaf tag _ _ _ ->
      case getText tag of
        "img" -> html ! A.class_ "mx-auto"
        _ -> html
    AddAttribute key key' val html' ->
      case getText key of
        "class" ->
          case fromChoiceString val "" of
            "math display" -> AddAttribute key key' (val <> " notranslate") html'
            "math inline" -> AddAttribute key key' (val <> " notranslate") html'
            "sourceCode" -> AddAttribute key key' (val <> " mb-3 notranslate") html'
            _ -> html
        _ -> html
    _ -> html

recurse :: (forall x. MarkupM x -> MarkupM x) -> (MarkupM a -> MarkupM a)
recurse f = \case
  Parent tag otag ctag m -> f (Parent tag otag ctag (recurse f m))
  CustomParent c m -> f (CustomParent c (recurse f m))
  Leaf tag otag ctag x -> f (Leaf tag otag ctag x)
  CustomLeaf c b x -> f (CustomLeaf c b x)
  Content c x -> f (Content c x)
  Comment c x -> f (Comment c x)
  Append m1 m2 -> f (Append (recurse f m1) (recurse f m2))
  AddAttribute rk k v m -> f (AddAttribute rk k v (recurse f m))
  AddCustomAttribute c1 c2 m -> f (AddCustomAttribute c1 c2 (recurse f m))
  Empty x -> Empty x
