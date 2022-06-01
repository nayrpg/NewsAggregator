module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (href, type_, id, for, value, checked, style)
import Html.Events exposing (onInput, onSubmit, onClick)
import Debug as Debug
import Ports
import Json.Encode as Encode
import Json.Decode as Decode

main =
  Browser.element { init = init, update = update, view = view, subscriptions=subscriptions}

-- TODO Implement timestamps
type alias Article =
  {
    name : String,
    url : String,
    timestamp : Int,
    pinned : Bool,
    read : Bool,
    id : Int
  }

type alias Model = {
    articles : List Article,
    new_article : Article,
    next_id : Int
  }

subscriptions : Model -> Sub Msg
subscriptions _ =
  Ports.loadState LoadState

-- TODO: Implement loading from disk  
init : () -> (Model, Cmd Msg)
init _ =
  ({
    articles = [],
    new_article = newBlankArticle,
    next_id = 1
  }, Cmd.none)



view : Model -> Html Msg
view model =
  -- TODO: Use loaded from disk 
  let
    (read, unread) = filterRead model.articles
    (pinned, unpinned) = filterPinned unread
  in
  -- TODO: style the app
  div [style "background" "#050f2f", style "color" "#e6b65e", style "font-family" "Verdana, sans-serif", style "padding" "20px"] [
    div [] [
      h2 [] [text  "New"],
      form [onSubmit SubmitNewArticle] [
        label [for "new_name"] [text "Name :"],
        input [type_ "text", id "new_name", value model.new_article.name, onInput NameEntered] [],
        label [for "new_url"] [text "URL :"],
        input [type_ "text", id "new_url", value model.new_article.url, onInput URLEntered] [],
        input [type_ "submit"] [ text "Submit" ]
      ],
      ul [] (articleListToView [model.new_article])
      
    ],
    div [] [
      h3 [] [text  "Pinned"],
      ul[] (articleListToView pinned)
    ],
    div [] [
      h2 [] [text  "Feed"],
      ul[] (articleListToView unpinned)
    ],
    div [] [
      h3 [] [text  "Already Read"],
      ul[] (articleListToView read)
    ]
  ]

articleListToView : List Article -> List (Html Msg)
articleListToView article_list = 
  article_list
    |> List.map (\article -> li [] [
                    h2[][text (article.name)],
                    a [href article.url] [text article.url],
                    br [] [],
                    -- TODO: Implement marking as read
                    label [for "read"] [text "Read: "],
                    input [type_ "checkbox", id "read", onClick (ToggleRead article.id), checked article.read] [],
                    -- TODO: Implement marking as pinned
                    label [for "pinned"] [text "Pinned: "],
                    input [type_ "checkbox", id "pinned", onClick (TogglePinned article.id), checked article.pinned] [],
                    button [style "font-size" "24px", onClick (DeleteArticle article.id)] [ text "🗑" ]
                  ]
                )

-- TODO: Implement loading from storage  
-- loadFromDisk =
--   articles = localStorage["link_locker_articles"]

-- TODO: Implement writing back to disk  
-- writeToDisk =
--   localStorage["link_locker_articles"] = articles

filterRead : List Article -> (List Article, List Article)
filterRead article_list =
  article_list
    |> List.partition (\article -> article.read)

filterPinned : List Article -> (List Article, List Article)
filterPinned article_list =
  article_list
    |> List.partition (\article -> article.pinned)

type Msg =
  NameEntered String |
  URLEntered String |
  SubmitNewArticle |
  TogglePinned Int |
  ToggleRead Int |
  LoadState String |
  DeleteArticle Int

  -- PinnedChange Bool |
  -- ReadChange Bool
articleEncoder : Article -> Encode.Value
articleEncoder article = 
    Encode.object
      [
        ( "name", Encode.string article.name ),
        ( "url", Encode.string article.url ),
        ( "timestamp", Encode.int article.timestamp ),
        ( "pinned", Encode.bool article.pinned ),
        ( "read", Encode.bool article.read ),
        ( "id", Encode.int article.id )
      ]

articleDecoder : Decode.Decoder Article
articleDecoder =
  Decode.map6 Article ( Decode.field "name" Decode.string ) ( Decode.field "url" Decode.string ) ( Decode.field "timestamp" Decode.int )  ( Decode.field "pinned" Decode.bool ) ( Decode.field "read" Decode.bool ) ( Decode.field "id" Decode.int )

type alias State = {
    articles : List Article,
    next_id : Int
  }
stateDecoder : Decode.Decoder State
stateDecoder =
  Decode.map2 State ( Decode.field "articles" (Decode.list articleDecoder) ) ( Decode.field "next_id" Decode.int )

saveState : Model -> Cmd msg
saveState model =
  Encode.object [ ("articles", Encode.list articleEncoder model.articles),
                  ("next_id", Encode.int model.next_id) ]
    |> Encode.encode 0
    |> Ports.storeState

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    new_article = model.new_article
  in
  case msg of
    NameEntered name ->
      ({ model | new_article = { new_article | name = name} }, Cmd.none)
    URLEntered url ->
      ({ model | new_article = { new_article | url = url} }, Cmd.none)
    SubmitNewArticle ->
      let
        new_next_id = model.next_id + 1
        new_articles = [{new_article | id = model.next_id}] ++ model.articles
        new_model = {
            model |
              new_article = newBlankArticle,
              articles = new_articles,
              next_id = new_next_id
          }
      in
      (new_model, saveState new_model)
    TogglePinned id ->
      let
        updated_articles = model.articles
          |> List.map (\article -> 
            if article.id == id then 
              { article | pinned = not article.pinned }
            else 
              article
          )
        new_model = {
            model | articles = updated_articles
          }
      in
      (new_model, saveState new_model)
    ToggleRead id ->
      let
        updated_articles = model.articles
          |> List.map (\article -> 
            if article.id == id then 
              { article | read = not article.read }
            else 
              article
          )
        new_model = {
            model | articles = updated_articles
          }
      in
      (new_model, saveState new_model)
    LoadState articles_str ->
      let
        result = Decode.decodeString stateDecoder (Debug.log "loaded_json" articles_str)
      in
      case Debug.log "result" result of
        Err _ ->
          ({ model | articles = [] }, Cmd.none) 
        Ok loaded_state ->
          ({ 
            model |
              articles = loaded_state.articles,
              next_id = loaded_state.next_id
          }, Cmd.none) 
    DeleteArticle id ->
      let
        updated_articles = model.articles
          |> List.filter (\article -> article.id /= id)
        new_model = {
            model | articles = updated_articles
          }
      in
      (new_model, saveState new_model)
    --TODO: decode JSON
newBlankArticle : Article
newBlankArticle =
  {name="", url="", timestamp=0, pinned=False, read=False, id = 0}
