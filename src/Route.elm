module Route exposing (Route(..), fromUrl)

import Url exposing (Url)

type Route
    = Dashboard
    | NotFound

fromUrl : Url -> Route
fromUrl url = 
    case url.path of 
        "/" -> Dashboard
        _ -> NotFound
