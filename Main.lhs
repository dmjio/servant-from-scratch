> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE DerivingStrategies   #-}
> {-# LANGUAGE OverloadedStrings    #-}
> {-# LANGUAGE DeriveAnyClass       #-}
> {-# LANGUAGE TypeOperators        #-}
> {-# LANGUAGE TypeApplications     #-}
> {-# LANGUAGE ScopedTypeVariables  #-}
> {-# LANGUAGE LambdaCase           #-}
> {-# LANGUAGE RecordWildCards      #-}
> {-# LANGUAGE DataKinds            #-}
> {-# LANGUAGE PolyKinds            #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE FlexibleInstances    #-}
> {-# LANGUAGE DeriveGeneric        #-}
> ------------------------------------
> -- | Talk: Servant-from-scratch
> --   Date: May 20, 2020
> --   Description:
> --     A pedagogical implementation of servant-client.
> --     This talk describes DSLs, generic programming,
> --     Haskell extensions, servant's grammar and
> --     a simpler implementation of servant-client.
> -----------------------------------
> module Main where

> import           Control.Monad
> import           Data.Aeson
> import           Data.Aeson.Encode.Pretty
> import qualified Data.ByteString.Char8      as B8
> import qualified Data.ByteString.Lazy.Char8 as BL8
> import           Data.List
> import           Data.Proxy
> import           GHC.Generics
> import           GHC.TypeLits
> import           Network.HTTP.Client        hiding (Proxy, path)
> import           Network.HTTP.Client.TLS
> import           Network.HTTP.Types.Status

Nice to see where we're going before we get there.
Our goal is to build the following http(s)-client using 'servant' from scratch.

> main :: IO ()
> main = do
>  -- run $ getEcho (Just "bar") (Just "quux")
>  run (postEcho alice)
>  -- run (postEcho bob)

> --  ### What is servant?

Servant is a collection of libraries used for web programming (created by Haskell devs in Singapore).
The 'core' servant package is 'servant', and all other packages depend on servant to implement their functionality.

Servant can be described as an extensible embedded domain specific language. This just means it isn't a full language (i.e. like Haskell / Purescript), but a domain-specific one. "Embedded" means this langauge exists inside the host language (Haskell in this case). Furthermore, it is embedded at the type-level. This might seem like an odd choice because there is not as much (albeit still a lot) expressive power at the type-level, as the value level. Haskell is not the best language for type-level programming (see Idris, Agda, Coq), but Haskell is the most practical functional programming language for real-world work, and therefore learning the type-level facilities it provides to us can be very beneficial for day-to-day work. In summary, servant is an embedded domain specific langauge, where the domain is Web programming.

> -- ### Why is Servant at the type-level?

  - 1) We can encode invariants about what our web server should do, what kinds of serialization constraints should be present on our business logic.
  - 2) Can throw compile errors when something is wrong (as opposed to runtime errors).
  - 3) Let the compiler generate the code for us (form or meta-programming)

> -- ### Combinators

  - Allows us to combine terms in our grammar

> -- ### Alternative

-- Used in servant to demarcate different routes
-- "GET /api/cats" :<|> "POST /api/cats"

> data left :<|> right = left :<|> right
> infixr 4 :<|>

> -- ### Sub

-- Used in servant to string terms together. Combinators.
-- "api" :> "cats" :> Get '[JSON] Value
-- "api" :> "cats" :> QueryParam "ishappy" Bool :> Get '[JSON] Value

> data (a :: k) :> b
> infixr 4 :>

> -- ### Content-Types

-- Notice only a single type, no value-level. Yet still useful to us.

> data JSON

> -- ### Method

-- Used at both type-level and value-level thanks to promotion from DataKinds
--  https://www.seas.upenn.edu/~sweirich/papers/tldi12.pdf
--
-- GET is a value, but also promoted as a type. Thanks to DataKinds
--
-- λ> :type GET -- type of the value
-- GET :: Method
--
-- λ> :kind 'GET -- type of the type
-- 'GET :: Method

> data Method
>   = GET
>   | PUT
>   | POST
>   | DELETE
>   | PATCH
>   deriving (Show, Eq)

> -- ### Capture

-- Allows variable substitution in a path fragment
-- GET /api/dog/{name}
-- Capture "name" String

> data Capture
>  (name :: Symbol)
>  (capType :: a)

> -- ### QueryParam

-- GET /api/dog?ishappy=true

> data QueryParam
>  (name :: Symbol)
>  (qType :: a)

> -- # ReqBody

-- POST /api/dog '{ \"type\" : \"Dachshund\", \"name\" : \"Rex\" }'

> data ReqBody
>   (ctypes :: [*])
>   (reqBodyType :: *)

> -- # Verb

-- POST /api/dog '{ \"type\" : \"Dachshund\", \"name\" : \"Rex\" }'

> data Verb
>  (method :: Method)
>  (ctypes :: [*])
>  (returnType :: a)

> -- # Type synonyms

> type Get  = Verb 'GET
> type Post = Verb 'POST
> type Put  = Verb 'PUT

> type DogAPI = "dog" :> Get '[JSON] Value
> type CatAPI = "cat" :> Get '[JSON] Value
> type PetAPI = "api" :> DogAPI :<|> CatAPI

> -- # Type families (aka. functions at the type-level)

-- Type level-functions, similar to value level functions
-- See singletons. Package for re-implementing the Prelude.

-- Is a list null?

> nul :: [a] -> Bool
> nul [] = True
> nul (_:_)  = False

-- | Is a type-level list nul?
-- https://www.seas.upenn.edu/~sweirich/papers/haskell12.pdf
-- Richard Eisenberg, spearheading dependent types in GHC Haskell
-- Wrote singletons, admits its a "hack".

-- | Nul is a 'closed' type family, which means it cannot be extended
-- Open type families exist, which allow you to make new instances across modules.
-- Haskell stripe library uses this to implement conditional parameters

> type family Nul (xs :: [k]) :: Bool where
>   Nul '[] = 'True
>   Nul (x ': xs) = 'False

-- | Type-level concat function

> type family a ++ b :: [k] where
>  a ++ '[] = a
>  '[] ++ b = b
>  (x ': xs) ++ b = x ': xs ++ b

-- | Example Web API

> type API = "api" :> "foo" :> Get '[JSON] Bool
>      :<|> "api" :> "lol" :> Get '[JSON] Bool

-- | Type function for extracting path fragments from our servant grammar

> type family Segments (xs :: k) :: [Symbol] where
>   Segments (Verb a b c) = '[]
>   Segments (left :<|> right) = Segments left ++ Segments right
>   Segments (sym :> b) = sym ': Segments b

-- | Exercise: Extract segments from API
-- λ> :kind! Segments API
-- Segments API :: [Symbol]
-- = '["api", "foo", "api", "lol"]

-- | Example of typeclass "induction"
-- Where base case is specified, along with the recursive-step.

> class ReifySymbols (xs :: [Symbol]) where
>   reifySymbols :: Proxy xs -> [String]

-- | Base case (similar to how base case is written in most haskell functions (see nul above)

> instance ReifySymbols '[] where
>   reifySymbols Proxy = []

-- | Recursive step, pop-off some information from the type-level, recurse one step closer to the base case

> instance (ReifySymbols xs, KnownSymbol sym) => ReifySymbols (sym ': xs) where
>   reifySymbols Proxy = symbolVal (Proxy @ sym) : reifySymbols (Proxy @ xs)

> -- symbolVal (Proxy @ sym) : symbolVal (Proxy @ sym) : symbolVal (Proxy @ sym) : []
> -- "foo" : "bar" : "quuz" : []
> -- ["foo","bar","quuz"]

-- | Apply type level function, reify resultant type as value
-- In this case fetch all the path fragments as values.

> segmentsAsValues :: [String]
> segmentsAsValues = reifySymbols (Proxy @ (Segments API))

-- | GHC.Generics
-- Andres Loh, author of generics-sop, helped pioneer Generics in GHC.
-- https://www.andres-loeh.de/ExploringGH.pdf

> data Person = Person
>   { name :: String
>   , age  :: Int
>   } deriving stock (Show, Eq, Generic)
>     deriving anyclass (ToJSON)

-- | Show :kind! Person
-- recordFields :: Generic a => a -> [String]

> type family RecordFields (xs :: k) :: [Symbol] where
>   RecordFields (a :*: b) = RecordFields a ++ RecordFields b
>   RecordFields (a :+: b) = RecordFields a ++ RecordFields b
>   RecordFields (S1 ('MetaSel ('Just s) _ _ _) _) = '[s]
>   RecordFields (M1 i c p) = RecordFields p

> personFields :: [String]
> personFields = reifySymbols (Proxy @ (RecordFields (Rep Person)))

-- λ> personFields
-- ["name","age"]

-- Interpret the types as values.
-- Spiritual equivalent of GHC.Generics
-- Combine both of the tools above (type families and type classes)
-- to interpret our grammar as an Http client.

> data Req
>   = Req
>   { path :: [String]
>   , verb :: Method
>   , contentType :: String
>   , reqBody :: Maybe BL8.ByteString
>   , qParams :: [(String, Maybe String)]
>   } deriving (Show, Eq)
>
> data Scheme = HTTP | HTTPS
>   deriving (Show, Eq)
>
> data BaseUrl
>   = BaseUrl
>   { baseUrl :: String
>   , basePort :: Int
>   , baseScheme :: Scheme
>   } deriving (Show, Eq)
>
> defReq :: Req
> defReq = Req mempty GET "*/*" Nothing []

-- | This class allows us to reify types as values
-- Embedding a type family in the class is how we
-- can perform a form of type-safe code-generation

> class HasClient api where
>   type ToClient api
>    -- ^ Called an associated type family, means this type family is associated to this class
>    -- We had previously mentioned closed-type families, at compile-time GHC lifts associated type families out of the scope of the class, so really there are only two version of type families.
>
>   client :: Proxy api -> BaseUrl -> ToClient api
>   client p b = clientWith p b defReq
>   -- ^ client is defined in terms of clientWith
>
>   clientWith :: Proxy api -> BaseUrl -> Req -> ToClient api
>   -- ^ have to implement this
>
> -- "foo" :> ...
> instance (KnownSymbol sym, HasClient api) => HasClient (sym :> api) where
>   type ToClient (sym :> api) = ToClient api
>   clientWith Proxy b req =
>     clientWith (Proxy @ api) b req { path = path req ++ [nextPath] }
>       where
>         nextPath :: String
>         nextPath = symbolVal (Proxy @ sym)
>
> instance (Show a, KnownSymbol sym, HasClient api) => HasClient (Capture sym a :> api) where
>   type ToClient (Capture sym a :> api) = a -> ToClient api
>   clientWith Proxy b req x =
>     clientWith (Proxy @ api) b req {
>       path = path req ++ [show x]
>     }
>
> instance (KnownSymbol sym, HasClient api) => HasClient (QueryParam sym String :> api) where
>   type ToClient (QueryParam sym String :> api) = Maybe String -> ToClient api
>   clientWith Proxy b req mb =
>     clientWith (Proxy @ api) b req {
>       qParams = (k, mb) : qParams req
>     } where
>         k = symbolVal (Proxy @ sym)
>
> instance (ToJSON a, HasClient api) => HasClient (ReqBody '[JSON] a :> api) where
>   type ToClient (ReqBody '[JSON] a :> api) = a -> ToClient api
>   clientWith Proxy b req x =
>     clientWith (Proxy @ api) b req {
>       reqBody = Just (encode x)
>     }
>
> instance (HasClient l, HasClient r) => HasClient (l :<|> r) where
>   type ToClient (l :<|> r) = ToClient l :<|> ToClient r
>   clientWith Proxy b req =
>     clientWith (Proxy @ l) b req :<|>
>       clientWith (Proxy @ r) b req

-- | Acts as a way to lower type-level terms to value level terms.
-- DataKinds allows us to promote Haskell values as types
-- DataKinds allows us to promote Haskell types as Kinds
-- We can go up, but can we come back down?
-- Yes, but we have to write that logic oursleves
-- Example of type-level "Demotion"

> class ToMethod (m :: Method) where
>  toMethod :: Proxy m -> Method

> instance ToMethod 'GET where toMethod Proxy = GET
> instance ToMethod 'POST where toMethod Proxy = POST
>
> debug :: Bool
> debug = True
>
> instance (ToMethod method, FromJSON a) => HasClient (Verb method '[JSON] a) where
>   type ToClient (Verb method '[JSON] a) = IO (Either Error a)
>   clientWith Proxy BaseUrl {..} Req {..} = do
>     initReq <- parseRequest url
>     let request =
>           initReq
>           { method = B8.pack $ show $ toMethod (Proxy @ method)
>           , requestBody = maybe (requestBody initReq) RequestBodyLBS reqBody
>           , queryString = B8.pack $
>               [ '?' | not (null qParams) ] <> intercalate "&"
>                 [ k <> "=" <> v
>                 | (k, Just v) <- qParams
>                 ]
>           , secure = baseScheme == HTTPS
>           , requestHeaders = requestHeaders initReq <>
>               [ ("Content-Type", "application/json") ]
>           }
>     when debug (print request)
>     manager <- getGlobalManager
>     response <- httpLbs request manager
>     let body = responseBody response
>     case statusCode (responseStatus response) of
>       x | x > 400 && x < 500 ->
>           pure
>             $ Left
>             $ RequestError x body
>         | x >= 500 ->
>           pure
>             $ Left
>             $ ServerError x body
>         | otherwise ->
>             case eitherDecode @a body of
>               Left e -> do
>                 print body
>                 pure $ Left (DecodeFailure x e)
>               Right r -> pure (Right r)
>     where
>       url =
>         mconcat
>         [ if baseScheme == HTTP
>             then "http://"
>             else "https://"
>         , baseUrl
>         , ":"
>         , show basePort
>         , "/"
>         , intercalate "/" path
>         ]
> data Error
>   = ServerError Int BL8.ByteString
>   | RequestError Int BL8.ByteString
>   | DecodeFailure Int String
>   | ConnFailure String
>   deriving (Show, Eq)

> -- # Bringing it all together

> type GetAPI =
>   "get"
>     :> QueryParam "foo" String
>     :> QueryParam "bar" String
>     :> QueryParam "quuz" String
>     :> Get '[JSON] Value
>
> bob, alice :: Person
> bob = Person "Bob" 55
> alice = Person "Alice" 55
>
> type PostAPI =
>   "post"
>     :> ReqBody '[JSON] Person
>     :> Post '[JSON] Value
>
> type FullAPI = GetAPI :<|> PostAPI
>
> run :: IO (Either Error Value) -> IO ()
> run f = f >>= \case
>   Left e ->
>     print e
>   Right r ->
>     BL8.putStrLn (encodePretty r)
>
> postEcho :: Person -> IO (Either Error Value)
> getEcho :: Maybe String -> Maybe String -> Maybe String -> IO (Either Error Value)

-- | Types defined above, this is made possible via the type-family in HasClient
-- > type ToClient (l :<|> r) = ToClient l :<|> ToClient r

> getEcho :<|> postEcho = client (Proxy @ FullAPI) postmanEcho

> postmanEcho :: BaseUrl
> postmanEcho = BaseUrl "postman-echo.com" 443 HTTPS

-- Summary

-- In this talk we discussed how an embedded domain specific language is a special purpose
-- limited language with terms that are used to describe a particular domain, along with a way to interpret this language into objects that suit the domain. Servant is one such domain specific language for web programming. We discussed how servant is similar in spirit to GHC.Generics, where GHC.Generics can be thought of as a domain specific language for reifying objects from Haskell records. We showed how we can use the same type-level induction and reification techniques to extract information from a Haskell record's metadata at compile time. We discussed various type-level extensions like PolyKinds, DataKinds, TypeOperators (all of which servant would not be possible without). We overviewied type level symbols and naturals that are enabled via the DataKinds extension. We showed the inner workings of a real servant interpretation for a subset of the servant grammar that actually works on a real world API. After this talk you should be more equipped to deal with type level programming in GHC and be an informed user of the Haskell servant library.

