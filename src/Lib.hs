{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lib (mainapp) where

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"

import Control.Monad.IO.Class
import qualified Db
import Todo
import Web.Scotty
import Db (DbTodo (DbTodo))

-- TODO
mapToDto :: DbTodo -> Todo
mapToDto (DbTodo t desc d) = Todo t desc d

mainapp :: IO ()
mainapp = do
  -- Initialize our fake DB
  db <- Db.mkDb

  -- Run the scotty web app on port 8080
  scotty 8080 $ do

    get "/hello/:name" $ do
        yourname <- param "name"
        text ("hello " <> yourname <> "!")

    get "/todos/" $ do
        todos <- liftIO (Db.getTodos db)
        json (map mapToDto todos)

    put "/todos/toggle/:id" $ do
        todoId <- param "id"
        -- parse the request body into our CreateUserRequest type
        liftIO $ Db.finishTodo db todoId

    put "/todos/:id" $ do
        todoId <- param "id"
        -- parse the request body into our CreateUserRequest type
        editTodoReq <- jsonData
        liftIO $ editTodo db editTodoReq todoId
    -- Listen for POST requests on the "/users" endpoint
    post "/todos" $ do
        -- parse the request body into our CreateUserRequest type
        createTodoReq <- jsonData

        -- Create our new user.
        -- In order for this compile we need to use liftIO here to lift the IO from our
        -- createUser function. This is because the `post` function from scotty expects an
        -- ActionM action instead of an IO action
        newTodoId <- liftIO $ createTodo db createTodoReq

        -- Return the user ID of the new user in the HTTP response
        json newTodoId

    -- Listen for DELETE requests on the "/users/:userId" endpoint
    delete "/todos/:id" $ do
      -- Get the value of the userId from the URL
      todoId <- param "id"

      -- Delete the relevant user
      -- Same as with the user creation, we need to use liftIO here.
      liftIO $ Db.deleteTodo db todoId

-- Our createUser function simply deals with constructing a DbUsr value and passes it
-- to the Db.insertUser function
createTodo :: Db.TodoStore -> CreateUpdateTodoRequest -> IO Int
createTodo db CreateUpdateTodoRequest {reqTodoName = tname, reqTodoDescription = tdesc} = Db.insertTodo db dbtodo
  where
    dbtodo = Db.DbTodo {Db.dbTodoName = tname, Db.dbTodoDescription = tdesc, Db.dbTodoDone = False}

editTodo :: Db.TodoStore -> CreateUpdateTodoRequest -> Int -> IO ()
editTodo db CreateUpdateTodoRequest {reqTodoName = tname, reqTodoDescription = tdesc} uid = Db.editTodo db uid dbtodo
  where
    dbtodo = (tname, tdesc)