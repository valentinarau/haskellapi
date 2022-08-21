-- Db.hs
module Db
  ( DbTodo (..),
    getTodoStore,
    insertTodo,
    deleteTodo,
    editTodo,
    getTodos,
    finishTodo,
    mkDb,
    TodoStore (..),
  )
where


import Data.IORef
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

data DbTodo = DbTodo
  { 
    dbTodoName :: Text,
    dbTodoDescription :: Text,
    dbTodoDone :: Bool
  }
  deriving (Show)

newtype TodoStore = TodoStore {unTodoStore :: IORef (Map Int DbTodo)}

-- Creates our initial empty database
mkDb :: IO TodoStore
mkDb = do
  ref <- newIORef (Map.empty :: Map Int DbTodo)
  pure $ TodoStore ref

-- Accepts a default value to return in case the list is empty
safeLast :: a -> [a] -> a
safeLast x [] = x
safeLast _ (x : xs) = safeLast x xs

-- Utility to allow us to read the data in our "database"
getTodoStore :: TodoStore -> IO (Map Int DbTodo)
getTodoStore (TodoStore store) = readIORef store

-- VERY naive utility to get the next unique user ID
getNextId :: TodoStore -> IO Int
getNextId x = (+ 1) . safeLast 0 . sort . Map.keys <$> getTodoStore x

getTodos :: TodoStore -> IO [DbTodo]
getTodos todoStore = do
 Map.elems <$> getTodoStore todoStore
-- insertUser uses getNextId to get the next ID and then updates our database using
-- modifyIORef from the Data.IORef library. It returns the new ID as a result.
insertTodo :: TodoStore -> DbTodo -> IO Int
insertTodo todoStore td = do
  nextId <- getNextId todoStore
  modifyIORef (unTodoStore todoStore) (Map.insert nextId td)
  pure nextId

-- deleteUser updates our database by deleting the relevant user data 
deleteTodo :: TodoStore -> Int -> IO ()
deleteTodo usrStore uid = modifyIORef' (unTodoStore usrStore) (Map.delete uid)

editTodo :: TodoStore -> Int -> (Text, Text) -> IO ()
editTodo todoStore uid (newName, newDesc) = 
    modifyIORef' (unTodoStore todoStore) (Map.adjust (\todo -> todo {dbTodoName = newName, dbTodoDescription = newDesc}) uid)

finishTodo :: TodoStore -> Int -> IO ()
finishTodo todoStore uid = modifyIORef' (unTodoStore todoStore) (Map.adjust (\todo -> todo {dbTodoDone = not $ dbTodoDone todo}) uid)