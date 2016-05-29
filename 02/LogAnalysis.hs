-- {-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log


-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage message = case words message of
  ("I" : timestamp : xs) -> LogMessage Info (read timestamp) (unwords xs)
  ("W" : timestamp : xs) -> LogMessage Warning (read timestamp) (unwords xs)
  ("E" : level : timestamp : xs) -> LogMessage (Error $ read level) (read timestamp) (unwords xs)
  msg -> Unknown (unwords msg)

parse :: String -> [LogMessage]
parse messages = parseMessage <$> lines messages

exercise1 = do
  print $ parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
  print $ parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
  print $ parseMessage "This is not in the right format" == Unknown "This is not in the right format"
  print =<< testParse parse 5 "error.log"


-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert x Leaf = Node Leaf x Leaf
insert (Unknown _) tree = tree
insert msg1@(LogMessage _ timestamp1 _) tree@(Node left msg2@(LogMessage _ timestamp2 _) right)
  | timestamp1 == timestamp2 = tree
  | timestamp1 < timestamp2  = Node (insert msg1 left) msg2 right
  | timestamp1 > timestamp2  = Node left msg2 (insert msg1 right)


-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right


-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getErrorMessage . inOrder . build . filter isMajorError
  where isMajorError (LogMessage (Error s) _ _) = s >= 50
        isMajorError _ = False
        getErrorMessage (LogMessage _ _ msg) = msg


main = do
  exercise1
  print =<< testWhatWentWrong parse whatWentWrong "error.log"
