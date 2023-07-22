module Types
where


newtype MessageQueue = MessageQueue [String] deriving Show 


emptyMessageQueue :: MessageQueue
emptyMessageQueue = MessageQueue []

enqueueMessage :: String -> MessageQueue -> MessageQueue
enqueueMessage msg (MessageQueue msgs) = MessageQueue (msgs ++ [msg])

nextMessage :: MessageQueue -> (String, MessageQueue)
nextMessage (MessageQueue queue) = (head queue, MessageQueue (tail queue))