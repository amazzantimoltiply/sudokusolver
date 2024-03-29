
main = do x <- getChar
          y <- getChar
          putChar x
          putChar y
          return (x,y)