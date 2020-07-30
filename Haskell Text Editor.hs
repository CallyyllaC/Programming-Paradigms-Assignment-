--Import requirements
import Data.List
import System.IO
import System.Directory
import System.Environment
import qualified Control.Exception as E

--Create text editor data structure
data TextEditor = TextEditor([Char], [Char], [Char], [Char]) deriving (Show)

--main
main = do
   --Welcome message
   putStrLn "Welcome to the haskell text editor"
   --if a file exists, load it, if not skip
   fileExists <- doesFileExist "file.txt"
   if fileExists
   then (do
      handle <- openFile "file.txt" ReadMode
      contents <- hGetContents handle
      --load data into t
      let t = TextEditor(contents, [], [], [])
      putStrLn ("Text has been loaded: "++contents)
      hClose handle)
   else (putStrLn "No file has been loaded")

--Function to highlight all of the text
highlightAll:: TextEditor -> TextEditor
highlightAll(TextEditor(x, h, y, c)) = (TextEditor([], x++h++y, [], c))

--Function to move the cursor left
moveLeft:: TextEditor -> TextEditor
moveLeft(TextEditor([], h, y, c)) = (TextEditor([], h, y, c)) --if far left dont try going further left
moveLeft(TextEditor(x, h, y, c)) = (TextEditor((reverse(tail(reverse x))), [], [head(reverse x)]++h++y, c))

--Function to move to cursor right
moveRight:: TextEditor -> TextEditor
moveRight(TextEditor(x, h, [], c)) = (TextEditor(x, h, [], c)) --if far right dont try going further right
moveRight(TextEditor(x, h, y, c)) = (TextEditor(x++h++[head(y)], [], tail(y), c))

--Function to jump one word left (recursive)
jumpLeft :: TextEditor -> TextEditor
jumpLeft(TextEditor([], h, y, c)) = (TextEditor([], h, y, c)) --if far left dont try going further left
jumpLeft(TextEditor(x, h, y, c)) = (
   if head (reverse x) == ' ' && head (h++y) /= ' '
   then TextEditor(x, h, y, c)
   else jumpLeft(moveLeft(TextEditor(x, h, y, c))))

--Function to jump one word right (recursive)
jumpRight :: TextEditor -> TextEditor
jumpRight(TextEditor(x, h, [], c)) = (TextEditor(x, h, [], c)) --if far right dont try going further right
jumpRight(TextEditor(x, h, y, c)) = (
   if head (reverse (x++h)) == ' ' && head y /= ' '
   then TextEditor(x, h, y, c)
   else jumpRight(moveRight(TextEditor(x, h, y, c))))

--Function to highlight the character on the left
highlightLeft :: TextEditor -> TextEditor
highlightLeft(TextEditor([], h, y, c)) = (TextEditor([], h, y, c)) --if far left dont try going further left
highlightLeft(TextEditor(x, h, y, c)) = (TextEditor(reverse(tail(reverse x)),([head(reverse x)]), y, c))

--Function to highlight the character on the right
highlightRight :: TextEditor -> TextEditor
highlightRight(TextEditor(x, h, [], c)) = (TextEditor(x, h, [], c)) --if far right dont try going further right
highlightRight(TextEditor(x, h, y, c)) = (TextEditor(x, ([head y]), (tail y), c))

--Function to highlight the word on the left (recursive)
jumpHighlightLeft :: TextEditor -> TextEditor
jumpHighlightLeft(TextEditor([], h, y, c)) = (TextEditor([], h, y, c)) --if far left dont try going further left
jumpHighlightLeft(TextEditor(x, h, y, c)) = (
   if (head(reverse x)) == ' '
   then (TextEditor(reverse(tail(reverse(x))), ([head(reverse(x))]++h), y, c))
   else jumpHighlightLeft((TextEditor(reverse(tail(reverse(x))), ([head(reverse(x))]++h), y, c))))

--Function to highlight the word on the right (recursive)
jumpHighlightRight :: TextEditor -> TextEditor
jumpHighlightRight(TextEditor(x, h, [], c)) = (TextEditor(x, h, [], c)) --if far right dont try going further right
jumpHighlightRight(TextEditor(x, h, y, c)) = (
   if (head(h)) == ' '
   then (TextEditor(x, (reverse([head(y)]++reverse(h))), tail(y), c))
   else jumpHighlightRight((TextEditor(x, (reverse([head(y)]++reverse(h))), tail(y), c))))

--Function to add new character at cursor position
insertChar :: TextEditor -> Char -> TextEditor
insertChar(TextEditor(x, h, y, c)) inputChar = (TextEditor((x++[inputChar]), h, y, c))

--Function to add new string at cursor position
insertString :: TextEditor -> [Char] -> TextEditor
insertString(TextEditor(x, h, y, c)) inputStr = (TextEditor((x++inputStr), h, y, c))

--Function to delete what is either in selection or the character before the cursor
backspace :: TextEditor -> TextEditor
backspace(TextEditor(x, h, y, c)) =(
   if h /= []
   then (TextEditor(x, [], y, c))
   else (TextEditor(reverse(tail(reverse x)), [], y, c)))

--Function to cut selected content
cut :: TextEditor -> TextEditor
cut(TextEditor(x, h, y, c)) = (TextEditor(x, [], y, h))

--Function to copy selected content
copy :: TextEditor -> TextEditor
copy(TextEditor(x, h, y, c)) = (TextEditor(x, h, y, h))

--Function to paste what is in the virtual clipboard
paste :: TextEditor -> TextEditor
paste(TextEditor(x, h, y, c)) = (TextEditor((x++c), [], y, c))

--Function to save text as a .txt file
save :: TextEditor -> IO ()
save(TextEditor(x, h, y, c)) = (do
   let save = (x++h++y)
   writeFile "file.txt" save)

--Function to open a .txt file
--load :: TextEditor -> IO () -> TextEditor
--load(TextEditor(x, h, y, c)) =E.Catch(do
--      handle <- openFile "file.txt" ReadMode
--      contents <- hGetContents handle
--      (TextEditor(contents, [], [], c)))
--      (TextEditor(x, h, y, c))