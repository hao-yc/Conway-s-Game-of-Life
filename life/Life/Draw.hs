
module Life.Draw where
import Life.Board
import Drawing

drawCell :: Pos -> Drawing
drawCell (col, row) =
    translated x y $ solidRectangle 1 1
  where
    x = fromIntegral col  
    y = fromIntegral row

drawBoard :: Board -> Drawing
drawBoard board = foldMap drawCell (liveCells board)

-- 'drawGrid minPos maxPos' obte el dibuix d'una graella que inclou els 2 extrems indicats.
-- 'minPos' es la posicio de l'extrem (esquerra, inferior) i
-- 'maxPos' es la posicio de l'extrem (dreta, superior)'.
-- NOTA: Aquesta funcio s'usa a partir del segon pas de la practica.
drawGrid :: Pos -> Pos -> Drawing
drawGrid minPos maxPos =
    colored gray $
        foldMap vline [fst minPos .. fst maxPos + 1]
        <> foldMap hline [snd minPos .. snd maxPos + 1]
    where
        hline row = polyline [ posToCoords (fst minPos, row), posToCoords (fst maxPos + 1, row) ]
        vline col = polyline [ posToCoords (col, snd minPos), posToCoords (col, snd maxPos + 1) ]
        posToCoords (col, row) = (fromIntegral col - 0.5, fromIntegral row - 0.5)

