Type GameCubes
  red as UByte
  green as UByte
  blue as UByte
end Type

Type Game
  id as UByte
  cubes as GameCubes
end Type

Type ParseNaturalResult
  value as ULong
  cursor as ULong
end Type

Declare Function ParseGameLine (gameLine as String) As Game

Function Consume (text as String, cursor as ULong) As ULong
  return cursor + Len(text)
end Function

Function ParseNatural (text as String, cursor as ULong) As ParseNaturalResult
  Dim newCursor as ULong = cursor
  Dim value as ULong = 0
  Dim result as ParseNaturalResult

  While (text[newCursor] >= 49 and text[newCursor] <= 57)
    value = (value * 10) + text[newCursor] - 48
    newCursor = newCursor + 1
  WEnd

  result.value = value
  result.cursor = newCursor

  return result
end Function

Function ParseGameLine (gameLine as String) As Game
  Dim parsedGame as Game

  Dim cursor as ULong = 0

  cursor = Consume("Game ", cursor)

  Dim gameIdParseResult as ParseNaturalResult = ParseNatural(gameLine, cursor)
  parsedGame.id = gameIdParseResult.value

  cursor = Consume(": ", cursor)

  ' Print cursor
  ' Print gameIdParseResult.cursor
  ' Print gameIdParseResult.value

  parsedGame.cubes.red = 12
  parsedGame.cubes.green = 12
  parsedGame.cubes.blue = 12

  return parsedGame
end Function

sub main()
  Dim currentLine as String
  Dim inputFile As long
  Dim loadedCubes as GameCubes

  With loadedCubes
    .red = 12
    .green = 12
    .blue = 12
  end With

  inputFile = FreeFile

  Open "input.txt" for Input as #inputFile

  Do Until Eof(inputFile)
    Line Input #inputFile, currentLine

    Print currentLine

    Dim game as Game = ParseGameLine(currentLine)

    Print "Game { id: " & game.id & ", red: " & game.cubes.red & ", green: " & game.cubes.green & ", blue: " & game.cubes.red & " }"
  Loop

  Close #inputFile
end sub

main()
