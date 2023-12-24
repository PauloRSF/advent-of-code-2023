' ---------------------
' INPUT PARSER
' ---------------------

Type CubeSet
  red as UByte
  green as UByte
  blue as UByte
end Type

Type Game
  id as UByte
  sets as CubeSet Ptr
  setsCount as UByte
end Type

Type ParseNaturalResult
  value as ULong
  cursor as ULong
end Type

Declare Function ParseGameLine (gameLine as String) As Game

Function GetLookAhead (text as String, cursor as ULong) As String
  return Chr(text[cursor])
end Function


Function Consume (text as String, cursor as ULong) As ULong
  return cursor + Len(text)
end Function

Function IsDigit (charCode as Integer) as Boolean
  return charCode >= Asc("0") and charCode <= Asc("9")
end Function

Function ParseNatural (text as String, cursor as ULong) As ParseNaturalResult
  Dim result as ParseNaturalResult

  result.value = 0
  result.cursor = cursor

  While (IsDigit(text[result.cursor]))
    result.value = (result.value * 10) + text[result.cursor] - 48
    result.cursor = result.cursor + 1
  WEnd

  return result
end Function

Type ParseCubeSetResult
  value as CubeSet
  cursor as ULong
end Type

Function ParseCubeSet (text as String, cursor as ULong) As ParseCubeSetResult
  Dim result as ParseCubeSetResult

  result.value.red = 0
  result.value.green = 0
  result.value.blue = 0
  result.cursor = cursor

  While (GetLookAhead(text, result.cursor) <> ";" and result.cursor < Len(text))
    Dim cubeCount as UByte = 0
    Dim cubeCountParseResult as ParseNaturalResult = ParseNatural(text, result.cursor)

    cubeCount = cubeCountParseResult.value
    result.cursor = cubeCountParseResult.cursor
    result.cursor = Consume(" ", result.cursor)

    Dim lookAhead as String = GetLookAhead(text, result.cursor)

    If lookAhead = "r" Then
      result.cursor = Consume("red", result.cursor)
      result.value.red = cubeCount
    ElseIf lookAhead = "g" Then
      result.cursor = Consume("green", result.cursor)
      result.value.green = cubeCount
    ElseIf lookAhead = "b" Then
      result.cursor = Consume("blue", result.cursor)
      result.value.blue = cubeCount
    End If

    If GetLookAhead(text, result.cursor) = "," Then
      result.cursor = Consume(", ", result.cursor)
    End If
  WEnd

  return result
end Function

Function ParseGameLine (gameLine as String) As Game
  Dim cursor as ULong = 0
  Dim parsedGame as Game

  parsedGame.setsCount = 0

  cursor = Consume("Game ", cursor)

  Dim gameIdParseResult as ParseNaturalResult = ParseNatural(gameLine, cursor)
  parsedGame.id = gameIdParseResult.value
  cursor = gameIdParseResult.cursor

  cursor = Consume(": ", cursor)
  
  While cursor < Len(gameLine)
    Dim handfulOfCubesParseResult as ParseCubeSetResult = ParseCubeSet(gameLine, cursor)

    parsedGame.setsCount = parsedGame.setsCount + 1
    parsedGame.sets = Reallocate(parsedGame.sets, parsedGame.setsCount * SizeOf(CubeSet))
    parsedGame.sets[parsedGame.setsCount - 1] = handfulOfCubesParseResult.value

    cursor = handfulOfCubesParseResult.cursor

    If GetLookAhead(gameLine, cursor) = ";" Then
      cursor = Consume("; ", cursor)
    End If
  WEnd

  return parsedGame
end Function

' ---------------------
' SOLVER
' ---------------------

Function IsValidGame (game as Game, loadedCubes as CubeSet) As Boolean
  For i As UByte = 0 to game.setsCount Step 1
    If game.sets[i].red > loadedCubes.red or game.sets[i].green > loadedCubes.green or game.sets[i].blue > loadedCubes.blue Then
      return False
    End If
  Next i

  return True
end Function

Function GetMinimalCubeSet (game as Game) As CubeSet
  Dim minimalSet as CubeSet = game.sets[0]

  For i As UByte = 1 to game.setsCount Step 1
    If game.sets[i].red > minimalSet.red Then
      minimalSet.red = game.sets[i].red
    End If

    If game.sets[i].green > minimalSet.green Then
      minimalSet.green = game.sets[i].green
    End If

    If game.sets[i].blue > minimalSet.blue Then
      minimalSet.blue = game.sets[i].blue
    End If
  Next i

  return minimalSet
end Function

Function GetCubeSetPower (cubeSet as CubeSet) As ULong
  return cubeSet.red * cubeSet.green * cubeSet.blue
end Function

sub main()
  Dim currentLine as String, inputFile As ULong, loadedCubes as CubeSet, sumOfValidGameIds as ULong = 0, sumOfMinimalSetPowers as ULong = 0

  loadedCubes.red = 12
  loadedCubes.green = 13
  loadedCubes.blue = 14

  inputFile = FreeFile

  Open "input.txt" for Input as #inputFile

  Do Until Eof(inputFile)
    Line Input #inputFile, currentLine

    Dim game as Game = ParseGameLine(currentLine)

    If IsValidGame(game, loadedCubes) Then
      sumOfValidGameIds = sumOfValidGameIds + game.id
    End If

    Dim minimalSet as CubeSet = GetMinimalCubeSet(game)
    sumOfMinimalSetPowers = sumOfMinimalSetPowers + GetCubeSetPower(minimalSet)
  Loop

  Close #inputFile

  Print "Sum of the valid game IDs: " & sumOfValidGameIds
  Print "Sum of the power of the minimum sets: " & sumOfMinimalSetPowers
end sub

main()
