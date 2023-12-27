import 'package:scratchcards/scratchcards.dart'
    show parseScratchcardsFromFile, countWonScratchcards;

void main(List<String> arguments) async {
  final scratchCards = await parseScratchcardsFromFile('input.txt');

  final totalPoints = scratchCards
      .map((s) => s.points())
      .reduce((totalPoints, points) => totalPoints + points);

  final totalScratchcardCount = countWonScratchcards(scratchCards);

  print("Sum of points for all scratchcards: ${totalPoints}");
  print("Total number of scratchcards after winning: ${totalScratchcardCount}");
}
