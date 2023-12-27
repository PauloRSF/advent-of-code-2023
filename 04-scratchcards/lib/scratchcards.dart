import 'dart:io';
import 'dart:convert';
import 'dart:async';
import 'dart:core';
import 'dart:math';

class Scratchcard {
  int matchingNumbersCount = 0;

  int points() => this.matchingNumbersCount > 0
      ? pow(2, this.matchingNumbersCount - 1).toInt()
      : 0;

  Scratchcard.fromString(String scratchcard) {
    final [_, gameNumbers] = scratchcard.split(': ');
    final [winningNumbers, cardNumbers] = gameNumbers.split(' | ');

    var cardNumbersSet = parseNumberListString(cardNumbers).toSet();

    var winningNumbersSet = parseNumberListString(winningNumbers).toSet();

    this.matchingNumbersCount =
        winningNumbersSet.intersection(cardNumbersSet).length;
  }
}

Iterable<int> parseNumberListString(String numberList) =>
    numberList.split(' ').where((s) => s != '').map((n) => int.parse(n));

int countWonScratchcards(List<Scratchcard> originalScratchcards) {
  var scratchcardCopyCounts =
      new List.generate(originalScratchcards.length, (_) => 1);

  for (var i = 0; i < scratchcardCopyCounts.length; i++) {
    final numberOfCardsToCopy = min(
        originalScratchcards[i].matchingNumbersCount,
        originalScratchcards.length);

    for (var j = 1; j <= numberOfCardsToCopy; j++) {
      scratchcardCopyCounts[i + j] += scratchcardCopyCounts[i];
    }
  }

  return scratchcardCopyCounts.fold(0, (total, count) => total + count);
}

Future<List<Scratchcard>> parseScratchcardsFromFile(String fileName) =>
    File('input.txt')
        .openRead()
        .transform(utf8.decoder)
        .transform(LineSplitter())
        .map(Scratchcard.fromString)
        .toList();
