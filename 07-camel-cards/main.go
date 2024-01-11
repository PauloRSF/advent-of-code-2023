package main

import "os"
import "fmt"
// import "sort"
import "bufio"
import "slices"
import "strings"
import "strconv"

type HandType = uint8
type CardStrength = uint8

const (
	HighCard HandType = iota
	OnePair
	TwoPair
	ThreeOfAKind
	FullHouse
	FourOfAKind
	FiveOfAKind
)

type Hand struct {
	cardStrengths []CardStrength
	typ HandType
	bid uint
}

func mapCardToStrength(card rune) CardStrength {
	if (card >= '2' && card <= '9') {
		return uint8(card) - '2'
	}

	if (card == 'T') {
		return 10
	}

	if (card == 'J') {
		return 11
	}

	if (card == 'Q') {
		return 12
	}

	if (card == 'K') {
		return 13
	}

	if (card == 'A') {
		return 14
	}

	return 0
}

func getHandType(cards []CardStrength) HandType {
	cardCountsMap := make(map[CardStrength]int)

	for _, card := range cards {
		cardCountsMap[card] = cardCountsMap[card] + 1
	}

	var cardCounts []int
	
	for _, count := range cardCountsMap {
		cardCounts = append(cardCounts, count)
	}

	slices.Sort(cardCounts)

	if (slices.Equal(cardCounts, []int{5})) {
		return FiveOfAKind
	}

	if (slices.Equal(cardCounts, []int{1, 4})) {
		return FourOfAKind
	}

	if (slices.Equal(cardCounts, []int{2, 3})) {
		return FullHouse
	}

	if (slices.Equal(cardCounts, []int{1, 1, 3})) {
		return ThreeOfAKind
	}

	if (slices.Equal(cardCounts, []int{1, 2, 2})) {
		return TwoPair
	}

	if (slices.Equal(cardCounts, []int{1, 1, 1, 2})) {
		return OnePair
	}

	return HighCard
}

func parseHand(handString string) Hand {
	splitHandString := strings.Split(handString, " ")

	var cardStrengths []CardStrength

	for _, card := range splitHandString[0] {
		cardStrengths = append(cardStrengths, mapCardToStrength(card))
	}

	bid, _ := strconv.Atoi(splitHandString[1])

	handType := getHandType(cardStrengths)

	return Hand{cardStrengths, handType, uint(bid)}
}

func readInput(filePath string) []Hand {
	file, _ := os.Open(filePath)
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var hands []Hand

	for scanner.Scan() {
		parsedHand := parseHand(scanner.Text())
		hands = append(hands, parsedHand)
	}

	return hands
}

func computeTotalWinnings(hands []Hand) uint {
	slices.SortFunc(hands, func(a, b Hand) int {
		if (a.typ > b.typ) {
			return 1
		}

		if (a.typ < b.typ) {
			return -1
		}

		for i := range a.cardStrengths {
			if (a.cardStrengths[i] > b.cardStrengths[i]) {
				return 1
			}

			if (a.cardStrengths[i] < b.cardStrengths[i]) {
				return -1
			}
		}

		return 0
	})

	var total uint

	for i, hand := range hands {
		total += uint(i + 1) * hand.bid
	}

	fmt.Println(hands)

	return total
}

func main() {
	hands := readInput("input.txt");

	totalWinnings := computeTotalWinnings(hands)

	fmt.Println("Total winnings of hands:", totalWinnings)
}
