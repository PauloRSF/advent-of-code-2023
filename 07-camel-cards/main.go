package main

import "os"
import "fmt"
import "bufio"
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
	return FiveOfAKind
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

func main() {
	hands := readInput("input.txt");

	fmt.Println(hands)
}
