package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"reflect"
	"sort"
	"strconv"
	"strings"
)

// ============================================================================
// ANSWERS
// ============================================================================

// #1

// From prof's code: http://www.sfu.ca/~tjd/383fall2019/go/primes.html
func isPrime(n int) bool {
	switch {
	case n < 2:
		return false
	case n == 2:
		return true
	case n%2 == 0:
		return false
	default: // n > 2
		d := 3
		for d*d <= n {
			if n%d == 0 {
				return false
			}
			d += 2
		} // for
		return true
	} // switch
}

// Algorithm from https://www.geeksforgeeks.org/write-a-program-to-reverse-digits-of-a-number/
func reverseInt(n int) (nReversed int) {
	const RADIX = 10
	for n > 0 {
		nReversed *= RADIX
		nReversed += n % RADIX
		n /= RADIX
	}
	return
}

func isEmirp(n int) bool {
	nReversed := reverseInt(n)
	return isPrime(n) && isPrime(nReversed) && n != nReversed
}

func countEmirpsLessThan(n int) (count int) {
	for i := 13; i < n; i++ {
		if isEmirp(i) {
			count++
		}
	}
	return
}

// #2
func countWords(filename string) (wordCountMap map[string]int, err error) {
	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return
	}

	wordCountMap = make(map[string]int)
	for _, word := range strings.Fields(string(bytes)) {
		wordCountMap[word]++
	}

	return
}

// #3

type Time24 struct {
	hour, minute, second uint8
}

func (t Time24) String() string {
	// Helper function to stringify uint8
	stringifyUint8 := func(n uint8) (s string) {
		s = strconv.Itoa(int(n))
		if n < 10 {
			s = "0" + s
		}
		return
	}

	return stringifyUint8(t.hour) + ":" +
		stringifyUint8(t.minute) + ":" +
		stringifyUint8(t.second)
}

func (t Time24) validTime24() bool {
	// 0 <= hour < 24
	// 0 <= minute < 60
	// 0 <= second < 60
	return 0 <= t.hour && t.hour < 24 &&
		0 <= t.minute && t.minute < 60 &&
		0 <= t.second && t.second < 60
}

func equalsTime24(a Time24, b Time24) bool {
	return a.hour == b.hour && a.minute == b.minute && a.second == b.second
}

func lessThanTime24(a Time24, b Time24) bool {
	// Helper function to convert Time24 to seconds
	toSeconds := func(t Time24) uint32 {
		return uint32(t.hour)*3600 + uint32(t.minute)*60 + uint32(t.second)
	}

	return toSeconds(a) < toSeconds(b)
}

func minTime24(times []Time24) (minTime Time24, err error) {
	if len(times) == 0 {
		minTime = Time24{0, 0, 0}
		err = errors.New("times slice is empty")
		return
	}

	minTime = times[0]
	for _, t := range times[1:] {
		if lessThanTime24(t, minTime) {
			minTime = t
		}
	}
	return
}

// ============================================================================
// TESTING
// ============================================================================

func checkFailures(failures int) {
	if failures > 0 {
		fmt.Printf("%v test cases failed", failures)
	} else {
		fmt.Println("All Test Passed")
	}
}

// countEmirpsLessThan tests

func countEmirpsLessThanTest(verbose bool) {
	fmt.Println("=== Testing countEmirpsLessThan ===")

	emirps := []int{
		13,
		17,
		31,
		37,
		71,
		73,
		79,
		97,
		107,
		113,
		149,
		157,
		167,
		179,
		199,
	}

	failures := 0
	for i, emirp := range emirps {
		start := 0
		if i > 0 {
			start = emirps[i-1] + 1
		}
		for n := start; n <= emirp; n++ {
			if verbose {
				fmt.Printf("Testing countEmirpsLessThan(%v)", n)
				fmt.Println()
			}
			if count := countEmirpsLessThan(n); count != i {
				failures++
				fmt.Printf(
					"Failed on countEmirpsLessThan(%v). Expected: %v. Actual: %v",
					n,
					i,
					count)
				fmt.Println()
			}
		}
	}

	checkFailures(failures)
	fmt.Println()
}

// countWords tests

func countWordsTest(verbose bool) {
	fmt.Println("=== Testing countWords ===")

	testCases := map[string]map[string]int{
		"The big big dog\nate the big apple": {
			"The":   1,
			"big":   3,
			"dog":   1,
			"ate":   1,
			"the":   1,
			"apple": 1,
		},
		"The\n\nbig\r\nbig dog\nate\r\nthe\t\tbig apple": {
			"The":   1,
			"big":   3,
			"dog":   1,
			"ate":   1,
			"the":   1,
			"apple": 1,
		},
		"": {},
	}

	const FILE_NAME = "test.txt"
	const FILE_PERM = 0644

	defer os.Remove(FILE_NAME)

	failures := 0
	for fileContent, expectedMap := range testCases {
		if verbose {
			fmt.Println("Testing with file content:")
			fmt.Println("```")
			if fileContent != "" {
				fmt.Println(fileContent)
			}
			fmt.Println("```")
		}

		bytes := []byte(fileContent)

		err := ioutil.WriteFile(FILE_NAME, bytes, FILE_PERM)
		if err != nil {
			fmt.Printf("Error writing to \"%v\": %v", FILE_NAME, err)
			fmt.Println()
			return
		}

		actualMap, err := countWords(FILE_NAME)
		if err != nil {
			fmt.Printf("Unexpected error: %v", err)
			fmt.Println()
			return
		}

		if !reflect.DeepEqual(actualMap, expectedMap) {
			failures++
			fmt.Printf("Failed. Expected: %v. Actual: %v", actualMap, expectedMap)
			fmt.Println()
			fmt.Println("File content:")
			fmt.Println(fileContent)
		}
	}

	// Testing for non-existent file.
	os.Remove(FILE_NAME)
	_, err := countWords(FILE_NAME)
	if err == nil {
		failures++
		fmt.Printf("Failed. Expected error upon non-existent file. Actual is nil.")
	}

	checkFailures(failures)
	fmt.Println()
}

// Time24 tests

var validTimes = map[Time24]string{
	Time24{hour: 23, minute: 59, second: 59}: "23:59:59",
	Time24{hour: 23, minute: 0, second: 0}:   "23:00:00",
	Time24{hour: 0, minute: 59, second: 0}:   "00:59:00",
	Time24{hour: 0, minute: 0, second: 59}:   "00:00:59",
	Time24{hour: 0, minute: 0, second: 0}:    "00:00:00",
	Time24{hour: 1, minute: 2, second: 3}:    "01:02:03",
	Time24{hour: 6, minute: 35, second: 7}:   "06:35:07",
	Time24{hour: 5, minute: 39, second: 8}:   "05:39:08",
	Time24{hour: 5, minute: 39, second: 8}:   "05:39:08",
}

func equalsTime24Test() {
	fmt.Println("=== Testing equalsTime24Test ===")
	failures := 0
	for time := range validTimes {
		if !equalsTime24(time, time) {
			failures++
			fmt.Printf(
				"Failed on equalsTime24(%v, %v). Expected: %v. Actual: %v",
				time,
				time,
				true,
				false)
		}
	}
	checkFailures(failures)
	fmt.Println()
}

func lessThanTime24Test() {
	fmt.Println("=== Testing lessThanTime24 ===")

	validTimesSorted := make([]Time24, len(validTimes))
	i := 0
	for time := range validTimes {
		validTimesSorted[i] = time
		i++
	}
	sort.Slice(validTimesSorted, func(i, j int) bool {
		return lessThanTime24(validTimesSorted[i], validTimesSorted[j])
	})

	failures := 0
	for i, t := range validTimesSorted {
		if i == 0 {
			continue
		}
		prev := validTimesSorted[i-1]
		if equalsTime24(prev, t) {
			continue
		}
		if !lessThanTime24(prev, t) {
			failures++
			fmt.Printf(
				"Failed on lessThanTime24(%v, %v). Expected: %v. Actual: %v",
				prev,
				t,
				true,
				false)
		}
	}

	checkFailures(failures)
	fmt.Println()
}

func timeStringTest() {
	fmt.Println("=== Testing lessThanTime24 ===")
	failures := 0
	for time, str := range validTimes {
		if time.String() != str {
			failures++
			fmt.Printf(
				"Failed. Expected: %v. Actual: %v",
				str,
				time.String())
		}
	}
	checkFailures(failures)
	fmt.Println()
}

func main() {
	countEmirpsLessThanTest(false)
	countWordsTest(false)

	equalsTime24Test()
	lessThanTime24Test()
	timeStringTest()
}
