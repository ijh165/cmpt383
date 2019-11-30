package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"math"
	"os"
	"reflect"
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

// #4

func linearSearch(x interface{}, lst interface{}) int {
	if reflect.TypeOf(lst).Kind() != reflect.Slice {
		panic("lst should be a slice!")
	}

	if reflect.TypeOf(x) != reflect.TypeOf(lst).Elem() {
		panic("Type of x should be same as type of elements in lst!")
	}

	lstVal := reflect.ValueOf(lst)
	for i := 0; i < lstVal.Len(); i++ {
		if reflect.DeepEqual(x, lstVal.Index(i).Interface()) {
			return i
		}
	}

	return -1
}

// #5

func toBitSeq(num int, bitSize int) (seq []int) {
	binStr := strconv.FormatInt(int64(num), 2)

	// apply zero padding
	padSize := bitSize - len(binStr)
	if padSize < 0 {
		padSize = 0
	}
	binStr = strings.Repeat("0", padSize) + binStr

	// convert to slice
	for _, bitChar := range binStr {
		bit, _ := strconv.Atoi(string(bitChar))
		seq = append(seq, bit)
	}

	return
}

func allBitSeqs(n int) [][]int {
	if n <= 0 {
		return make([][]int, 0)
	}
	bitSeqs := make([][]int, int(math.Pow(2, float64(n))))
	for i := range bitSeqs {
		bitSeqs[i] = toBitSeq(i, n)
	}
	return bitSeqs
}

// ============================================================================
// TESTING
// ============================================================================

func checkFailures(failures int) {
	if failures > 0 {
		fmt.Printf("%v test cases failed", failures)
		fmt.Println()
	} else {
		fmt.Println("All Test Passed")
	}
}

// #1 tests

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

// #2 tests

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
		fmt.Println()
	}

	checkFailures(failures)
	fmt.Println()
}

// #3 tests

var validTimes = []Time24{
	Time24{23, 59, 59},
	Time24{23, 0, 0},
	Time24{6, 35, 7},
	Time24{5, 39, 8},
	Time24{5, 39, 8},
	Time24{1, 2, 3},
	Time24{0, 59, 0},
	Time24{0, 0, 59},
	Time24{0, 0, 0},
}

var validTimesStr = []string{
	"23:59:59",
	"23:00:00",
	"06:35:07",
	"05:39:08",
	"05:39:08",
	"01:02:03",
	"00:59:00",
	"00:00:59",
	"00:00:00",
}

var invalidTimes = []Time24{
	Time24{24, 60, 60},
	Time24{24, 59, 59},
	Time24{23, 60, 59},
	Time24{23, 59, 60},
	Time24{255, 255, 255},
}

func equalsTime24Test() {
	fmt.Println("=== Testing equalsTime24 ===")
	failures := 0
	for _, time := range validTimes {
		if !equalsTime24(time, time) {
			failures++
			fmt.Printf(
				"Failed on equalsTime24(%v, %v). Expected: %v. Actual: %v",
				time,
				time,
				true,
				false)
			fmt.Println()
		}
	}
	checkFailures(failures)
	fmt.Println()
}

func lessThanTime24Test() {
	fmt.Println("=== Testing lessThanTime24 ===")

	failures := 0
	for i, t := range validTimes {
		if i == 0 {
			continue
		}
		prev := validTimes[i-1]
		if equalsTime24(prev, t) {
			if lessThanTime24(t, prev) {
				failures++
				fmt.Printf(
					"Failed on lessThanTime24(%v, %v). Expected: %v. Actual: %v",
					prev,
					t,
					false,
					true)
				fmt.Println()
			}
			continue
		}
		if !lessThanTime24(t, prev) {
			failures++
			fmt.Printf(
				"Failed on lessThanTime24(%v, %v). Expected: %v. Actual: %v",
				prev,
				t,
				true,
				false)
			fmt.Println()
		}
	}

	checkFailures(failures)
	fmt.Println()
}

func timeStringTest() {
	fmt.Println("=== Testing t.String() ===")
	failures := 0
	for i, time := range validTimes {
		actualStr := time.String()
		expectedStr := validTimesStr[i]
		if actualStr != expectedStr {
			failures++
			fmt.Printf(
				"Failed. Expected: %v. Actual: %v",
				expectedStr,
				actualStr)
			fmt.Println()
		}
	}
	checkFailures(failures)
	fmt.Println()
}

func validTime24Test() {
	fmt.Println("=== Testing t.validTime24() ===")
	failures := 0
	for _, time := range validTimes {
		if !time.validTime24() {
			failures++
			fmt.Printf(
				"Failed. Expected to be valid time for %v",
				time)
			fmt.Println()
		}
	}
	for _, time := range invalidTimes {
		if time.validTime24() {
			failures++
			fmt.Printf(
				"Failed. Expected to be invalid time for %v",
				time)
			fmt.Println()
		}
	}
	checkFailures(failures)
	fmt.Println()
}

func minTime24Test() {
	fmt.Println("=== Testing minTime24() ===")

	failures := 0
	actualMinTime, _ := minTime24(validTimes)
	expectedMinTime := Time24{0, 0, 0}
	if !equalsTime24(actualMinTime, expectedMinTime) {
		failures++
		fmt.Printf(
			"Failed. Expected min time to be %v",
			expectedMinTime)
		fmt.Println()
	}

	_, err := minTime24([]Time24{})
	if err == nil {
		failures++
		fmt.Printf("Failed. Expected error upon empty array. Actual is nil.")
		fmt.Println()
	}

	checkFailures(failures)
	fmt.Println()
}

// #4 tests

func linearSearchTest() {
	fmt.Println("=== Testing linearSearch ===")

	// helper function to check if linearSearch panics
	linearSearchPanics :=
		func(x interface{}, lst interface{}) (panicOccurred bool) {
			defer func() {
				if err := recover(); err != nil {
					panicOccurred = true
				}
			}()
			linearSearch(x, lst)
			return
		}

	type TestCases struct {
		x, lst   interface{}
		expected int
	}

	testCases := []TestCases{
		{5, []int{4, 2, -1, 5, 0}, 3},
		{3, []int{4, 2, -1, 5, 0}, -1},
		{"egg", []string{"cat", "nose", "egg"}, 2},
		{"up", []string{"cat", "nose", "egg"}, -1},
		{5, []int{4, 2, -1, 5, 5, 0}, 3},
		{"nose", []string{"cat", "nose", "nose", "egg"}, 1},
		{true, []bool{false, false, true, true, false}, 2},
		{[]int{1, 2}, [][]int{{5, 6}, {1, 2}, {3, 4}}, 1},
		{[]int{}, [][]int{{}, {}, {}}, 0},
	}

	invalidTestCases := []TestCases{
		{"egg", []int{4, 2, -1, 5, 0}, -1},
		{5, []string{"cat", "nose", "egg"}, -1},
		{true, []int{1, 2, 3}, -1},
		{[]int{1, 2}, [][]string{{"hello"}, {"world"}}, -1},
		{"egg", []int{}, -1},
		{5, []string{}, -1},
		{true, []int{}, -1},
		{[]int{}, [][]string{}, -1},
	}

	failures := 0
	for _, testCase := range testCases {
		actual := linearSearch(testCase.x, testCase.lst)
		if actual != testCase.expected {
			failures++
			fmt.Printf(
				"Failed on linearSearch(%v, %v). Expected: %v. Actual: %v",
				testCase.x,
				testCase.lst,
				testCase.expected,
				actual)
			fmt.Println()
		}
	}

	for _, testCase := range invalidTestCases {
		if !linearSearchPanics(testCase.x, testCase.lst) {
			failures++
			fmt.Printf(
				"Failed on linearSearch(%v, %v). Expected to panic.",
				testCase.x,
				testCase.lst)
			fmt.Println()
		}
	}

	checkFailures(failures)
	fmt.Println()
}

// #5 tests

func allBitSeqsTest() {
	fmt.Println("=== Testing allBitSeqs ===")

	testCases := map[int][][]int{
		-1: {},
		0:  {},
		1:  {{0}, {1}},
		2:  {{0, 0}, {0, 1}, {1, 0}, {1, 1}},
		3: {
			{0, 0, 0},
			{0, 0, 1},
			{0, 1, 0},
			{0, 1, 1},
			{1, 0, 0},
			{1, 0, 1},
			{1, 1, 0},
			{1, 1, 1},
		},
	}

	failures := 0
	for n, expectedBitSeqs := range testCases {
		actualBitSeqs := allBitSeqs(n)
		if !reflect.DeepEqual(actualBitSeqs, expectedBitSeqs) {
			failures++
			fmt.Printf(
				"Failed on allBitSeqs(%v). Expected: %v. Actual: %v",
				n,
				expectedBitSeqs,
				actualBitSeqs)
			fmt.Println()
		}
	}

	for n := 4; n <= 16; n++ {
		bitSeqs := allBitSeqs(n)
		actualLen := len(bitSeqs)
		expectedLen := int(math.Pow(2, float64(n)))
		if actualLen != expectedLen {
			fmt.Printf(
				"Failed on allBitSeqs(%v). Expected len: %v. Actual len: %v",
				n,
				expectedLen,
				actualLen)
			fmt.Println()
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
	validTime24Test()
	minTime24Test()

	linearSearchTest()
	allBitSeqsTest()
}
