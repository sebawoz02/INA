package main

import (
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"sync"
	"time"
)

var m, n, k int

var grid [][]int
var edges [][]int
var mutex = &sync.Mutex{}
var wg = &sync.WaitGroup{}

func main() {
	if len(os.Args) < 4 {
		fmt.Println("Usage: go run program.go m n k")
		return
	}

	var err error
	m, err = strconv.Atoi(os.Args[1])
	if err != nil {
		fmt.Println("Invalid value for m:", err)
		return
	}

	n, err = strconv.Atoi(os.Args[2])
	if err != nil {
		fmt.Println("Invalid value for n:", err)
		return
	}

	k, err = strconv.Atoi(os.Args[3])
	if err != nil {
		fmt.Println("Invalid value for k:", err)
		return
	}

	// Init grid
	for i := 0; i < m; i++ {
		grid = append(grid, make([]int, n))
	}

	// Init edges/footprints array
	for i := 0; i < 2*m-1; i++ {
		edges = append(edges, make([]int, n))
	}

	// Init camera goroutine
	wg.Add(1)
	go camera()

	// Init travelers generator goroutine
	wg.Add(1)
	go newTravelerGenerator()

	wg.Wait()
}

func traveler(id int) {
	defer wg.Done()
	x, y := 0, 0
	for {
		x, y = rand.Intn(m), rand.Intn(n)
		mutex.Lock()
		if grid[x][y] == 0 {
			grid[x][y] = id + 1
			mutex.Unlock()
			break
		}
		mutex.Unlock()
	}
	for {

		moves := [][2]int{{-1, 0}, {0, -1}, {0, 1}, {1, 0}}

		rand.Shuffle(len(moves), func(i, j int) {
			moves[i], moves[j] = moves[j], moves[i]
		})

		time.Sleep(time.Second)

		newX, newY := x+moves[0][0], y+moves[0][1]
		if newX >= 0 && newX < m && newY >= 0 && newY < n {
			mutex.Lock()
			if grid[newX][newY] == 0 {
				grid[x][y] = 0
				// leave footprint
				if x-newX == 0 { // left or right
					if newY > y { // right
						edges[2*x][y] = 1
					} else {
						edges[2*x][y-1] = 1
					}
				} else {
					if newX > x { // down
						edges[2*x+1][y] = 1
					} else {
						edges[2*x-1][y] = 1
					}
				}
				grid[newX][newY] = id + 1
				x, y = newX, newY
			}
			mutex.Unlock()
		}
	}
}

func camera() {
	defer wg.Done()
	for {
		mutex.Lock()
		for i := 0; i < m; i++ {
			for j := 0; j < n; j++ {
				if grid[i][j] != 0 {
					fmt.Printf("%02d ", grid[i][j])
				} else {
					fmt.Print("{ }")
				}
				if j < n-1 && edges[2*i][j] == 1 {
					fmt.Print("--")
				} else {
					fmt.Print("  ")
				}
			}
			fmt.Println()
			for j := 0; j < n; j++ {
				if 2*i+1 < 2*m-1 && edges[2*i+1][j] == 1 {
					fmt.Print(" |   ")
				} else {
					fmt.Print("     ")
				}
			}
			fmt.Println()
		}
		// Clear footprints
		for i := 0; i < 2*m-1; i++ {
			for j := 0; j < n; j++ {
				edges[i][j] = 0
			}
		}
		mutex.Unlock()
		fmt.Println("-----------")
		time.Sleep(2 * time.Second) // Photo every 2 seconds
	}
}

func newTravelerGenerator() {
	defer wg.Done()
	no_travelers := 0
	for {
		time.Sleep(time.Duration(rand.Intn(3)+1) * time.Second)
		mutex.Lock()
		if no_travelers < k {
			wg.Add(1)
			go traveler(no_travelers)
			no_travelers++
		}
		mutex.Unlock()
	}
}
