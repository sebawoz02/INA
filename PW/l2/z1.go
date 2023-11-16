package main

import (
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"sync"
	"time"
)

type sigIn struct{
	sender int
	sender_location_x int
	sender_location_y int
	channel chan sigOut
	sig_type string
}

type sigOut struct{
	occupied int	// Id of traveler on this field
	edges []int
	err int	// 0 - success, 1 - failure, 2 - YOU DIED
}

type locator_signal_in struct{
	// Request to move out
	channel chan locator_signal_out
}

type locator_signal_out struct{
	// Info if locator managed to move out
	err int	// 0 - success, 1 - no free space, 2 - locators time passed
}

var m, n, k, d, z int	// Grid dimensions, max_num_of_travelers, wild locators lifetime, danger lifetime

var grid [][]int
var channel_in [][]chan sigIn	// grid channel in
var locator_channels [][]chan locator_signal_in
var no_travelers int

var wg = &sync.WaitGroup{}


/* OBJECTS ON GRID */

func traveler(id int) {
	var out_chanel = make(chan sigOut)
	x, y := 0, 0
	alive := true
	for {
		x, y = rand.Intn(m), rand.Intn(n)
		sig_in := sigIn{id, -1, -1, out_chanel, "set"}
		channel_in[x][y] <- sig_in
		sig_out := <-out_chanel
		if(sig_out.err == 0){
			break
		}
		if(sig_out.err == 2){
			alive = false
			break
		}
	}
	for {
		if(alive == false){
			break
		}
		moves := [][2]int{{-1, 0}, {0, -1}, {0, 1}, {1, 0}}

		rand.Shuffle(len(moves), func(i, j int) {
			moves[i], moves[j] = moves[j], moves[i]
		})

		time.Sleep(500*time.Millisecond)

		newX, newY := x+moves[0][0], y+moves[0][1]

		if newX >= 0 && newX < m && newY >= 0 && newY < n {
			channel_in[newX][newY] <- sigIn{id, x, y, out_chanel, "set"}
			sig_out := <-out_chanel
			if sig_out.err == 0 {
				x, y = newX, newY
			}
			if sig_out.err == 2{
				alive = false
				no_travelers--
				fmt.Printf("Traveler %02d stepped on a danger. Num of travelers reduced to %02d", id, no_travelers)
				fmt.Println()
			}
		}
	}
}


func wild_locator(){
	start_time := time.Now()
	spawn_channel := make(chan sigOut)
	alive := true
	x, y := 0, 0
	for {
		x, y = rand.Intn(m), rand.Intn(n)
		sig_in := sigIn{-1, -1, -1, spawn_channel, "set"}	// All wild locators id is -1
		channel_in[x][y] <- sig_in
		sig_out := <-spawn_channel
		if(sig_out.err == 0){
			break
		}
	}
	for {
		if(alive == false){
			break
		}
		elapsed_time := time.Since(start_time).Seconds()
		if(elapsed_time >= float64(d)){	// time passed
			// Check if any signals came in and clear
			select{
			case data := <-locator_channels[x][y]:
				data.channel<-locator_signal_out{2}
			default:
				channel_in[x][y] <- sigIn{-1, x, y, spawn_channel, "clear"}
				for{	// Wait for confirmation
					select{
					case data := <-spawn_channel:
						if(data.err == 0){
							break
						}	
					default:
						select{
							case data := <-locator_channels[x][y]:
								data.channel<-locator_signal_out{2}
							default:
								continue
						}
					}
				}
			}
			break
		}
		// Use select to check for a signal without blocking
		select {
			case data := <-locator_channels[x][y]:
				// Check if you can move out
				moves := [][2]int{{-1, 0}, {0, -1}, {0, 1}, {1, 0}}
				success := false
				dx, dy := 0, 0
				for i := 0; i < 4; i++{
					dx, dy = moves[i][0], moves[i][1]
					if(x + dx > 0 && x + dx < n && y + dy > 0 && y + dy < m){
						channel_in[x+dx][y+dy]<-sigIn{-1, x, y, spawn_channel, "set"}
						ret := <-spawn_channel
						if(ret.err == 0){
							success = true
							break
						}
						if(ret.err == 2){	// You moved out on a trap
							alive = false
							break
						}
					}
				}
				if(success == true){
					x += dx
					y += dy
					data.channel <- locator_signal_out{0} // Success
				} else{
					data.channel <- locator_signal_out{1} // No free space
				}
			default:
				continue
		}
	}
	defer wg.Done()
}


func danger(){
	start_time := time.Now()
	spawn_channel := make(chan sigOut)
	x, y := 0, 0
	for {
		x, y = rand.Intn(m), rand.Intn(n)
		sig_in := sigIn{-2, -1, -1, spawn_channel, "set"}	// All dangers id is -2
		channel_in[x][y] <- sig_in
		sig_out := <-spawn_channel
		if(sig_out.err == 0){
			break
		}
	}
	for {
		elapsed_time := time.Since(start_time).Seconds()
		if(elapsed_time >= float64(z)){	// time passed
			// Check if any signals came in and clear
			select{
			case <-locator_channels[x][y]:
				break
			default:
				channel_in[x][y] <- sigIn{-2, x, y, spawn_channel, "clear"}
			}
			break
		}
		// Use select to check for a signal without blocking
		select {
			case <-locator_channels[x][y]:	// Communication with danger is also through locator_channels
				// If danger is receiving signal it means someone triggered it
				break
			default:
				continue
		}
	}
	defer wg.Done()
}


/* GRID HANDLER */


func receiver(i int, j int) {
	loc_chan := make(chan locator_signal_out) // channel used to communicate with locator

	var edges = make([]int, 4)	
	edges[0], edges[1], edges[2], edges[3] = 0, 0, 0, 0
	for{
		sig_in := <-channel_in[i][j]
		switch sig_in.sig_type{

		case "set":
			if(grid[i][j] == -1){ // Wild locator is here
				if(sig_in.sender == -2){
					sig_in.channel<-sigOut{-1, edges, 1} 
					continue
				}
				if(sig_in.sender == -1){
					sig_in.channel<-sigOut{-1, edges, 1} 
					continue
				}
				// Send request to move out
				locator_channels[i][j]<-locator_signal_in{loc_chan}
				data:=<-loc_chan
				if(data.err == 1){				// Can't move in
					fmt.Printf("Traveler %02d could not move in. Wild locator had nowhere to go.", sig_in.sender)
					sig_in.channel <- sigOut{-1, edges, 1}
					fmt.Println()
					continue
				}
				fmt.Printf("Traveler %02d made wild locator move out.", sig_in.sender)
				fmt.Println()
			}else if(grid[i][j] == -2){	// Danger is here
				if(sig_in.sender == - 2){	// Danger cannot step on another danger
					sig_in.channel <- sigOut{-2, edges, 1}
				}
				if(sig_in.sender_location_x != -1 && sig_in.sender_location_y != -1){ // You cant spawn on danger
					// Clear sender
					channel_in[sig_in.sender_location_x][sig_in.sender_location_y] <- sigIn{0, 0, 0, sig_in.channel,"clear"}
					// Clear danger
					locator_channels[i][j] <- locator_signal_in{loc_chan}
					grid[i][j] = 0
					sig_in.channel <- sigOut{0, edges, 2}
				}
				continue
			}else if(grid[i][j] != 0){
				// Field occupied send failure signal
				sig_out := sigOut{grid[i][j], edges, 1}
				sig_in.channel <- sig_out
				continue
			}
			if(sig_in.sender_location_x != -1 && sig_in.sender_location_y != -1){
				// 0 - left, 1 - up, 2 - down, 3 - right
				var dir int
				if(sig_in.sender_location_x < i && sig_in.sender_location_y == j){
					dir = 1
				}else if(sig_in.sender_location_x == i && sig_in.sender_location_y > j){
					dir = 3
				}else if(sig_in.sender_location_x == i && sig_in.sender_location_y < j){
					dir = 0
				}else{
					dir = 2
				}
				edges[dir] = 1
				// Signal to clear prev position
				if(sig_in.sender != -1){ // Dont clear after wild locator, someone is moving in there
					channel_in[sig_in.sender_location_x][sig_in.sender_location_y] <- sigIn{0, 0, 0, sig_in.channel,"clear"}
				}
			}
			// Set new position
			grid[i][j] = sig_in.sender
			// Send success signal
			sig_out := sigOut{sig_in.sender, edges, 0}
			sig_in.channel <- sig_out

		case "clear":
			// Clear field
			grid[i][j] = 0	
			// If its wild locator clearing send confirmation signal
			if(sig_in.sender == -1){
				sig_in.channel<-sigOut{grid[i][j], edges, 0}
			}
		
		case "read":
			// Send all informations about this field
			var send_edges = make([]int, 4)
			send_edges[0], send_edges[1], send_edges[2], send_edges[3] = edges[0], edges[1], edges[2], edges[3]
			sig_in.channel <- sigOut{grid[i][j], send_edges, 0}
			edges[0], edges[1], edges[2], edges[3] = 0, 0, 0, 0
		}
	}
}

/* CAMERA */

func camera() {
	var camera_chanel_out = make(chan sigOut)
	for {
		var info_grid [][]int
		var edges [][]int
		for i := 0; i < m; i++ {
			info_grid = append(info_grid, make([]int, n))
		}
		for i := 0; i < 2*m-1; i++ {
			edges = append(edges, make([]int, n))
		}

		// Get info
		for i := 0; i < m; i++ {
			for j := 0; j < n; j++ {
				sig_in := sigIn{0, -1, -1, camera_chanel_out, "read"}
				channel_in[i][j] <- sig_in
				sig_out := <-camera_chanel_out
				info_grid[i][j] = sig_out.occupied
				// 0 - left, 1 - up, 2 - down, 3 - right
				if(2*i < 2*m-2){
					edges[2*i+1][j] = max(edges[2*i+1][j], sig_out.edges[2])
				}	
				edges[2*i][j] = max(edges[2*i][j], sig_out.edges[3])
				if(j > 0){
					edges[2*i][j-1] = max(edges[2*i][j-1], sig_out.edges[0])
				}
				
				if(i > 0){
					edges[2*i-1][j] = max(edges[2*i-1][j], sig_out.edges[1])
				}
			}
		}
		// Print photo
		for i := 0; i < m; i++ {
			for j := 0; j < n; j++ {
				if info_grid[i][j] > 0 {
					fmt.Printf("%02d ", info_grid[i][j])
				}else if info_grid[i][j] == -1{
					fmt.Printf(" * ")
				}else if info_grid[i][j] == -2{
					fmt.Printf(" # ")
				}else {
					fmt.Print("{ }")
				}
				if j < n && edges[2*i][j] == 1 {
					fmt.Print("--")
					edges[2*i][j] = 0
				} else {
					fmt.Print("  ")
				}
			}
			fmt.Println()
			for j := 0; j < n; j++ {
				if 2*i+1 < 2*m-1 && edges[2*i+1][j] == 1 {
					fmt.Print(" |   ")
					edges[2*i+1][j] = 0
				} else {
					fmt.Print("     ")
				}
			}
			fmt.Println()
		}

		fmt.Println("-----------")
		time.Sleep(2 * time.Second) // Photo every 2 seconds
	}
}


/* SPAWNERS */

func traveler_spawner() {
	no_travelers = 0
	id := 1
	for {
		time.Sleep(time.Duration(rand.Intn(3)+1) * time.Second)
		if no_travelers < k {
			wg.Add(1)
			go traveler(id)
			no_travelers++
			fmt.Printf("Traveler %02d spawned - he is %02d(-th) at the moment", id, no_travelers)
			fmt.Println()
			id++
		}
	}
}


func wild_locator_spawner() {	// Locators keep spawning every (d/4+1)s-(d/4+3)s
	for {
		time.Sleep(time.Duration(rand.Intn(2)+int(d/4)+1) * time.Second)
		wg.Add(1)
		go wild_locator()
	}
}

func danger_spawner() { // Danger keep spawning every (z/4+1)s-(z/4+3)s
	for {
		time.Sleep(time.Duration(rand.Intn(2)+int(z/4)+1) * time.Second)
		wg.Add(1)
		go danger()
	}
}



func main() {
	if len(os.Args) < 6 {
		fmt.Println("Usage: go run z1.go <grid x dim> <grid y dim> <max_no_travelers> <wild_lifetim> <danger_lifetime>")
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

	d, err = strconv.Atoi(os.Args[4])
	if err != nil {
		fmt.Println("Invalid value for d:", err)
		return
	}

	z, err = strconv.Atoi(os.Args[5])
	if err != nil {
		fmt.Println("Invalid value for z:", err)
		return
	}

	// Init grid
	for i := 0; i < m; i++ {
		grid = append(grid, make([]int, n))
	}

	// Init channels
	for i := 0; i < m; i++ {
		channel_in = append(channel_in, make([]chan sigIn, n))
		locator_channels = append(locator_channels, make([]chan locator_signal_in, n))
		for j := 0; j < n; j++{
			channel_in[i][j] = make(chan sigIn)
			locator_channels[i][j] = make(chan locator_signal_in)
		}
	}

	// Init channel receivers goroutines
	for i := 0; i < m; i++ {
		for j := 0; j < n; j++{
			wg.Add(1)
			go receiver(i, j)
		}
	}

	// Init camera goroutine
	wg.Add(1)
	go camera()

	// Init travelers generator goroutine
	wg.Add(1)
	go traveler_spawner()
	
	// Init wild_locators generator goroutine
	wg.Add(1)
	go wild_locator_spawner()

	// Init danger generator goroutine
	wg.Add(1)
	go danger_spawner()

	wg.Wait()

	// Close channels
	for i := 0; i < m; i++ {
		for j := 0; j < n; j++{
			close(channel_in[i][j])
			close(locator_channels[i][j])
		}
	}
}
