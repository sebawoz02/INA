package main

import (
	"sync"
	"fmt"
	"time"
	"math/rand"
)

const (
	Thinking = "Thinking"
	Hungry = "Hungry"
	Eating = "Eating"
)

var n int
var state []string
var common_mutex sync.Mutex
var eating_list []string
var philisophers []*Semaphore


func pick_Up_Fork(id int) {
	common_mutex.Lock()
	state[id] = Hungry
	test(id)
	common_mutex.Unlock()

	philisophers[id].Wait()
}

func put_Down_Fork(id int) {
	common_mutex.Lock()
	state[id] = Thinking
	remove_From_Eating_List(id)
	test((id+1)%n)
	test((id+n-1)%n)
	common_mutex.Unlock()
}

func test(id int) {
	if state[(id+n-1)%n] != Eating &&
	   state[id] == Hungry &&
	   state[(id+1)%n] != Eating{
		
		state[id] = Eating
		add_To_Eating_List(id)

		philisophers[id].Post()
 	}
}

func add_To_Eating_List(id int) {
	to_print := fmt.Sprintf("(W%d, F%d, W%d)",id, id, (id+1)%n)
	eating_list = append(eating_list, to_print)
	fmt.Println("$ ", eating_list)
}

func remove_From_Eating_List(id int) {
	to_print := fmt.Sprintf("(W%d, F%d, W%d)",id, id, (id+1)%n)

	for i := 0; i < len(eating_list); i++{
		if eating_list[i] == to_print {
			eating_list = append(eating_list[:i], eating_list[i+1:]...)
			fmt.Println("$ ", eating_list)
			break
		}
	}
}

func philisopher(id int, wg *sync.WaitGroup){
	defer wg.Done()
	for{
		time.Sleep(time.Second* time.Duration(rand.Intn(3)+1))
		pick_Up_Fork(id)
		time.Sleep(time.Second * time.Duration(rand.Intn(3)+1))
		put_Down_Fork(id)
	}
}

func main(){
	n = 6
	state = make([]string, n)
	for i := 0; i < n; i++ {
		state[i] = Thinking
		philisophers = append(philisophers, Semaphore_Init(0))
	}
	common_mutex = sync.Mutex{}
	var wg sync.WaitGroup

	for i:= 0; i<n; i++ {
		wg.Add(1)
		go philisopher(i, &wg)
	}

	wg.Wait()
}

