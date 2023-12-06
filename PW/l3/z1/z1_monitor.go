package main

import (
	"sync"
	"fmt"
	"time"
	"math/rand"
)

type Monitor struct {
	state []string
	cond *sync.Cond
	mmutex *sync.Mutex
	eating_list []string
}

const (
	Thinking = "Thinking"
	Hungry = "Hungry"
	Eating = "Eating"
)

func monitor_init(n int) *Monitor {
	state := make([]string, n)
	for i := range state {
		state[i] = Thinking
	}
	mut := sync.Mutex{}
	monitor := &Monitor{
		state: state,
		mmutex: &mut,
		cond: sync.NewCond(&mut),
	}
	return monitor;
}

func (m *Monitor) pick_Up_Fork(id int) {
	m.mmutex.Lock()
	defer m.mmutex.Unlock()

	m.state[id] = Hungry

	m.test(id)
	if m.state[id] != Eating {
		m.cond.Wait()
	}
}

func (m *Monitor) put_Down_Fork(id int) {
	m.mmutex.Lock()
	m.remove_From_Eating_List(id)
	m.state[id] = Thinking
	m.test((id+len(m.state)-1)%len(m.state))
	m.test((id+1)%len(m.state))
	m.mmutex.Unlock()
}

func (m *Monitor) test(id int) {
	if m.state[(id+len(m.state)-1)%len(m.state)] != Eating &&
	   m.state[id] == Hungry &&
	   m.state[(id+1)%len(m.state)] != Eating{
		m.state[id] = Eating
		m.add_To_Eating_List(id)
		m.cond.Signal()
	}
}

func (m *Monitor) add_To_Eating_List(id int) {
	to_print := fmt.Sprintf("(W%d, F%d, W%d)",id, id, (id+1)%len(m.state))
	m.eating_list = append(m.eating_list, to_print)
	fmt.Println("$ ", m.eating_list)
}

func (m *Monitor) remove_From_Eating_List(id int) {
	to_print := fmt.Sprintf("(W%d, F%d, W%d)",id, id, (id+1)%len(m.state))

	for i := 0; i < len(m.eating_list); i++{
		if m.eating_list[i] == to_print {
			m.eating_list = append(m.eating_list[:i], m.eating_list[i+1:]...)
			fmt.Println("$ ", m.eating_list)
			break
		}
	}
}

func philisopher(id int, monitor *Monitor, wg *sync.WaitGroup){
	defer wg.Done()
	for{
		time.Sleep(time.Second* time.Duration(rand.Intn(3)+1))
		monitor.pick_Up_Fork(id)
		time.Sleep(time.Second * time.Duration(rand.Intn(3)+1))
		monitor.put_Down_Fork(id)
	}
}


func main() {
	n := 6
	monitor := monitor_init(n)
	var wg sync.WaitGroup

	for i:= 0; i<n; i++ {
		wg.Add(1)
		go philisopher(i, monitor, &wg)
	}

	wg.Wait()
}