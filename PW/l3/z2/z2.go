package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

type AttendanceList struct{
	list []string
	mutex sync.Mutex
}

func AttendanceListInit() *AttendanceList {
	a := AttendanceList{
		mutex: sync.Mutex{},
	}
	return &a
}

func (a *AttendanceList) Add(id string){
	a.mutex.Lock()
	a.list = append(a.list, id)
	fmt.Println(a.list)
	a.mutex.Unlock()
}

func (a *AttendanceList) Remove(id string){
	a.mutex.Lock()
	for i := 0; i < len(a.list); i++{
		if a.list[i] == id{
			a.list = append(a.list[:i], a.list[i+1:]...)
			break
		}
	}
	fmt.Println(a.list)
	a.mutex.Unlock()
}

var (
	rwLock sync.RWMutex		// Allows to RLock only for writers (used by readers) 
							// or Lock for everyone (used by writers)
	n 	   int
	attendance_list AttendanceList
)

func main() {
	n = 6
	attendance_list = AttendanceList{}

	// Start readers and writers
	for i := 0; i < n; i++ {
		go reader(i)
		go writer(i)
	}

	// Keep the main goroutine running
	select {}
}

func reader(id int) {
	for {
		time.Sleep(time.Duration(rand.Intn(3)+1) * time.Second)

		// Enter
		rwLock.RLock()

		// Add yourself to attendance_list
		attendance_list.Add(fmt.Sprintf("R%d", id))

		// Sleep
		time.Sleep(time.Duration(rand.Intn(3)+1) * time.Second)

		// Remove yourself from attendance_list
		attendance_list.Remove(fmt.Sprintf("R%d", id))

		// Exit
		rwLock.RUnlock()
	}
}

func writer(id int) {
	for {
		time.Sleep(time.Duration(rand.Intn(3)+ 1) * time.Second)

		// Enter
		rwLock.Lock()
		
		// Add to attendance list
		attendance_list.Add(fmt.Sprintf("W%d", id))

		// Sleep
		time.Sleep(time.Duration(rand.Intn(3)+ 1) * time.Second)

		// Remove from attendance list
		attendance_list.Remove(fmt.Sprintf("W%d", id))

		// Exit
		rwLock.Unlock()
	}
}