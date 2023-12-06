package main

import (
	"sync"
	"fmt"
)

type Semaphore struct {
	count int
	mutex sync.Mutex
	cond  *sync.Cond
}

func Semaphore_Init(initial_count int) *Semaphore {
	if(initial_count < 0){
		fmt.Println("Semaphore init error. Initial count cannot be less then 0.")
		return nil;
	}

	s := &Semaphore{count: initial_count}
	s.cond = sync.NewCond(&s.mutex)
	return s
}

func (s *Semaphore) Wait() {
	s.mutex.Lock()
	defer s.mutex.Unlock()

	for s.count <= 0 {
		s.cond.Wait()
	}
	s.count--
	s.cond.Signal()
}

func (s *Semaphore) Post() {
	s.mutex.Lock()
	defer s.mutex.Unlock()
	s.count++
	s.cond.Signal()
}