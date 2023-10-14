#ifndef TIMER_TIMER_H
#define TIMER_TIMER_H

#include <cstdlib>
#include <chrono>

class Timer
{
private:
    std::size_t no_measurements;
    std::chrono::time_point<std::chrono::system_clock> m_start;
public:
    Timer();
    void start(const char* title);
    void stop();
};

#endif //TIMER_TIMER_H
