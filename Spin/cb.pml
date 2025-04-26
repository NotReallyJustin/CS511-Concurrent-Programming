// Justin Chen 
// I pledge my honor that I have abided by the Stevens Honor System.
#include "sem_bw.h"

#define N 2       // 2 (resp. 3) - requires setting max_depth to 12000 (resp. 22000)
#define B 2

// Semaphores
byte mutexE = 1;
byte mutexL = 1;
byte barrier = 0;
byte barrier2 = 0;

// Shared state
byte enter = 0;
byte leaving = 0;

// Ghost variables
byte c[N];        // Array for counting cycles
// Declare other ghost variables here, if needed

active [N] proctype P() {
    byte i;
    byte j;
    byte temp;

    for (i : 1..100) {    // Change to 5 when testing bc Promela max
        acquire(mutexE); // Enter critical section
        enter++;

        if
            :: enter == B ->
                // release(barrier, B);
                byte B2 = B;
                for (temp: 1..B2) {
                    release(barrier);
                }

                enter = 0
            :: else -> skip
        fi

        release(mutexE); // Exit critical section

        acquire(barrier); // Wait for barrier

        // PID seems to be 1 or 0 here, so we're going to assume this can be used to index
        printf("%d reached at cycle %d\n", _pid, c[_pid]);

        atomic {
            // Increment counter. This means we've cyclic barrier'd once
            c[_pid]++;

            // Correct here means that no one thread gets “ahead” of the others
            // I guess this means the current thread is > 1 away from other threads

            byte a;
            byte b;
            byte X1 = N;

            for (a : 0..X1-1)
            {
                for (b : 0..X1-1)
                {
                    if
                        :: c[a] > c[b] -> assert(c[a] - c[b] <= 1)
                        :: else -> assert(c[b] - c[a] <= 1)
                    fi
                }
            }
        }
        acquire(mutexL); // Enter critical section
        leaving++;

        if
            :: leaving == B ->
                // release(barrier2, B);
                byte B3 = B;
                for (temp: 1..B3) {
                    release(barrier2);
                }

                leaving = 0
            :: else -> skip
        fi

        release(mutexL); // Exit critical section
        acquire(barrier2); // Wait for barrier2

        printf("%d leaves at cycle %d\n", _pid, c[_pid])
    }
}
