import java.util.concurrent.Semaphore

// One-time use barrier
// Barrier size = N
// Total number of threads in the system = N

final int N = 3

// Declare semaphores and other variables here
int numPeopleAtBarrier = 0;
Semaphore mutex = new Semaphore(1);     // Critical Section to prevent people at barrier from being wonky
Semaphore permToEnter = new Semaphore(0);

N.times {
    int id = it
    Thread.start {
        // Barrier arrival protocol - enter critical section since will fuck with numpeopleAtBarrier
        mutex.acquire();

        numPeopleAtBarrier++;

        // If you're the last person at the barrier, activate turnstile
        if (numPeopleAtBarrier == N)
        {
            permToEnter.release();
        }

        mutex.release();

        println id + " got to barrier. Waiting for the other threads"

        // Wait at the barrier until they can go :)
        permToEnter.acquire();
        println id + " went through."
        permToEnter.release();          // Turnstile! Once you activate it, it's like the MTA subways
                                        // One guy goes in --> frees slot --> allows other guy to go in
    }
}
