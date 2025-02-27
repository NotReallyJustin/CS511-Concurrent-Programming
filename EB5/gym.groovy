import java.util.concurrent.Semaphore

MAX_WEIGHTS = 10        // Weights as in # of disks, and not lbs
GYM_CAP = 4

// Declare semaphores here
Semaphore permToEnterGym = new Semaphore(GYM_CAP);
Semaphore weights = new Semaphore(MAX_WEIGHTS);     // Acquire a weight to use it
Semaphore mutex = new Semaphore(1);
permToUseMachine = [new Semaphore(1), new Semaphore(1), new Semaphore(1), new Semaphore(1)];

// Returns a random routine
// We don't need to care about this function
def make_routine(int no_exercises) { 
    Random rand = new Random()
    int size = rand.nextInt(no_exercises)
    def routine = []

    size.times {
        routine.add([rand.nextInt(4), rand.nextInt(MAX_WEIGHTS)])
    }
    return routine
}

10.times { id ->
    Thread.start { // Client
        def routine = make_routine(20) // Random routine of 20 exercises

        // ⭐ Enter gym logic here
        permToEnterGym.acquire();

        // Performing exercise
        routine.size().times { i ->
            int machine = routine[i][0];
            int weight = routine[i][1];

            // Prevent a mutex - enter critical section here or else we get a deadlock
            // where Josh Allen grabbed machine A but needs 10 weight and Nick Bosa grabbed 4 
            // weight and needs machine A or smth like that
            mutex.acquire();
            permToUseMachine[machine].acquire();
            weights.acquire(weight);
            mutex.release();

            // ⭐ Enter gym logic here
            println "$id is performing: exercise ${routine[i][0]} -- weight ${routine[i][1]}"

            // Exercise done - release the machines
            permToUseMachine[machine].release();
            weights.release(weight);
        }

        // Check out gym membership
        permToEnterGym.release();
    }
}

return
