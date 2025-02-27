import java.util.concurrent.Semaphore

// Declare semaphores for stations
// This tells the car if it can enter the station
Semaphore station0 = new Semaphore(1)     // Blast
Semaphore station1 = new Semaphore(1)     // Rinse
Semaphore station2 = new Semaphore(1)     // Dry

// List of semaphores for machines
// This tells the car station whether it has permission to start operating
permToProcess = [new Semaphore(0), new Semaphore(0), new Semaphore(0)]

// This tells the car if it has permission to leave the station
doneProcessing = [new Semaphore(0), new Semaphore(0), new Semaphore(0)]

10.times {
    Thread.start { // Car
        // Go to station 0
        station0.acquire();         // Try to go into station 0
        permToProcess[0].release();    // Tell car station they can work on car
        doneProcessing[0].acquire();   // Wait for car to finish processing
        station0.release();

        // Move on to station 1
        station1.acquire();         // Try to go into station 1
        permToProcess[1].release();    // Tell car station they can work on car
        doneProcessing[1].acquire();   // Wait for car to finish processing
        station1.release();

        // Move on to station 2
        station2.acquire();         // Try to go into station 2
        permToProcess[2].release();    // Tell car station they can work on car
        doneProcessing[2].acquire();   // Wait for car to finish processing
        station2.release();
    }
}

3.times {
    int id = it; // Iteration var; AKA the station #
    Thread.start {
        while (true) {
            // Wait for car to arrive
            permToProcess[id].acquire();
                // Process car when it has arrived
            print("Station " + id + " processing car right now.\n");
            doneProcessing[id].release();
        }
    }
}

return
