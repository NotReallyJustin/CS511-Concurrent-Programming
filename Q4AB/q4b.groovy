import java.util.concurrent.Semaphore
// When truck filling, no one else can fuel gas station

// Declare additional semaphores and variables here
Semaphore gasUp = new Semaphore(6, true);

final int NC = 100 // Number of cars
final int NT = 10  // Number of trucks

NC.times {
    Thread.start { // Vehicle
        gasUp.acquire();
        print("Car refueling...\n");
        gasUp.release();
    }
}

NT.times {
    Thread.start { // Truck
        // atomic {
        //     gasUp.acquire()
        //     gasUp.acquire()
        //     gasUp.acquire()
        //     gasUp.acquire()
        //     gasUp.acquire()
        //     gasUp.acquire()
        // }
       gasUp.acquire(6);                // Acquire all gas slots
       print("Trucks refueling...\n");
       gasUp.release(6);
    }
}
