import java.util.concurrent.Semaphore;
import groovy.transform.Field
// Goal: Simulate a IRL ferry between 2 coasts

int capacity = 10;
@Field volatile int currPassengers = 0;

// Binary semaphore letting the Ferry know if they can leave/board
// Since they're at max capacity
Semaphore canLeave = new Semaphore(0);  
Semaphore canBoard = new Semaphore(0);
Semaphore mutexCurrPassengers = new Semaphore(1);

// West and East tickets
Semaphore ticketsBoardEast = new Semaphore(0);
Semaphore ticketsBoardWest = new Semaphore(0);
Semaphore permissionToLeave = new Semaphore(0);

Thread.start { // Ferry
    int coast = 0;              // 0 east, 1 west

    while (true) {
        // allow passengers on
        if (coast == 0)
        {
            ticketsBoardEast.release(capacity);
        }
        else
        {
            ticketsBoardWest.release(capacity);
        }
        
        canLeave.acquire();     // Ask for perms to leave
        print("We are at coast " + coast + " . Max capacity prepare for takeoff.\n");
        coast = 1 - coast;      // move to opposite coast
        
        // allow passengers off
        print("Ship has docked.\n")
        permissionToLeave.release(capacity);

        // wait for all passengers to get off
        canBoard.acquire();
        print("We are now boarding.\n");
    }
}

100.times {
    Thread.start { // Passenger on East coast
        // get on
        ticketsBoardEast.acquire();

        // Register the customer
        // Put mutex cos we're fucking with shared variables
        mutexCurrPassengers.acquire();
        currPassengers++;
        
            // If you're the last customer on board, tell the boat they can leave
        if (currPassengers == capacity)
        {
            canLeave.release();
        }
        mutexCurrPassengers.release();

        // get off at opposite coast. Get permission to leave
        permissionToLeave.acquire();

        // Mutex b/c fucking w/ shared variable
        mutexCurrPassengers.acquire();
        currPassengers--;

            // If last one to leave, tell boat to board
        if (currPassengers == 0)
        {
            canBoard.release();
        }
        mutexCurrPassengers.release();
    }
}

100.times {
    Thread.start { // Passenger on West coast
        // get on
        ticketsBoardWest.acquire();

        mutexCurrPassengers.acquire();
        currPassengers++;
        
        if (currPassengers == capacity)
        {
            canLeave.release();
        }
        mutexCurrPassengers.release();

        // get off at opposite coast. Get permission to leave
        permissionToLeave.acquire();

        // Mutex b/c fucking w/ shared variable
        mutexCurrPassengers.acquire();
        currPassengers--;

            // If last one to leave, tell boat to board
        if (currPassengers == 0)
        {
            canBoard.release();
        }
        mutexCurrPassengers.release();
    }
}

return;
