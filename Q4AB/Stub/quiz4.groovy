// Justin Chen + Zakariyya Scavotto
// I pledge my honor that I have abided by the Stevens Honor System

import java.util.concurrent.Semaphore;
final int N = 4; // Capacity of each elevator

// Tells the workers they can board on ith floor. 4 tickets to board each time
permToBoard = [new Semaphore(N), new Semaphore(0)];

// Tells the elevator they can board on the ith floor
// This is used by workers to let elevator know they're on. Once they're all on, elevator can leave
confirmOnBoard = [new Semaphore(0), new Semaphore(0)];

// Worker waits for this permission to leave before they can do the exit protocol
permToLeave = new Semaphore(0);

// Elevator waits for this permission before it starts to board new passengers
gotOff = new Semaphore(0);

// Start thread

2.times {
    Thread.start {  // elevator
        int floor = 0;
        while (true)
        {
            // Allow workers on 
            // Wait until all workers are on before you depart. Acquire all those worker permissions
            confirmOnBoard[floor].acquire(N);

            floor = 1 - floor;      // Move to opposite floor

            // // Release the confirm on boards because there's no one there anymore
            // confirmOnBoard[floor].release(N);

            // Wait for workers to get off
            // Allow N workers to leave now
            permToLeave.release(N);
            
            // Wait until the workers all got off.
            // The elevator will not load new people until everyone got off
            gotOff.acquire(N);

            // Release permissions to board when the elevator is ready
            permToBoard[floor].release(N);
        }
    }
}

100.times {
    Thread.start {      // Worker on 1st floor

        // Worker gets a ticket to board. Once they're on, they give elevator permission to leave.
        permToBoard[0].acquire();
        // print("Worker got on floor 0\n");
        confirmOnBoard[0].release();

        // Get off at opposite floor
        // Get permission to leave.
        permToLeave.acquire();
        // print("Worker got off floor 1\n");

        // Once we leave, tell elevator the guy got off
        gotOff.release();
    }
}

100.times {
    Thread.start {      // Worker on 2nd floor
        permToBoard[1].acquire();
        // print("Worker got on floor 1\n");
        confirmOnBoard[1].release();

        // Get off at opposite floor
        // Get permission to leave.
        permToLeave.acquire();
        // print("Worker got off floor 0\n");

        // Once we leave, tell elevator the guy got off
        gotOff.release();
    }
}