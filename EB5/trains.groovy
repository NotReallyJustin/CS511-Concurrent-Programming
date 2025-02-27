import java.util.concurrent.Semaphore

// Freight trains can only leave when the loading machine is done working on it. 
// This is how we let the freight train know it can leave
Semaphore doneLoading = new Semaphore(0);

// The loading machine needs to know when it can start operating.
// This is how we inform the machine about the status.
Semaphore permToLoad = new Semaphore(0);

// Permission for passenger train to dock at a track.
// The tracks are N --> S (0) and S --> N (1). Makes sense that you only have 1 train per track
permToDock = [new Semaphore(1), new Semaphore(1)]

20.times {
    int dir = (new Random()).nextInt(2)

    Thread.start { // Passenger Train travelling in direction dir
        permToDock[dir].acquire();
        print("A train is docking at direction " + dir + ".\n");
        permToDock[dir].release();
    }
}

20.times {
    int dir = (new Random()).nextInt(2)

    Thread.start { // Freight Train travelling in direction dir
        
        // To load one of these things, a freight train needs both tracks to be empty. Hence, we grab both tracks
        permToDock[0].acquire();
        permToDock[1].acquire();
        print("Clearing the tracks for a freight train!\n");
        // Now we can tell the machine to operate!
        permToLoad.release();
        
        // Wait for machine to be done loading
        doneLoading.acquire();
        print("Freight train done. Releasing tracks...\n");
        // Be on our merry way. Release the tracks
        permToDock[0].release();
        permToDock[1].release();
    }
}

Thread.start { // Loading Machine
    while (true) {
        permToLoad.acquire()
        print("Loading Machine is loading freight train\n");
        doneLoading.release()
    }
}
