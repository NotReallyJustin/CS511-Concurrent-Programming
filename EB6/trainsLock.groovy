import java.util.concurrent.locks.*

class TrainStation {

    // The power of locks! We can lock both tracks seperately
    // This is kind of like a Semaphore, but you just lock the entire thread when needed
    Lock trackNS = new ReentrantLock();
    boolean hasTrainNS = false;                     // Var to track whether NS has a train
    Condition nsTrackFree = trackNS.newCondition();       // Create a signal to fire when NS track is free

    Lock trackSN = new ReentrantLock();   
    boolean hasTrainSN = false;                    // Var to track whether track SN has a train
    Condition snTrackFree = trackSN.newCondition();      // Create a signal to fire when SN track is tree

    void acquireNorthTrackP() {
        // Lock the thread. We split it into 2 locks so what happens in NS doesn't affect what happens in SN
        // But yeah! Only one NS thread can be operating at once
        // Lock the NS track
        trackNS.lock();

        try
        {
            while (hasTrainNS)          // If track NS is occupied, wait
            {
                nsTrackFree.await();
            }
             print("Passenger Train acquiring NS...\n");
            hasTrainNS = true;
        }
        finally
        {
            trackNS.unlock();
        }
    }

    void releaseNorthTrackP() {
        // Lock the thread. Because hasTrainNS is a shared resource
        trackNS.lock();

        try
        {
             print("Passenger Train freeing NS...\n");
            hasTrainNS = false;         // There's no more trains on the track
            nsTrackFree.signal();           // Signal that
        }
        finally
        {
            trackNS.unlock();
        }
    }

    void acquireSouthTrackP() {
        trackSN.lock();

        try
        {
            while (hasTrainSN)          // If track SN is occupied, wait
            {
                 print("Passenger Train acquiring SN...\n");
                snTrackFree.await();
            }
            hasTrainSN = true;
        }
        finally
        {
            trackSN.unlock();
        }
    }

    void releaseSouthTrackP() {
        trackSN.lock();

        try
        {
            print("Passenger Train freeing SN...\n");
            hasTrainSN = false;         // There's no more trains on the track
            snTrackFree.signal();           // Signal that
        }
        finally
        {
            trackSN.unlock();
        }
    }

    void acquireTracksF() {
        trackNS.lock();     // Acquire a lock on NS track
        print("Freight train wants to dock!\n");

        try
        {
            while (hasTrainNS)
            {
                nsTrackFree.await();            // Wait for NS Track to be free
            }
            print("NS pog\n")
            
            // Acquire a lock on SN track too  
            // We have to do this seperately or else we deadlock when both acquireTracksF gets ran
            // nsTrackFree.await() frees a lock... which immediately gets acquired by another acquireTracksF()
            // Now we deadlock because trackSN.lock() is a thing, and we didn't get to free that 
            // since we're stuck in `while (hasTrainNS)` while loop.

            // Moving this down solves the deadlock problem 
            // trackNS.lock() would be freed. trackNS.lock() would be both be temp unlock'd and you get an
            // unofficial "deadlock" since only one thread gets woken up at once when track NS is free
            // That free thread officially occupies trackNS as it waits for trackSN

            // If this explanation is complicated, think about it this way:
            // Our train acquires track NS. After it **successfully** (emphasis on successfully) does it,
            // it then tries to acquire track NS
            trackSN.lock();             
            
            while (hasTrainSN)
            {
                snTrackFree.await();            // Wait for SN Track to free
            }
            print("Freight train incoming! Reserving both tracks\n");

            hasTrainNS = true;
            hasTrainSN = true;
        }
        finally
        {
            trackNS.unlock();
            trackSN.unlock();       // Unlock the stuff
        }
    }

    void releaseTracksF() {
        trackNS.lock();     // Acquire a lock on NS track
        trackSN.lock();     // Acquire a lock on SN track too because shared resource

        try
        {
            hasTrainNS = false;
            hasTrainSN = false;

            print("Freight Train leaving...\n");

            snTrackFree.signal();
            nsTrackFree.signal();       // Signal that the tracks are free
        }
        finally
        {
            trackNS.unlock();
            trackSN.unlock();       // Unlock the stuff
        }
    }
}

TrainStation s = new TrainStation()

30.times {
    Thread.start { // Passenger Train going North
        s.acquireNorthTrackP()
        println "NPT " + Thread.currentThread().getId()
        s.releaseNorthTrackP()
    }
}

30.times {
    Thread.start { // Passenger Train going South
        s.acquireSouthTrackP()
        println "SPT " + Thread.currentThread().getId()
        s.releaseSouthTrackP()
    }
}

10.times {
    Thread.start { // Freight Train
        s.acquireTracksF()
        println "FT " + Thread.currentThread().getId()
        s.releaseTracksF()
    }
}
