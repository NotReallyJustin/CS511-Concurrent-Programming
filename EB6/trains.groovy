import java.util.concurrent.locks.*

class TrainStation {
    
    // There's two tracks - North South and South North
    // Passenger trains can reserve either one

    private boolean trackNSReserved = false;
    private boolean trackSNReserved = false;

    // The freight train wants both tracks. We're just going to reserve trackNS and trackSN.
    // This var is to establish mutex so when freight trains and passenger trains are competing, only one gets it
    private boolean wantFreight = false;

    synchronized void acquireNorthTrackP() {
        // If track already reserved or if the freight train wants it, wait
        while (trackNSReserved || wantFreight)
        {
            wait();
        }

        trackNSReserved = true;
        print("Passenger train acquiring north track\n");
        notifyAll();                // Tell the world we reserved the track
    }

    synchronized void releaseNorthTrackP() {
        trackNSReserved = false;
        print("Passenger train releasing north track\n");
        notifyAll();                // Notify train gone
    }

    synchronized void acquireSouthTrackP() {
        // If track alr reserved or freight wants it, wait
        while (trackSNReserved || wantFreight)
        {
            wait();
        }

        trackSNReserved = true;      
        print("Passenger train acquiring south track\n");
        notifyAll();            // Tell the world we reserved the track
    }

    synchronized void releaseSouthTrackP() {
        trackSNReserved = false;
        print("Passenger train releasing south track\n");
        notifyAll();
    }

    synchronized void acquireTracksF() {
        // Again, reserve both tracks to unload
        print("Freight train! Reserving both tracks...\n");
        wantFreight = true;             // prevents mutex like we said earlier
        notifyAll();                // Notify all wantFreight has updated

        while (trackNSReserved)
        {
            wantFreight = true;         // In case we deadlock or smth. Prevent wantFreight from getting overridden
            wait();
        }
        trackNSReserved = true;
        notifyAll();                // Notify all threads in case

        while (trackSNReserved)
        {
            wantFreight = true;
            wait();
        }
        trackSNReserved = true; 

        print("Loading freight!\n");
        notifyAll();                    // Notify all threads other track is reserved
    }

    synchronized void releaseTracksF() {

        wantFreight = false;
        trackNSReserved = false;
        trackSNReserved = false;

        notifyAll();                // Release everything and tell everyone
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
