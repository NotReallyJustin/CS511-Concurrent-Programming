import java.util.Random
import java.util.concurrent.locks.*

class Grid {
    // How many people are powering the grid. 1 producer should == 1 consumer
    private int numProducer;
    private int numConsumer;

    // Maximum # of producers allowed at once
    private int maxProducers;
    private int wantStopProduce;        // Track how many producers want to stop producing

    // Use this to lock certain functions
    Lock lock = new ReentrantLock();
    Condition canProduce = lock.newCondition();
    Condition canConsume = lock.newCondition();

    // Lock on $wantStopProduce to make sure only one thing can modify it at once
    Lock raceCondLock = new ReentrantLock();

    public Grid(int N) {
        numConsumer = 0;
        numProducer = 0;

        maxProducers = N;
        wantStopProduce = 0;
    }

    void startConsuming() {
        lock.lock();            // We have to lock this. Updating a shared variable

        try
        {                                               // 3. Prioritize leaving of producers over entry of consumers
            while (numProducer < (numConsumer + 1) || wantStopProduce != 0)
            {
                canConsume.await();             // If consuming would create more consumers than producers, wait until someone backs off.
            }

            numConsumer++;
            print("We started consuming! Producers: " + numProducer + ". Consumers: " + numConsumer + ".\n");
        }
        finally
        {
            lock.unlock();
        }
    }

    void stopConsuming() {
        lock.lock();                // Again, updating shared variables! Lock this too

        try
        {
            numConsumer--;
            print("We stopped consuming! Producers: " + numProducer + ". Consumers: " + numConsumer + ".\n");
            canConsume.signal();            // Someone stopped consuming - maybe that means someone can start producing
        }
        finally
        {
            lock.unlock();
        }
    }

    void startProducing() {
        lock.lock();                // Updating shared variables - lock this

        try
        {
            if ((numProducer + 1) > maxProducers)
            {
                canProduce.await();         // If we're going to have too many producers, wait until you get the permissions
            }

            numProducer++;
            print("We started producing! Producers: " + numProducer + ". Consumers: " + numConsumer + ".\n");
            canConsume.signal();            // We got one more producer - maybe this means someone can consume now
        }
        finally
        {
            lock.unlock();
        }
    }

    void stopProducing() {

        // We're about to update wantStopProducers to reflect that someone wants to stop producing.
        raceCondLock.lock();
        try
        {
            wantStopProduce++;
        }
        finally
        {
            raceCondLock.unlock();
        }

        lock.lock();                // Accessing a shared variable

        // We're about to remove someone from wantStopProduce since we finished taking care of their request
        raceCondLock.lock();
        try
        {
            wantStopProduce--;  // We could signal this now - but we'll signal this at the end
                                // since the producers are done leaving when numProducer--
        }
        finally
        {
            raceCondLock.unlock();
        }

        // Continue on
        try
        {
            numProducer--;
            print("We stopped producing! Producers: " + numProducer + ". Consumers: " + numConsumer + ".\n");
            canProduce.signal();            // We got one less producer - maybe this means someone can produce now
                                            // bc we might not be at max producers
        }
        finally
        {
            lock.unlock();
        }  
    }
}

final int CP = 100
Grid grid = new Grid(60)

CP.times {
    Thread.start {
        switch (new Random().nextInt(2)) {
            case 0:
                grid.startConsuming()
                sleep(500);
                grid.stopConsuming()
                break
            default:
                grid.startProducing()
                sleep(500);
                grid.stopProducing()
        }
    }
}
