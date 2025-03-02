import java.util.Random

class Grid {
    // How many people are powering the grid. 1 producer should == 1 consumer
    private int numProducer;
    private int numConsumer;

    // Maximum # of producers allowed at once
    private int maxProducers;
    private boolean producingStopped;

    public Grid(int N) {
        numConsumer = 0;
        numProducer = 0;

        maxProducers = N;
        producingStopped = false;
    }

    synchronized void startConsuming() {
        while (producingStopped)                    // Task 3: Prioritize exiting producer over entry consumer
        {
            wait();
        }

        while (numProducer < (numConsumer + 1))         // If we're gonna have insufficient producers, wait
        {
            wait();
        }
        numConsumer++;
        print("We started consuming! Producers: " + numProducer + ". Consumers: " + numConsumer + ".\n");
        notifyAll();                // Wake all other stuff up to check conditions
    }

    synchronized void stopConsuming() {
        numConsumer--;
        print("We stopped consuming! Producers: " + numProducer + ". Consumers: " + numConsumer + ".\n");
        notifyAll();
    }

    synchronized void startProducing() {
        while (numProducer > maxProducers)      // Clause 2: Limit to N producers
        {
            wait();
        }
        numProducer++;
        print("We started producing! Producers: " + numProducer + ". Consumers: " + numConsumer + ".\n");
        notifyAll();
    }

    synchronized void stopProducing() {
        producingStopped = true;
        while ((numProducer - 1) < numConsumer)     // Clause 1: Can't stop producing if it leaves consumers straggling
        {
            wait();
        }

        numProducer--;
        print("We stopped consuming! Producers: " + numProducer + ". Consumers: " + numConsumer + ".\n");
        producingStopped = false;                   // Reset priority - we no longer need it
        notifyAll();
    }
}

final int CP = 100
Grid grid = new Grid(60)

CP.times {
    Thread.start {
        switch (new Random().nextInt(2)) {
            case 0:
                grid.startConsuming()
                grid.stopConsuming()
                break
            default:
                grid.startProducing()
                grid.stopProducing()
        }
    }
}
