// A barrier == Countdown latch
// Think Java! This waits once for all threads to reach the latch
// Once they do, release everything. The floodgates are permanently open
class Barrier {
    private int numToReleaseLatch;

    Barrier(int n)
    {
        numToReleaseLatch = n;
    }

    // You might think we're cooked because only 1 thread can be in here at once. But no :)
    // Once you wait, you release the lock
    synchronized void waitAtBarrier()
    {
        numToReleaseLatch--;
        while (numToReleaseLatch != 0)  // Have the threads wait. While bc spurrious wakeups
        {
            wait();
        }

        if (numToReleaseLatch == 0)     // If we hit zero, release the floodgates!
        {
            notifyAll();            // Tell all monitors to wake tf up
        }
        // Don't need to do anything here; floodgates are alr open
    }
}

Barrier b = new Barrier(3);

Thread.start {      // T1
    print("a\n");
    b.waitAtBarrier();
    print("1\n");
}

Thread.start {      // T2
    print("b\n");
    b.waitAtBarrier();
    print("2\n");
}

Thread.start {      // T3
    print("c\n");
    b.waitAtBarrier();
    print("3\n");
}