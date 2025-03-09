import java.util.concurrent.locks.*;

class Barrier {
    private int numToReleaseLatch;

    // Create locks
    Lock barrier = new ReentrantLock();
    Condition canGo = barrier.newCondition();   // Signal to the threads when they can go
    int currentNumAtGate = 0;       // Track how many ppl are at gate rn

    Barrier(int n)
    {
        numToReleaseLatch = n;
    }

    void waitAtBarrier()
    {
        // currentNumAtGate is a shared variable. Lock it
        // If you even think it *can* deadlock, lock it
        barrier.lock();

        try
        {
            currentNumAtGate++;

            // If there's not enough people yet, wait at the gate
            while (currentNumAtGate < numToReleaseLatch)
            {
                canGo.await();
            }

            // If you reached this part of the code, there is enough people
            // at the gate! Go!!!
            // Also let everyone know they can leave.

            // This is a cascading signal - signal() only waits one thread up at once
            // but the other threads wake up more threads!
            
            // IRL, this looks like one guy passing through the barrier and recursively letting the guy in the back
            // know the barrier is good to go
            canGo.signal();
        }
        finally
        {
            barrier.unlock();
        }
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