// Coffee bar! Remember the rules:
// Patron set max bar size. Then, they all leave together
// Ignore fairness and starvation

class CoffeeBar {
    private final int N // Bar size
    private int c = 0  // Patron counter
    private boolean full = false // Bar is full flag

    CoffeeBar(int size) {
        this.N = size
    }

    synchronized void enter() {
        while (full)            // If the bar is full, wait for EVERYONE to leave
        {
            wait();
        }

        c++;
        print("Bar patron enters! Patron Counter: " + c + ".\n");

        if (c == N)             // If you're the last one in a seat, leave
        {
            full = true;
            print("Bar full! No one can get in now.\n");
        }
    }

    synchronized void leave() {
        c--;
        print("Bar patron leaves! Patron Counter: " + c + ".\n");

        if (c == 0)         // Wait for everyone else to leave before declaring bar empty
        {
            full = false;
            print("Bar empty! Letting people in now.\n");
        }
        notifyAll();                // Notify other threads (read: the entry one) to check on their patrons
    }
}

CoffeeBar cb = new CoffeeBar(3)

20.times {
    Thread.start { // Patron
        cb.enter()
        cb.leave()
    }
}
