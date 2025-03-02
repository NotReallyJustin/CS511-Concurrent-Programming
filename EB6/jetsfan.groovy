class Bar {
    private int numJets;
    private int numPatriots;
    private boolean itGotLate;

    // Declare a bar class
    Bar()
    {
        numJets = 0;
        numPatriots = 0;            // 1 jets for 2 patriots
        itGotLate = false;
    }

    // Jets go in
    // We're accessing a shared resource! That's why you put a lock on this function
    // The good thing about monitors is that because the function is exclusive, you can write normal stuff and trust it runs in crit section
    synchronized void jets()
    {
                        // If it got late, let everyone in
        while (numJets * 2 > numPatriots && !itGotLate)       // Account for 1 jets for 2 patriots
        {                                       // If we have too many fans, they'll have to wait
            wait();
        }

        numJets += 1;
        
        print("Jets fan in. Jets: " + numJets + ", Patriots: " + numPatriots + "\n");
    }

    // Patriots go in
    synchronized void patriots()
    {
        numPatriots += 1;
        notifyAll();                    // Notify jets fans that are sleeping
        print("Patriots fan in. Jets: " + numJets + ", Patriots: " + numPatriots + "\n");
    }

    // Update variable to let the bar know it's getting
    // You need synchronized to call notifyAll() or wait()
    synchronized void itGotLate()
    {
        print("It's getting late!");
        itGotLate = true;
        notifyAll();                    // Notify everyone so it all can be goober
    }
}

Bar b = new Bar();

100.times {
    Thread.start {  // Jets
        b.jets();
    }
}

100.times {
    Thread.start {  // Patriots
        b.patriots();
    }
}

Thread.start {          // Simulate it getting late
    sleep(1000);
    b.itGotLate();
}