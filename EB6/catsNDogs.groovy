BigBowl bb = new BigBowl();

class BigBowl {
    private int cats;
    private int dogs;

    synchronized void enter_cat()
    {
        while (dogs > 0)
        {
            wait();
        }
        print("Cats in\n");
        cats++;
    }

    synchronized void exit_cat()
    {
        cats--; 
        print("Cats out\n");
        if (cats == 0)  // Readers writers - last cat notifies dogs
        {
            notifyAll();
        }
    }

    synchronized void enter_dog()
    {
        while (cats > 0)
        {
            wait();
        }
        print("Dogs in\n");
        dogs++;
    }

    synchronized void exit_dog()
    {
        dogs--;
        print("Dogs out\n");

        // If you're the last dog, tell the cats they can wake up now
        if (dogs == 0)
        {
            notifyAll();
        }
    }

}
10.times {
    Thread.start {      // Cats
        bb.enter_cat();
        // feed
        bb.exit_cat();
    }
}

10.times {
    Thread.start {          // Dogs
        bb.enter_dog();
        // feed
        bb.exit_dog()
    }
}