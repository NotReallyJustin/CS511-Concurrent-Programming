import java.util.concurrent.Semaphore;

Semaphore resource = new Semaphore(1);
int cats = 0;   // Current # cats and dogs that are eating
int dogs = 0;

// Onluy cats XOR Dogs can eat
// This mutex might not be right - check later cos race conditions might happen

numCat.times {      // Cats
    Thread.start {
        mutex.acquire(); cats++; mutex.release();   // Lock in critical section b/c r++ isn't concurrent

        if (cats == 1)  // If this is the first cat eating, grab the resource for everyone
        {
            resource.acquire();
        }
        
        mutex.acquire(); cats --; mutex.release();

        if (cats == 0)  // If this is the last cat eating, release the resource for everyone
        {
            resource.release();
        }
    }
}

numDogs.times {     // Dogs
    Thread.start {
        mutex.acquire(); dogs++; mutex.release();
        if (dogs == 1)  // If this is the first dog eating, grab the resource for everyone
        {
            resource.acquire();
        }

        // Eat
        mutes.acquire(); dogs--; mutex.release();

        // If this is the last dog that's eating, release the resource for everyone
        if (dogs == 0)
        {
            resource.release();
        }
    }
}