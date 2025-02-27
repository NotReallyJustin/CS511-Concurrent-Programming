import java.util.concurrent.Semaphore

// Permission to travel up and down the road
// Grant permission to travel up and down the road
Semaphore permTravel = new Semaphore(1);

// NYC decides to regulate congestion! Now only 3 cars can be on the road at once
Semaphore congestionPricing = new Semaphore(3);

// Semaphore to make sure noOfCarsCrossing isn't being race condition'd
endpointMutex = [new Semaphore(1), new Semaphore(1)]

// Number of cars in each crossing path
noOfCarsCrossing = [0, 0]
r = new Random()

// Start car in random direction
40.times {
    int myEndpoint = r.nextInt(2) // pick a random direction, 0 or 1

    Thread.start { // Car
        // Try to enter the car lane. 
        // Congestion pricing! Only 3 cars at once
        congestionPricing.acquire();    

        endpointMutex[myEndpoint].acquire();    // Start w/ mutex bc shared variable sus

        // If you're the first car trying to enter the path, ask for permission to travel
        if (noOfCarsCrossing[myEndpoint] == 0)
        {
            permTravel.acquire();
        }
        noOfCarsCrossing[myEndpoint]++; // Increment

        endpointMutex[myEndpoint].release();

        // Cross the road
        print("Car is crossing the road in direction " + myEndpoint + ".\n");

        // exit protocol
        endpointMutex[myEndpoint].acquire();        // Critical Section
        
        congestionPricing.release();        // Car no longer on road
        noOfCarsCrossing[myEndpoint]--;
        // If last car crossing the path, give permission to travel to other side
        if (noOfCarsCrossing[myEndpoint] == 0)
        {
            permTravel.release();
        }

        endpointMutex[myEndpoint].release();
    }
}
