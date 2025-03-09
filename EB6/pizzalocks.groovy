import java.util.concurrent.locks.*;

class Pizza {
    private int smallPizza;
    private int largePizza;  

    // Use a lock to make things exclusive
    Lock l = new ReentrantLock();
    Condition smallAvailable = l.newCondition();
    Condition largeOrTwoSmallAvailable = l.newCondition();

    Pizza()
    {
        smallPizza = 0;
        largePizza = 0;
    }

    void purchaseSmallPizza() {
        l.lock();       // Lock the function
        try
        {
            // If there's no small pizzas, we have to wait for it
            while (smallPizza == 0)
            {
                smallAvailable.await();
            }

            print("Buying a small pizza...\n");
            smallPizza--;
        }
        finally
        {
            l.unlock();
        }
    }

    void purchaseLargePizza() {
        l.lock();
        try
        {
            // If we can't buy a large pizza or 2 small pizzas, wait until we can
            while (largePizza == 0 && smallPizza < 2)
            {
                largeOrTwoSmallAvailable.await();
            }

            if (largePizza > 0)
            {
                print("Buying a large pizza...\n");
                largePizza--;
            }
            else
            {
                print("Buying 2 small pizzas...\n");
                smallPizza -= 2;
            }
        }
        finally
        {
            l.unlock();
        }
    }

    void bakeSmallPizza() {      
        // We are putting all 4 functions under locks
        // smallPizza and largePizza are shared resources. Lock is our way of strong-arming them to synchronize.
        // If ANY thread is modifying smallPizza or largePizza, you MUST make sure they go in a critical section. Synchronize keyword is not enough
        // Also ++ is not atomic.
        l.lock();       
        try
        {
            smallPizza++;
            smallAvailable.signal();            // Pizza shop owner lets people know small pizza is available

            if (smallPizza >= 2)                // if we have 2 small pizzas, let the large people know
            {
                largeOrTwoSmallAvailable.signal();
            }
        }
        finally
        {
            l.unlock();
        }  
        
    }

    void bakeLargePizza() {
        l.lock();
        try
        {
            largePizza++;
            largeOrTwoSmallAvailable.signal();          // If we have a large pizza, let the people know
        }
        finally
        {
            l.unlock();
        }
        
    }
}

Pizza pizzaria = new Pizza();

// If this simulation is right, it should end with 20 ppl buying one small pizza, 5 buying large, and 5 buying two small pizzas
30.times {
    Thread.start {  // Small Pizza Baker
        pizzaria.bakeSmallPizza();
    }
}

5.times {
    Thread.start {  // Big Pizza Baker
        pizzaria.bakeLargePizza();
    }
}

20.times {
    Thread.start {  // Small Pizza Consumer
        pizzaria.purchaseSmallPizza();
    }
}

10.times {
    Thread.start {  // Big Pizza Consumer
        pizzaria.purchaseLargePizza();
    }
}