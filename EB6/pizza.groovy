class Pizza {
    private int smallPizza;
    private int largePizza;

    // Use this to lock large pizza. We might have a race condition if we don't mutually exclude smallPizza
    private boolean buyingSmallPizza;           

    Pizza()
    {
        smallPizza = 0;
        largePizza = 0;
    }

    synchronized void purchaseSmallPizza() {
        buyingSmallPizza = true;            // Use this to create essentially a Semaphore on LargePizza guys buying 2 small pizza
        while (smallPizza <= 0)             // If there's no pizza, wait.
        {
            wait(); 
        }

        smallPizza--;
        buyingSmallPizza = false;
        print("Buying small pizza. Small pizza: " + smallPizza + ". Large pizza: " + largePizza + ".\n");

        // Due to large pizza guys possibly waiting on small pizzas, we need to notify the threads when a pizza has been purchased.
        notifyAll();                        
    }

    synchronized void purchaseLargePizza() {
        // We will wait if:
        // - There's no large pizza 
        // - AND either there's not enough small pizza to buy OR someone's trying to hog and buy small pizzas
        while (largePizza <= 0 && (smallPizza < 2 || buyingSmallPizza))
        {
            wait();
        }

        // If there is large pizza, buy it. Else, buy small
        if (largePizza > 0)
        {
            print("Buying large pizza. Small pizza: " + smallPizza + ". Large pizza: " + largePizza + ".\n");
            largePizza--;
        }
        else if (smallPizza >= 2)
        {
            print("Buying two small pizzas. Small pizza: " + smallPizza + ". Large pizza: " + largePizza + ".\n");
            smallPizza -= 2;
        }
        else
        {
            print("IDK how but we fucked up.\n")
        }
    }

    // Still gotta synchronize this because incrementing may not be atomic
    synchronized void bakeSmallPizza() {        
        smallPizza++;
        print("Baking small pizza. Small pizza: " + smallPizza + ". Large pizza: " + largePizza + ".\n");
        notifyAll();            // Notify the fellas to let them know pizza is ready
    }

    synchronized void bakeLargePizza() {
        largePizza++;
        print("Baking large pizza. Small pizza: " + smallPizza + ". Large pizza: " + largePizza + ".\n");
        notifyAll();            // Notify the threads to let them know pizza is ready
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