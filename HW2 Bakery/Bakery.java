/*
    Justin Chen
    I pledge my honor that I have abided by the Stevens Honor System
*/

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Semaphore;
import java.util.concurrent.CountDownLatch;

/**
 * Represents an instance of a Bakery. Basically, the day in the life of a bakery
 */
public class Bakery implements Runnable {
    /**
     * Total number of customers we're going to have today. Used in CountdownLatch. We're spawning this much stuff
     */
    private static final int TOTAL_CUSTOMERS = 200;

    /**
     * Maximum capacity of Bakery. This should be maintained with a semaphore
     */
    private static final int CAPACITY = 50;

    /**
     * How much of each type of bread we have when we start the day.
     * This automatically restocks so :))
     */
    private static final int FULL_BREAD = 20;

    /**
     * ConcurrentHashMap of # of available bread we have.
     * This isn't atomic. We don't need to make it volatile either because we're not updating the variable itself
     * Semaphore this
     */
    private Map<BreadType, Integer> availableBread;

    /**
     * Spawns thread pools.
     * I feel like we could limit # of customers with this. If we only spawn 50 threads at once,
     * that means we only have 50 customers at once
     */
    private ExecutorService executor;

    /**
     * Maintains how much money we've made. Semaphore this.
     * I REALLY don't like the fact that this isn't volatile or atomic. But apparently Java enforces
     * happen-before relationships when you unlock() a semaphore so :shrug:
     */
    private float sales = 0;

    /**
     * Use a countdown latch to essentially `.join()` all 200 threads
     */
    private CountDownLatch doneSignal = new CountDownLatch(TOTAL_CUSTOMERS);

    /**
     * I got really mad at trying to find this magic number 4 so it's a variable now
     * I'm so mad it's getting the final keyword
     */
    private final int NUM_CHECKOUTS = 4;

    // TODO --> Declare Semaphores here

    /**
     * A semaphore on all the shelves. Each bread shelf can only be used by one customer.
     */
    private Map<BreadType, Semaphore> shelves;

    /**
     * A semaphore on checkouts. Each checkout can only serve 1 customer b/c there's only 1 cashier.
     */
    private Semaphore checkouts;

    private Semaphore permissionToAccessSales;

    /**
     * Remove a loaf from the available breads and restock if necessary
     */
    public void takeBread(BreadType bread) {
        int breadLeft = availableBread.get(bread);
        if (breadLeft > 0) {
            availableBread.put(bread, breadLeft - 1);
        } else {
            System.out.println("No " + bread.toString() + " bread left! Restocking...");
            // restock by preventing access to the bread stand for some time
            try {
                Thread.sleep(1000);
            } catch (InterruptedException ie) {
                ie.printStackTrace();
            }
            availableBread.put(bread, FULL_BREAD - 1);
        }
    }

    /**
     * Add to the total sales
     */
    public void addSales(float value) {
        sales += value;
    }

    /**
     * Gets access to the store's shelves
     * @return this.shelves
     */
    public Map<BreadType, Semaphore> getShelves()
    {
        return this.shelves;
    }

    /**
     * Gets access to a checkout
     * @return this.checkouts
     */
    public Semaphore getCheckouts()
    {
        return this.checkouts;
    }

    /**
     * Gets the **variable** for permission to access sales. You still need to acquire() the actual permissions.
     * @return this.permissionToAccessSales
     */
    public Semaphore getPermissionToAccessSales()
    {
        return this.permissionToAccessSales;
    }

    /**
     * Run all customers in a fixed thread pool.
     * Simulate the store.
     */
    public void run() {
        availableBread = new ConcurrentHashMap<BreadType, Integer>();
        availableBread.put(BreadType.RYE, FULL_BREAD);
        availableBread.put(BreadType.SOURDOUGH, FULL_BREAD);
        availableBread.put(BreadType.WONDER, FULL_BREAD);

        // TODO
        // Initialize the Semaphores
        this.shelves = new ConcurrentHashMap<>();
        for (BreadType bread : BreadType.values()) {
            // I think you need to make these fair Semaphores or else you'll starve threads
            // Since we have more than 2 threads
            this.shelves.put(bread, new Semaphore(1, true));
        }

        this.checkouts = new Semaphore(this.NUM_CHECKOUTS, true);
        this.permissionToAccessSales = new Semaphore(1, true);

        // Create a threadPool with CAPACITY
        this.executor = Executors.newFixedThreadPool(CAPACITY);

        // Create all the customers inside a ThreadPool
        for (int i = 0; i < TOTAL_CUSTOMERS; i++)
        {
            Customer customerThread = new Customer(this, this.doneSignal);
            this.executor.execute(customerThread);
        }

        // Wait for countdownlatch to reach 0
        // Of course this would throw a ****ing error 😡
        // Well done Java thanks for being annoying
        try
        {
            this.doneSignal.await();
        }
        catch(Exception err)
        {
            System.out.println(err.toString());
        }

        // Shut down the executor b/c we don't any more processes
        this.executor.shutdown();

        // Calculate total profit for the day
        System.out.println("----------------------------------");
        System.out.printf("Total sales = %.2f\n", this.sales);
    }
}
