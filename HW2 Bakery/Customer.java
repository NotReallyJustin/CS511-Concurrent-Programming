/*
    Justin Chen
    I pledge my honor that I have abided by the Stevens Honor System
*/

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.concurrent.CountDownLatch;

/**
 * Simulates a customer in the store.
 */
public class Customer implements Runnable {

    /**
     * The bakery the customer is in. This will be used to enforce semaphores later
     */
    private Bakery bakery;

    /**
     * Random object
     */
    private Random rnd;

    /**
     * Shopping list of bread the customer wants to buy
     */
    private List<BreadType> shoppingList;

    /**
     * How much time the customer will be shopping for. I guess this is like a realistic simulation of them shopping
     */
    private int shopTime;

    /**
     * How much time the customer will be checking out at the cashier for. I guess this is like a realistic simulation
     * of them shopping
     */
    private int checkoutTime;

    /**
     * The countdown latch that's we will decrement to ensure all customers have finished shopping
     */
    private CountDownLatch doneSignal;
    
    /**
     * Initialize a customer object and randomize its shopping list
     * @param bakery The Bakery the customer is in. This is needed later to enforce the semaphores
     * @param l The countdown latch to decrement later
     */
    public Customer(Bakery bakery, CountDownLatch l) {
        this.bakery = bakery;
        this.rnd = new Random();

        this.shoppingList = new ArrayList<BreadType>();
        this.fillShoppingList();

        // checkoutTime and shopTime is randomized so we don't know what's gonna happen
        // We weren't given an upper bound so I'll have them take 0-200 milliseconds
        // Sorry I'm hella impatient so I guess today is Black Friday
        this.shopTime = this.rnd.nextInt(1, 201);
        this.checkoutTime = this.rnd.nextInt(1, 201);

        this.doneSignal = l;
    }

    /**
     * Run tasks for the customer
     * Basically, buy all the bread.
     * Print when the customer starts shopping, takes an item, buys an item, and finishes shopping
     */
    public void run() {
        System.out.println("Customer " + hashCode() + " has begun shopping.");

        // Buy all bread
        for (BreadType breadType : this.shoppingList)
        {
            // Wrap in try catch loop because turns out Java hates exceptions and I can't
            // throw Exceptions in run() b/c inheritance or some dumb stuff like that

            try
            {
                // Acquire semaphore
                this.bakery.getShelves().get(breadType).acquire();

                System.out.println("Customer " + hashCode() + " is taking a piece of " + breadType + " off the shelves.");
                Thread.sleep(this.shopTime);
                this.bakery.takeBread(breadType);

                // Release semaphore
                this.bakery.getShelves().get(breadType).release();
            }
            catch(Exception err)
            {
                System.out.println(err.toString());
            }
        }

        // Go to cashier
        // Try catch because acquire() loves throwing errors :((
        try
        {
            // Acquire Semaphore
            this.bakery.getCheckouts().acquire();

            System.out.println("Customer " + hashCode() + " is now checking out.");
            Thread.sleep(this.checkoutTime);

            // 🛑 We are updating the sales variable!!!
            // Because we're not slapping the atomic keyword on it, it goes in a critical section
            // That means we're acquiring a Semaphore for it
            this.bakery.getPermissionToAccessSales().acquire();

            // Add total checkout price to the bakery's sales
            float totalPrice = this.getItemsValue();
            this.bakery.addSales(totalPrice);

            this.bakery.getPermissionToAccessSales().release();

            // Back to main thread - customer is done shopping. Release the semaphore
            this.bakery.getCheckouts().release();
        }
        catch(Exception err)
        {
            System.out.println(err.toString());
        }

        System.out.println("Customer " + hashCode() + " has finished shopping.");

        // Decrement latch
        // 🤨 This guy on StackOverflow is swearing up and down that this is atomic so we're going to trust them
        // @see https://stackoverflow.com/questions/16966138/is-java-util-concurrent-countdownlatch-countdown-atomic
        this.doneSignal.countDown();
    }

    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingList=" + Arrays.toString(shoppingList.toArray()) + ", shopTime=" + shopTime + ", checkoutTime=" + checkoutTime;
    }

    /**
     * Add a bread item to the customer's shopping list
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingList.size() >= 3) {
            return false;
        }
        shoppingList.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping list with 1 to 3 random breads
     */
    private void fillShoppingList() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping list
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingList) {
            value += bread.getPrice();
        }
        return value;
    }
}
