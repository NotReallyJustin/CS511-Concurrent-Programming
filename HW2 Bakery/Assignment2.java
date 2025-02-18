/*
    Justin Chen
    I pledge my honor that I have abided by the Stevens Honor System
*/

/* start the simulation */
public class Assignment2 {

    // Entry point to code
    public static void main(String[] args) {
        Thread thread = new Thread(new Bakery());
        thread.start();

        try {
            thread.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
