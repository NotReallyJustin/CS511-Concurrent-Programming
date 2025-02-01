public class Interval {

    private int x;
    private int y;

    /**
     * Creates an interval
     * @param x Start index (inclusive)
     * @param y End index (exclusive)
     */
    public Interval(int x, int y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Get the start index
     * @return Start index (inclusive)
     */
    public int getX() {
        return this.x;
    }

    /**
     * Get the end index
     * @return End index (exclusive)
     */
    public int getY() {
         return this.y;
    }

    public String toString() {
        return "(" + this.x + ", " + this.y + ")";
    }
}