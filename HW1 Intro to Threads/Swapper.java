public class Swapper implements Runnable {
    private int offset;
    private Interval interval;
    private String content;
    private char[] buffer;

    /**
     * Creates a swapper to swap around the read input content so we can print it out later
     * @param interval The interval in the string content to swap
     * @param content The string content
     * @param buffer The actual buffer to write the output to
     * @param offset The offset to start writing in the buffer
     */
    public Swapper(Interval interval, String content, char[] buffer, int offset) {
        this.offset = offset;
        this.interval = interval;
        this.content = content;
        this.buffer = buffer;
    }

    @Override
    public void run() {
        // TODO: Implement me!
        int currOffset = offset;        // Current offset to write to

        // Loop through all characters within the interval
        for (int i = this.interval.getX(); i < this.interval.getY(); i++)
        {
            // And print that in buffer
            this.buffer[currOffset] = this.content.charAt(i);
            currOffset++;
        }
    }
}