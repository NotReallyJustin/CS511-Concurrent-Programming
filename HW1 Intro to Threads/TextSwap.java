import java.io.*;
import java.util.*;

/*
    Justin Chen
    I pledge my honor that I have abided by the Stevens Honor System.
 */

public class TextSwap {

    private static String readFile(String filename, int chunkSize) throws Exception {
        String line;
        StringBuilder buffer = new StringBuilder();
        File file = new File(filename);
        // The "-1" below is because of this:
        // https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline
        if ((file.length()-1) % chunkSize!=0)
        {
            System.out.println("File size should be a multiple of the chunk size");
            throw new Exception("File size not multiple of chunk size");
        };
        BufferedReader br = new BufferedReader(new FileReader(file));
        while ((line = br.readLine()) != null){
            buffer.append(line);
        }
        br.close();
        return buffer.toString();
    }

    /**
     * Iterates through numChunks and chunkSize to calculate and return the intervals that the threads should act on
     * @param numChunks Number of chunks we want (AKA # threads)
     * @param chunkSize Chunk size. This is passed in via the CLI.
     * @return Array of all the intervals that the threads should act on
     */
    private static Interval[] getIntervals(int numChunks, int chunkSize) {
        Interval[] intervals = new Interval[numChunks];

        for (int i = 0; i < numChunks; i++) {
            int leftInclusive = chunkSize * i;
            int rightExclusive = chunkSize * (i + 1);

            Interval interval = new Interval(leftInclusive, rightExclusive);
            intervals[i] = interval;
        }
        return intervals;
    }

    /**
     * Get the "labels" that describe the pattern in which we want to print the chars
     * @param numChunks Number of chunks
     * @return Pattern to print chunks
     */
    private static List<Character> getLabels(int numChunks) {
        Scanner scanner = new Scanner(System.in);
        List<Character> labels = new ArrayList<Character>();
        int endChar = numChunks == 0 ? 'a' : 'a' + numChunks - 1;
        System.out.printf("Input %d character(s) (\'%c\' - \'%c\') for the pattern.\n", numChunks, 'a', endChar);
        for (int i = 0; i < numChunks; i++) {

            // Justin fix: error check input to ensure it stays inbounds
            char readLabel = scanner.next().charAt(0);
            while (readLabel < 'a' || readLabel > endChar) {
                System.out.println("Your character is out of bounds. Try again.");
                readLabel = scanner.next().charAt(0);
            }

            labels.add(readLabel);
        }
        scanner.close();
        // System.out.println(labels);
        return labels;
    }

    /**
     * Runs the swapper and returns an output buffer
     * @param content The content to print out
     * @param chunkSize Chunk size
     * @param numChunks Number of chunks
     * @return An output buffer to write later. This is the char[].
     */
    private static char[] runSwapper(String content, int chunkSize, int numChunks) throws Exception {
        List<Character> labels = getLabels(numChunks);
        Interval[] intervals = getIntervals(numChunks, chunkSize);
        // TODO: Order the intervals properly, then run the Swapper instances. --> Done

        Interval[] newIntervals = new Interval[numChunks];

        for (int i = 0; i < labels.size(); i++)
        {
            // 'a' should be interval[0], so work based off of that. Calculate the position
            // Schedule that in newIntervals
            int index = labels.get(i) - 'a';

            newIntervals[i] = intervals[index];
        }

        // Spawn threads and run the swapper instance
        char[] outputBuffer = new char[numChunks * chunkSize];
        Thread[] spawnedThreads = new Thread[numChunks];

        // Create & Start thread
        for (int i = 0; i < numChunks; i++)
        {
            Swapper swapInstance = new Swapper(newIntervals[i], content, outputBuffer, i * chunkSize);
            spawnedThreads[i] = new Thread(swapInstance);
            spawnedThreads[i].start();
        }

        // Join all the threads (read: wait for them to finish)
        for (int i = 0; i < numChunks; i++)
        {
            spawnedThreads[i].join();
        }

        return outputBuffer;
    }

    private static void writeToFile(String contents, int chunkSize, int numChunks) throws Exception {
        char[] buff = runSwapper(contents, chunkSize, contents.length() / chunkSize);
        PrintWriter writer = new PrintWriter("output.txt", "UTF-8");
        writer.print(buff);
        writer.close();
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java TextSwap <chunk size> <filename>");
            return;
        }
        String contents = "";
        int chunkSize = Integer.parseInt(args[0]);

        // Error Check
        if (chunkSize > 26)
        {
            System.out.println("Chunk size too small");
            return;
        }
        try {
            contents = readFile(args[1],chunkSize);
            writeToFile(contents, chunkSize, contents.length() / chunkSize);
        } catch (Exception e) {
            System.out.println("Error with IO.");
            return;
        }
    }
}
