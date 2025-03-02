// Class for a sequencer. Think DE hardware component
// What this does is it runs A, B, C
// But only one thread can latch on to A, B, and C

class ThreeWaySequencer {
    private int state;

    ThreeWaySequencer()
    {
        this.state = 1;
    }

    synchronized void first()       // Synchronoized so only 1 thread can run first/sec/third at a time
    {
        // If state is not 1, we gotta wait before we run
        while (state != 1)
        {
            wait();
        }

        print("First sequencer is running.\n");
        state = 2;      
        notifyAll();               // Have all other monitors check to see if they can run
    } 

    synchronized void second()   
    {
        while (state != 2)
        {
            wait();
        }

        print("Second sequencer is running.\n");
        state = 3;
        notifyAll();         
    } 

    synchronized void third()    
    {
        while (state != 3)
        {
            wait();
        }

        print("Third sequencer is running.\n");
        state = 1;
        notifyAll();
    } 
}

