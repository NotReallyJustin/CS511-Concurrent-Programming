import java.util.concurrent.Semaphore;

Semaphore a = new Semaphore(2);
Semaphore b = new Semaphore(0);
Semaphore c = new Semaphore(0);
Semaphore d = new Semaphore(2);

Thread.start{
    while (true)
    {
        a.acquire();
        d.acquire(2);
        print("a");
        b.release();
        d.release(2);
    }
}

Thread.start {
    while (true)
    {
        b.acquire(2);
        print("b");
        d.acquire(2);
        c.release(2);
    }
}

Thread.start {
    while (true)
    {
        c.acquire();
        print("c");
        d.release();
        a.release();
    }
}