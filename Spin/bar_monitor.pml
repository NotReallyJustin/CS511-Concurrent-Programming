int lock = 1;       // Simulating monitors - it's technically just re-entrant locks
int wait = 0;       // Semaphore to simulate wait. Kind of just put them to sleep until this mutex becomes 1
int delta = 0;      // Diff between jets and patriots

int j = 0;
int p = 0;

inline acquire(s)
{
    atomic {
        s > 0;      // acquire sleeps/waits until you have at least 1 perm.
        s--        // Then, remove that perm. This is how we simulate how Semaphores work
    }
}

inline release(s)
{
    s++          // Give perms. We're fine because s++ is atomic in Promela. Different in Java tho
}

inline enter_patriots(lock)
{
    acquire(lock);              // Re-entrant lock
    delta++;
    release(wait);                  // notify();
    p++;                            // Instrumentation
    release(lock)
}

inline enter_jets(lock)
{
    acquire(lock);              // Re-entrant lock
    do
        :: delta < 2 ->
            release(lock);
            acquire(wait);          // wait();
            // acquire another lock because 2 jets for 1 patriot - remember we're modeling and not actually coding w/ monitors
            // Remember the whole release the lock and lock again after you wait in Java? yea it's this.
            acquire(lock)
        :: else -> break
    od

    delta = delta - 2;          // Subtract delta
    j++;
    release(lock)
}

active[6] proctype Patriots()
{
    enter_patriots(lock);
    assert(2 * j <= p)
}

active[3] proctype Jets()
{
    enter_jets(lock);
    assert(2 * j <= p)          // Problem invariant
}