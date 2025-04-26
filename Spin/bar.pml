// We need to model acquire() and release() because Spin doesn't actually have acquire() release()
// Promela doesn't use functions; it uses macros

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

int ticket = 0;
int mutex = 1;

// Problem invariant vars
int p = 0;
int j = 0;

// Spawn 10 threads
active[6] proctype Patriots()
{
    p++;                        // Put this up here to avoid a weird interleaving that violates assertion
    release(ticket);             // 2 patriots go in = 1 jets can go in
    assert(2 * j <= p)          // Problem invariant assertion check
}

// Spawn 5 threads
active[3] proctype Jets()
{
    acquire(mutex);
    acquire(ticket);            // Wait for the first perm
    // Wait for the second perm. If it's hard to think about, imagine Patriots fans giving someone the ok when they go in
    acquire(ticket);           
    release(mutex);
    j++;        
    assert(2*j <= p)        // Problem invariant assertion check
}