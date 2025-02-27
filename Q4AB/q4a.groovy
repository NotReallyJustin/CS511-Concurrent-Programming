import java.util.concurrent.Semaphore

Semaphore mutexS = new Semaphore(1)
Semaphore mutexF = new Semaphore(1)
Semaphore studentPermit = new Semaphore(1)
Semaphore facultyPermit = new Semaphore(1)

final int S = 10 // Number of students
final int F = 10 // Number of faculty
final int CM = 3 // Number of cleaning machines

int s = 0 // Counter for students
int f = 0 // Counter for faculty

S.times {
    Thread.start { // Student

        // Student locks
        mutexS.acquire();
        if (s == 0)
        {
            studentPermit.acquire();
        }
        s++;
        mutexS.release();

        print("Student in lounge\n");

        // Student unlocks
        mutexS.acquire();
        s--;

        if (s == 0)
        {
            studentPermit.release();
        }
        mutexS.release();
    }
}

F.times {
    Thread.start { // Faculty
        // Faculty locks
        mutexF.acquire();
        if (f == 0)
        {
            facultyPermit.acquire();
        }
        f++;
        mutexF.release();

        print("Faculty in lounge\n");

        // Faculty unlocks
        mutexF.acquire();
        f--;

        if (f == 0)
        {
            facultyPermit.release();
        }
        mutexF.release();
    }
}

CM.times {
    Thread.start { // Clean
        while (true) {
            // No more students in lounge. Only one robot thread can run at once
            // bc only one of the bots can acquire at once
            studentPermit.acquire();
            facultyPermit.acquire();

            print("Robot cleaning...");
            studentPermit.release();
            facultyPermit.release();
        }
    }
}
