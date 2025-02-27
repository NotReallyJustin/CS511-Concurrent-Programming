import java.util.concurrent.Semaphore;

Semaphore mutexS = new Semaphore(1);
Semaphore mutexF = new Semaphore(1);
Semaphore studentPermit = new Semaphore(1);
Semaphore facultyPermit = new Semaphore(1);

final int S = 10; // Number of students
final int F = 10; // Number of faculty
final int CM = 3; // Number of cleaning machines

// Goal: When no student & no faculty, robot clean. Only one robot may clean a room.
int curr_students = 0;
int curr_staff = 0;

S.times {
    int id = it;
    Thread.start { // Student
        
        mutexS.acquire();               // Messing with shared var - enter crit section
        curr_students++;                // Readers problem
        if (curr_students == 1)
        {
            studentPermit.acquire();
        }
        mutexS.release();

        print("Student is in lounge");

        mutexS.acquire();
        curr_students--;
        if (curr_students == 0)
        {
            studentPermit.release();
        }
        mutexS.release();
    }
}

F.times {
    int id = it;
    Thread.start { // Faculty
        mutexF.acquire();               
        curr_staff++;                // Readers problem
        if (curr_staff == 1)
        {
            facultyPermit.acquire();
        }
        mutexF.release();

        print("Faculty is in lounge");

        mutexF.acquire();
        curr_staff--;
        if (curr_staff == 0)
        {
            facultyPermit.release();
        }
        mutexF.release();
    }
}

CM.times {
    int id = it;
    Thread.start { // Clean
        while (true) {
            // There is only 1 permit. These permits only get released when there's no one in the lounge.
            // Hence, only 1 robot is in the room at once when there's no student or faculty
            studentPermit.acquire();
            facultyPermit.acquire();

            print("Robot cleaning");

            studentPermit.release();
            facultyPermit.release();
        }
    }
}
