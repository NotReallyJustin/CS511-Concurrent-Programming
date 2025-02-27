import java.util.concurrent.Semaphore;

final int patriots = 20;
final int jets = 10;

Semaphore jetsTickets = new Semaphore(0);

Thread.start {
    sleep(1000);            // After a certain period of time..
    itGotLate = true;       // It gets late
    ticket.release(2);      // Release the tickets so the folks currently stuck in the Semaphore gets released/gets the tickets
}

patriots.times {
    Thread.start {
        jetsTickets.release();
        print("Patriots fan goes in\n");
    }   
}

jets.times {
    Thread.start {
        
        if (!itGotLate) // Only enforce ticket policy if it's not late
        {
            jetsTickets.acquire(2);
            print("Jets fan goes in\n");
        }
    }
}