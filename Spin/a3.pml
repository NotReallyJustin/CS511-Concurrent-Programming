// We want to check attempt 3 of the MEP
bool wantP = false;
bool wantQ = false;

// Thread P
active proctype P()
{
    do
        :: wantP = true;
            // Busywaiting
            do
                :: !wantQ -> break
                :: else         
            od

        // Progress labels. Just label lines of code in promela
        // When this line gets executed, we hit the progress label! This is how Spin knows there's progress
        progress1:
            wantP = false
    od
}

active proctype Q()
{
    do
        :: wantQ = true;
        !wantP;

        // Busywaiting (await)
        do
            :: !wantQ -> break
            :: else         
        od;
    
        progress2:
            wantQ = false

    od
}