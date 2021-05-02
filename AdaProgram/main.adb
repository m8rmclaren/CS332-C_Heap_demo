-----------------------------------------------------------------------
-- Authors: Colton Van Orsdel, Hayden Roszell, Arseniy Gorbanev
-- File:    main.adb
-- Purpose: Equivalent to a "main.c" in C; this is where we will call 
--          everything to perform the heapsort operation and time test
-- Compile using: gnatmake -gnat2012 main.adb
-----------------------------------------------------------------------
with Ada_Heapsort; use Ada_Heapsort;
with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
    heap : LittleEndUpHeap;
    
    -- timing variables
    startTime, endTime : Time;
    milliS : Duration;
    
begin
    -- timing start
    startTime := Clock;
    
    heap := Ada_Heapsort.ConstructHeapFromFile; -- retrieve heap, filled with data from input text
    Ada_Heapsort.DeconstructHeap(heap); -- print out sorted heap data
    
    -- timing calculation
    endTime := Clock;
    milliS := (endTime - startTime) * 1000000;
    Put_Line ("Runtime =" & Duration'Image(milliS) & " microseconds.");
    milliS := (endTime - startTime);
    Put_Line ("Runtime =" & Duration'Image(milliS) & " seconds.");
end Main;