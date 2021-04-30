-----------------------------------------------------------------------
-- Authors:	Colton Van Orsdel, Hayden Roszell
-- File:	main.adb
-- Purpose:	Equivalent to a "main.c" in C; this is where we will call 
--			everything to perform the heapsort operation and time test
-- References: 	Note that, for readability sake, these are the
--				references I used for ALL of the .adb/.ads files,
--				not just the main. Please add to this list!
-- 
--				https://erau.instructure.com/courses/123340/pages/array-list-1?module_item_id=7432360
--				* This is the first of six videos on Ada from Dr. Van 
--					Hilst; they're all useful and are my primary resource so far
--				https://rosettacode.org/wiki/Sorting_algorithms/Heapsort#Ada
--				* This is an example of a heapsort algorithm in Ada.
--				http://ada-auth.org/arm.html
--				* This is the Ada2012 reference manual.
--				https://rosettacode.org/wiki/Pointers_and_references#Pointers
--				* Very relevant, since we do a lot with pointers in a 
--					heapsort algorithm.
--				https://www.adacore.com/uploads/books/pdf/Ada_For_The_Embedded_C_Developer_2021-02.pdf
--				* AdaCore book comparing C and Ada, with many great examples
-----------------------------------------------------------------------
with Ada_Heapsort; use Ada_Heapsort;

procedure Main is
    heap : LittleEndUpHeap;
begin
    heap := Ada_Heapsort.ConstructHeapFromFile; -- retrieve heap, filled with data from input text
    Ada_Heapsort.DeconstructHeap(heap); -- print out sorted heap data
end Main;