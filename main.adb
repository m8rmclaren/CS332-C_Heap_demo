-----------------------------------------------------------------
-- Author: Colton Van Orsdel
-- File: main.adb
-- Purpose: Equivalent to a "main.c" in C; this is where we will
-- 			call everything
-- References: Note that, for readability sake, these are the
--			   references I used for ALL of the .adb/.ads files,
-- 			   not just the main. Please add to this list!
-- 
-- 			   https://erau.instructure.com/courses/123340/pages/array-list-1?module_item_id=7432360
--			   -- This is the first of six videos on Ada from Dr. Van Hilst; they're all useful and are my primary resource so far
--			   https://rosettacode.org/wiki/Sorting_algorithms/Heapsort#Ada
--			   -- This is an example of a heapsort algorithm in Ada. Not as useful as you might think, at least not yet.
-- 			   http://ada-auth.org/arm.html
--		       -- This is the Ada2012 reference manual; helps to understand functions from other examples.
-- 			   https://rosettacode.org/wiki/Pointers_and_references#Pointers
--		       -- Very relevant, since we do a lot with pointers in a heapsort algorithm.
-----------------------------------------------------------------
with Ada.Integer_Text_IO; -- again, not yet sure what this includes, but better safe than sorry!
with Ada_Heapsort; -- includes our .h and .adb (I think it's both?) for use in the Main procedure

procedure Main is
-- we do some things with our function procedures and packages here...

begin
-- this is where we actually start to call functions and make things happen

end Main;
