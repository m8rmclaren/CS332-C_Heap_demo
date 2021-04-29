-----------------------------------------------------------------
-- Author: Colton Van Orsdel
-- File: ada_heapsort.adb
-- Purpose: Similar to a function's ".h" file in C, to consolidate
-- 			the background code (variables, function prototypes,
--			stucture declarations, etc.) for use in main
-----------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Ada_Heapsort is
	MAXSIZE : constant Integer := 100;
	File_Name : constant String := "/home/hroszell/school/project/text.txt";

	type Heap_Node is record
		word : String (1 .. 20);
		size : Integer;
	end record;

	type Heap is array (1 .. MAXSIZE) of Heap_Node;

    type LittleEndUpHeap is record
        theHeap : Heap;
        count : Integer;
    end record;

    function constructHeapFromFile return LittleEndUpHeap;
    -- procedure deconstructHeap (heap : LittleEndUpHeap);

private

	function GetNewNode (word : String; size : Integer) return Heap_Node;
    function CreateHeap return LittleEndUpHeap;
    procedure InsertNode (heap : in out LittleEndUpHeap; newNode : in out Heap_Node);
    procedure BubbleUp (heap : in out LittleEndUpHeap; theIndex : Integer);
    procedure SiftDown (heap : in out LittleEndUpHeap; parent : Integer);
    function RemoveNode (heap : in out LittleEndUpHeap) return Heap_Node;

end Ada_Heapsort;
