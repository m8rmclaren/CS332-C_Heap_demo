-----------------------------------------------------------------
-- Author: Colton Van Orsdel
-- File: ada_heapsort.adb
-- Purpose: Similar to a function's ".h" file in C, to consolidate
-- 			the background code (variables, function prototypes,
--			stucture declarations, etc.) for use in main
-----------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
package Ada_Heapsort is
	MAXSIZE : constant Integer := 10000;
	File_Name : constant String := "/home/hroszell/school/project/text.txt";

	type Heap_Node is record
        word : Unbounded_String;
        size : Integer;
    end record;

	type Heap is array (0 .. MAXSIZE) of Heap_Node;

    type LittleEndUpHeap is record
        theHeap : Heap;
        count : Integer;
    end record;

    function ConstructHeapFromFile return LittleEndUpHeap;
    procedure DeconstructHeap (heap : in out LittleEndUpHeap);

	function GetNewNode (word : Unbounded_String; size : Integer) return Heap_Node;
    function CreateHeap return LittleEndUpHeap;
    procedure InsertNode (heap : in out LittleEndUpHeap; newNode : in out Heap_Node);
    procedure BubbleUp (heap : in out LittleEndUpHeap; theIndex : Integer);
    procedure SiftDown (heap : in out LittleEndUpHeap; parent : Integer);
    function RemoveNode (heap : in out LittleEndUpHeap) return Heap_Node;

end Ada_Heapsort;
