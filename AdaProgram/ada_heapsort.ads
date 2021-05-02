-----------------------------------------------------------------------
-- Authors: Colton Van Orsdel, Hayden Roszell
-- File: ada_heapsort.adb
-- Purpose: Similar to a function's ".h" file in C, to consolidate
-- 			the background code (variables, function prototypes,
--			stucture declarations, etc.) for use in main
-----------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
package Ada_Heapsort is
	MAXSIZE : constant Integer := 10000;
	FILE_PATH : constant String := "./clean_text.txt";

	type Heap_Node is record
        word : Unbounded_String;
        size : Integer;
    end record;

	type Heap_Type is array (0 .. MAXSIZE) of Heap_Node;

    type LittleEndUpHeap is record
        theHeap : Heap_Type;
        count : Integer;
    end record;

	function GetNewNode (word : Unbounded_String; size : Integer) return Heap_Node;
    function CreateHeap return LittleEndUpHeap;
    procedure InsertNode (heap : in out LittleEndUpHeap; newNode : in out Heap_Node);
    procedure BubbleUp (heap : in out LittleEndUpHeap; theIndex : Integer);
    procedure SiftDown (heap : in out LittleEndUpHeap; parent : Integer);
    function RemoveNode (heap : in out LittleEndUpHeap) return Heap_Node;

    function ConstructHeapFromFile return LittleEndUpHeap;
    procedure DeconstructHeap (heap : in out LittleEndUpHeap);

end Ada_Heapsort;