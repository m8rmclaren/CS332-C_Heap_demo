-----------------------------------------------------------------
-- Author: Colton Van Orsdel
-- File: ada_heapsort.adb
-- Purpose: Similar to a function's ".h" file in C, to consolidate
-- 			the background code (variables, function prototypes,
--			stucture declarations, etc.) for use in main
-----------------------------------------------------------------
package Ada_Heapsort is
	type Heap_Node is record
        word : String (1 .. 20);
        size : Integer;
    end record;

    type LittleEndUpHeap is record
        theHeap : Heap_Node;
        count : Integer;
    end record;

    function constructHeapFromFile return LittleEndUpHeap;
    procedure deconstructHeap (heap : LittleEndUpHeap);

private

	function getNewNode (word : String; size : Integer) return Heap_Node;
    function createHeap return LittleEndUpHeap;
    procedure insertNode (heap : LittleEndUpHeap; newNode : Heap_Node);
    procedure bubbleUp (heap : LittleEndUpHeap; theIndex : Integer);
    procedure siftDown (heap : LittleEndUpHeap; parent : Integer);
    function removeNode (heap : LittleEndUpHeap) return Heap_Node;

end Ada_Heapsort;