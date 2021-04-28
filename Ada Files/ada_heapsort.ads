-----------------------------------------------------------------
-- Author: Colton Van Orsdel
-- File: ada_heapsort.adb
-- Purpose: Similar to a function's ".h" file in C, to consolidate
-- 			the background code (variables, function prototypes,
--			stucture declarations, etc.) for use in main
-----------------------------------------------------------------
package Ada_Heapsort is
	
	Max_Size: constant := 10; -- just an arbitrary limit for testing purposes
	type Data_Type is private; -- we just declare everything as private in the public section here, then define them all in the private section below
	type Index_Type is private;
	type Heap_Array_Type is private;

	-- Think of the below statements as our "function prototypes", as this is the equivalent of a ".h" file in C
	procedure Insert_Heap(Heap: Heap_Node_Type'; New_Node: Heap_Node_Type') -- ' denotes pointers of these types
	procedure Remove_Heap(Heap: Heap_Node_Type') -- ' denotes pointers of these types
	procedure Get_New_Node(Data_Element: Data_Type'; Data_Element_Size: Integer) -- not sure if this should be a pointer, but just mirroring Hayden's main.c for now
	-- Note that Data_Element_Size is not a good name for our integer test case; it is just the "priority" of the associated Data_Element for sorting purposes. 
	-- continued from above: In CS315, the equivalent to "size" was "freq", for the frequency of the character being entered into the Huffman tree

    function constructHeapFromFile () return LittleEndUpHeap;
    procedure deconstructHeap (heap : LittleEndUpHeap);

private
	function getNewNode (word : String; size : Integer) return Heap_Node;
    function createHeap () return LittleEndUpHeap;
    procedure insertNode (heap : LittleEndUpHeap; newNode : Heap_Node);
    procedure bubbleUp (heap : LittleEndUpHeap; theIndex : Integer);
    procedure siftDown (heap : LittleEndUpHeap; parent : Integer);
    function removeNode (heap : LittleEndUpHeap) return Heap_Node;

    type Heap_Node is record
        word : String;
        size : Integer;
    end record;

    type LittleEndUpHeap is record
        theHeap : Heap_Node;
        count : Integer;
    end record;

	type Data_Type is Integer; -- working with integers as our data type for testing purposes; will eventually be chars
	type Index_Type is Integer; -- just the index for manuevering through both the heap and the array; will likely remain as integer
	type Heap_Node_Type is record -- this is essentially the equivalent of a struct in C
		Data: Data_Type; -- data stored in heap will be of Data_Type, as defined above
		Size: Integer; -- will update size as we add/remove from heap, for checking purposes
	end record; 
	type Heap_Array_Type is array(Index_Type range 1 .. ) of Data_Type; -- fill this with data set, then iterate over it while using Get_New_Node

end Ada_Heapsort;