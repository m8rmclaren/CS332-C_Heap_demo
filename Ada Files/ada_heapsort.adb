-----------------------------------------------------------------
-- Author: Colton Van Orsdel
-- File: ada_heapsort.adb
-- Purpose: Similar to a function's ".c" file in C, to consolidate
-- 			the primary working function code for use in our main
-----------------------------------------------------------------
with Ada.Text_IO, Ada.Integer_Text_IO; -- not yet sure what these include, got them from Dr. Van Hilst's videos

package body Ada_Heapsort is

	function GetNewNode (word : String; size : Integer) return Heap_Node is
		temp : Heap_Node;
	begin
	   temp.word := word;
	   temp.size := size;
	   return temp;
	end GetNewNode;

	function CreateHeap return LittleEndUpHeap is
	   heap : LittleEndUpHeap;
	begin
	   heap.count := 1;
	   return heap;
	end CreateHeap;

	procedure InsertNode (heap : in out LittleEndUpHeap; newNode : in out Heap_Node) is
	begin
	   if heap.count < MAXSIZE then
			heap.theHeap (heap.count) := newNode; -- insert new node into heap array
			bubbleUp(heap, heap.count); -- sort until heap properties are met
			heap.count := heap.count + 1; -- increment size of heap for indexing
		end if;
	end InsertNode;

	procedure BubbleUp (heap : in out LittleEndUpHeap; theIndex : Integer) is
		tempNode : Heap_Node;
		parent : Integer := theIndex / 2;
	begin
		if heap.theHeap (parent).size > heap.theHeap (theIndex).size then
			tempNode := heap.theHeap (parent); -- set temp to current parent
			heap.theHeap (parent) := heap.theHeap (theIndex); --  new parent is now index from call
			heap.theHeap (theIndex) := tempNode; -- index from call is now old parent

			bubbleUp(heap, parent); -- recursive call until parent is minimum
		end if;
	end BubbleUp;

	procedure SiftDown (heap : in out LittleEndUpHeap; parent : Integer) is
	   tempNode : Heap_Node;
	   leftIndex : Integer := parent * 2;
	   rightIndex : Integer := parent * 2 + 1;
	   smallest : Integer;
	begin
		if leftIndex < 1 or leftIndex >= heap.count then
	   		leftIndex  := 0;
		end if;
		if rightIndex < 1 or rightIndex >= heap.count then
			rightIndex := 0;
		end if;

		if leftIndex /= 0 and heap.theHeap  (parent).size > heap.theheap (leftIndex).size then
			smallest := leftIndex;
		else
			smallest := parent;
		end if;

		if rightIndex /= 0 and heap.theHeap (smallest).size > heap.theHeap (rightIndex).size then
			smallest := rightIndex;
		end if;

		if parent /= smallest then
			tempNode := heap.theHeap (smallest);
			heap.theHeap (smallest) := heap.theHeap  (parent);
			heap.theHeap (parent) := tempNode;

			siftDown(heap, smallest);
		end if;
	end SiftDown;

	function RemoveNode (heap : in out LittleEndUpHeap) return Heap_Node is
		removedNode : Heap_Node;
	begin
		if heap.count = 1 then
			Ada.Text_IO.Put_Line ("Cannot remove - heap is empty!");
			return removedNode;
		end if;

		removedNode := heap.theHeap (1);
		heap.theHeap (1) := heap.theHeap (heap.count - 1);
		heap.count := heap.count - 1;

		siftDown(heap, 1);

		return removedNode;
	end RemoveNode;

end Ada_Heapsort;