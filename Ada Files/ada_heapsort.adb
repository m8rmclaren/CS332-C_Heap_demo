-----------------------------------------------------------------
-- Author: Colton Van Orsdel
-- File: ada_heapsort.adb
-- Purpose: Similar to a function's ".c" file in C, to consolidate
-- 			the primary working function code for use in our main
-----------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

package body Ada_Heapsort is

	function GetNewNode (word : Unbounded_String; size : Integer) return Heap_Node is
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
			removedNode.word := Ada.Strings.Unbounded.To_Unbounded_String("");
			removedNode.size := 0;
			return removedNode;
		end if;

		removedNode := heap.theHeap (1);
		heap.theHeap (1) := heap.theHeap (heap.count - 1);
		heap.count := heap.count - 1;

		siftDown(heap, 1);

		return removedNode;
	end RemoveNode;

	function ConstructHeapFromFile return LittleEndUpHeap is
		F : File_Type;
		heap : LittleEndUpHeap := CreateHeap;
	begin
		Open (F, In_File, File_Name);
		while not End_Of_File (F) loop
            declare
                word : String := Get_Line (F);
				word_t : Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String(word);
                newNode : Heap_Node := GetNewNode (word_t, length(word_t));
            begin
                InsertNode (heap, newNode);
            end;
        end loop;
		Close (F);
		return heap;
	end constructHeapFromFile;

	procedure DeconstructHeap (heap : in out LittleEndUpHeap) is
		temp : Heap_Node;
	begin
	   while heap.count > 1 loop
			temp := RemoveNode(heap);
			Put_Line (temp.word);
	   end loop;
	end deconstructHeap;

end Ada_Heapsort;