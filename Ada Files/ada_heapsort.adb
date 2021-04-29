-----------------------------------------------------------------------
-- Authors: Colton Van Orsdel, Hayden Roszell
-- File: ada_heapsort.adb
-- Purpose: Similar to a ".c" file in C, to consolidate
-- 			the primary working function code for use in our main
-----------------------------------------------------------------------
package body Ada_Heapsort is

	function GetNewNode (word : Unbounded_String; size : Integer) return Heap_Node is
		temp : Heap_Node;
	begin
	   temp.word := word; -- assign key (size) of new node to function parameter's input
	   temp.size := size; -- assign data of new node to function parameter's input
	   return temp;
	end GetNewNode;

	function CreateHeap return LittleEndUpHeap is
	   heap : LittleEndUpHeap;
	begin
	   heap.count := 1; -- initialize heap's array index to 1 (not 0!)
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
	   leftIndex : Integer := parent * 2; -- left child of parent
	   rightIndex : Integer := parent * 2 + 1; -- right child of parent
	   smallest : Integer; -- smallest value of heap
	begin
		if leftIndex < 1 or leftIndex >= heap.count then -- if there is no left child
	   		leftIndex  := 0;
		end if;
		if rightIndex < 1 or rightIndex >= heap.count then -- if there is no right child
			rightIndex := 0;
		end if;

		if leftIndex /= 0 and heap.theHeap  (parent).size > heap.theheap (leftIndex).size then -- if left child is less than parent
			smallest := leftIndex; -- left child is minimum
		else
			smallest := parent; -- otherwise, parent is already minimum
		end if;

		if rightIndex /= 0 and heap.theHeap (smallest).size > heap.theHeap (rightIndex).size then -- if right child is less than parent
			smallest := rightIndex; -- right child is minimum
		end if;

		if parent /= smallest then -- if parent isn't already minimum
			tempNode := heap.theHeap (smallest); -- set temp node to the minimum node as determined by above indexing
			heap.theHeap (smallest) := heap.theHeap  (parent); -- set min node to parent node
			heap.theHeap (parent) := tempNode; -- set the parent to tempNode

			siftDown(heap, smallest); -- recursive call until parent is minimum
		end if;
	end SiftDown;

	function RemoveNode (heap : in out LittleEndUpHeap) return Heap_Node is
		removedNode : Heap_Node;
	begin
		if heap.count = 1 then -- simple error-checking to see if heap has any nodes remaining before removal
			Ada.Text_IO.Put_Line ("Cannot remove - heap is empty!");
			removedNode.word := Ada.Strings.Unbounded.To_Unbounded_String("");
			removedNode.size := 0;
			return removedNode;
		end if;

		removedNode := heap.theHeap (1); -- store removed node for return
		heap.theHeap (1) := heap.theHeap (heap.count - 1); -- decrement index of heap
		heap.count := heap.count - 1; -- decrement size of heap

		siftDown(heap, 1); -- recursive call until heap properties are met

		return removedNode;
	end RemoveNode;

	function ConstructHeapFromFile return LittleEndUpHeap is
		F : File_Type;
		heap : LittleEndUpHeap := CreateHeap;
	begin
		Open (F, In_File, File_Name); -- open the input text file for reading
		while not End_Of_File (F) loop -- while file hasn't been fully read
            declare
                word : String := Get_Line (F); -- read in words from input text, one line at a time
				word_t : Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String(word); -- convert read word to an Ada unbounded string
                newNode : Heap_Node := GetNewNode (word_t, length(word_t)); -- generate new node based on read in words and their size 
            begin
                InsertNode (heap, newNode); -- insert the new node into the heap
            end;
        end loop;
		Close (F); -- close the input text file
		return heap; -- return the heap, now filled with data from input text
	end constructHeapFromFile;

	procedure DeconstructHeap (heap : in out LittleEndUpHeap) is
		temp : Heap_Node;
	begin
	   while heap.count > 1 loop -- as long as heap is not empty
			temp := RemoveNode(heap); -- perform removal from heap, storing removed nodes in temp
			Put_Line (Integer'Image(temp.size) & " : " & temp.word); -- print sorted data in format of "size : word"
	   end loop;
	end deconstructHeap;

end Ada_Heapsort;