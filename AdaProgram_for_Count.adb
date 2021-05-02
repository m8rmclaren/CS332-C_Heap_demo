with Ada_Heapsort; use Ada_Heapsort;
with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
    heap : LittleEndUpHeap;
    startTime, endTime : Time;
    milliS : Duration;
begin
    startTime := Clock;
    heap := Ada_Heapsort.ConstructHeapFromFile;
    Ada_Heapsort.DeconstructHeap(heap);
    endTime := Clock;
    milliS := (endTime - startTime) * 1000000;
    Put_Line ("Runtime =" & Duration'Image(milliS) & " microseconds.");
    milliS := (endTime - startTime);
    Put_Line ("Runtime =" & Duration'Image(milliS) & " seconds.");
end Main;
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
            heap.theHeap (heap.count) := newNode;
            bubbleUp(heap, heap.count);
            heap.count := heap.count + 1;
        end if;
    end InsertNode;
    procedure BubbleUp (heap : in out LittleEndUpHeap; theIndex : Integer) is
        tempNode : Heap_Node;
        parent : Integer := theIndex / 2;
    begin
        if heap.theHeap (parent).size > heap.theHeap (theIndex).size then
            tempNode := heap.theHeap (parent);
            heap.theHeap (parent) := heap.theHeap (theIndex);
            heap.theHeap (theIndex) := tempNode;
            bubbleUp(heap, parent);
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
            Put_Line (Integer'Image(temp.size) & " " & temp.word);
       end loop;
    end deconstructHeap;
end Ada_Heapsort;
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