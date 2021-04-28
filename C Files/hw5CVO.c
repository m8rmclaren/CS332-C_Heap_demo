/*********************************************************************
*   Colton Van Orsdel
*   CS315
*   Program hw5
*
*   Program to perform insertion, in-order traversal, and pre-order 
	traversal on a Huffman tree inside of a "little end up" heap inside
	an almost perfect binary tree in array (used to build Huffman codes)
**********************************************************************/
// theData.txt: "g 56 b 15 c 25 k 5 a 17 d 18 $"

#include <stdio.h>
#include <stdlib.h>

#define MAXSIZE 20 // generic size definition > test case requirement

typedef struct huffmanTreeNode
{
	char theKey; // character of HTNs
	int theFrequency; // frequency of HTNs (for sorting)
	struct huffmanTreeNode *ptrToLeftChild, *ptrToRightChild; // left, and right branches of HTNs
}	HTN;

typedef struct littleEndUpHeap{
	HTN *theHeap; // heap array to store HTN data
	int count; // current size of HTN 
}	HEAP;

HTN *createNode(char key, int freq); // creates a new HTN to be inserted into heap
HEAP *createHeap(); // creates heap and sets first index to 1 (not 0!)
void insertNode(HEAP *h, HTN *newNode); // inserts HTN into heap array and calls bubbleUp to correct
void bubbleUp(HEAP *h, int theIndex); // sorts up through the heap (insertion)
void siftDown(HEAP *h, int parent); // sorts down through the heap (removal)
HTN *removeNode(HEAP *h); // removes node from heap and calls siftDown to correct
HTN *createHuffmanTree(HEAP *h); // sorts heap nodes into Huffman tree with new interior nodes
void inOrderTraverse(HTN *root); // recursive in-order traversal
void preOrderTraverse(HTN *root); // recursive pre-order traversal

int main()
{
	char userChar;
	int userFreq; 
	HEAP *heap = createHeap(); // initialize heap for node insertion

	while (userChar != '$') // loop until users designates termination
	{
		printf("\n\nEnter a character (enter a '$' to quit entering characters: ");
		scanf(" %c", &userChar); // catch "key" for HTN
		if (userChar != '$') // check again if loop needs to be exited
		{
			printf("\n Enter '%c's frequency: ", userChar);
			scanf("%d", &userFreq); // catch "frequency" for HTN

			insertNode(heap, createNode(userChar, userFreq));
		}
		else if (userChar == '$') // terminate user entries to start printing traversals
			break;
		else
			printf("\nInvalid entry - try again.\n"); // simple error-handling
	}

	HTN *root = createHuffmanTree(heap); // create and sort tree until final Huffman is ready for traversals

	printf("\n\nThe nodes of the Huffman tree in In-order are: \n\n");
	inOrderTraverse(root);

	printf("\n and in Pre-order are: \n\n");
	preOrderTraverse(root);

	return 0;
}
/***********************************************************************************/
HTN *createNode(char key, int freq)
{
	HTN *temp = (HTN*)malloc(sizeof(HTN)); 

	temp->theKey = key; // Assign key of new node to user's input
	temp->theFrequency = freq; // Assign data of new node to user's input

	return temp;
}
/***********************************************************************************/
HEAP *createHeap()
{
	HEAP *h = (HEAP*)malloc(sizeof(HEAP));

	h->count = 1; // initialize heap's array index to 1 (not 0!)
	h->theHeap = (HTN*)malloc(MAXSIZE*sizeof(int));

	return h;
}
/***********************************************************************************/
void insertNode(HEAP *h, HTN *newNode)
{
	if (h->count < MAXSIZE)
	{
		h->theHeap[h->count] = *newNode; // insert new node into heap array
		bubbleUp(h, h->count); // sort until heap properties are met
		h->count++; // increment size of heap for indexing
	}
}
/***********************************************************************************/
void bubbleUp(HEAP *h, int theIndex)
{
	HTN tempNode;
	int parent = (theIndex) / 2; // as per algorithm on slide 9

	if (h->theHeap[parent].theFrequency > h->theHeap[theIndex].theFrequency)
	{
		tempNode = h->theHeap[parent]; // set temp to current parent
		h->theHeap[parent] = h->theHeap[theIndex]; // new parent is now index from call
		h->theHeap[theIndex] = tempNode; // index from call is now old parent

		bubbleUp(h, parent); // recursive call until parent is minimum
	}
}
/***********************************************************************************/
void siftDown(HEAP *h, int parent)
{
	HTN tempNode;
	int leftIndex = parent * 2; // left child of parent
	int rightIndex = parent * 2 + 1; // right child of parent
	int smallest; // smallest value of heap

	if ((leftIndex < 1) || (leftIndex >= h->count)) // if there is no left child
		leftIndex = 0;
	if ((rightIndex < 1) || (rightIndex >= h->count)) // if there is no right child
		rightIndex = 0;

	if ((leftIndex != 0) && (h->theHeap[parent].theFrequency > h->theHeap[leftIndex].theFrequency)) // if left child is less than parent
		smallest = leftIndex; // left child is minimum
	else
		smallest = parent; // otherwise, parent is already minimum

	if ((rightIndex != 0) && (h->theHeap[smallest].theFrequency > h->theHeap[rightIndex].theFrequency)) // if right child is less than parent
		smallest = rightIndex; // right child is minimum

	if (parent != smallest) // if parent isn't already minimum
	{
		tempNode = h->theHeap[smallest]; // set temp node to the minimum node as determined by above indexing
		h->theHeap[smallest] = h->theHeap[parent]; // set min node to parent node
		h->theHeap[parent] = tempNode; // set the parent to tempNode

		siftDown(h, smallest); // recursive call until parent is minimum
	}
}
/***********************************************************************************/
HTN *removeNode(HEAP *h)
{
	HTN *removedNode = (HTN*)malloc(sizeof(HTN));

	if (h->count == 1) // simple error-checking to see if heap has any nodes remaining before removal
	{
		printf("\nCannot remove - heap is empty\n");
		return removedNode;
	}
	*removedNode = h->theHeap[1]; // store removed node for return
	h->theHeap[1] = h->theHeap[h->count-1]; // decrement index of heap
	h->count--; // decrement size of heap

	siftDown(h, 1); // recursive call until heap properties are met

	return removedNode;
}
/***********************************************************************************/
HTN *createHuffmanTree(HEAP *h)
{
	HTN *root, *ptrToLeftChild, *ptrToRightChild;

	while (h->count != 2) // while the heap still contains trees
	{
		ptrToLeftChild = removeNode(h); // remove left child first
		ptrToRightChild = removeNode(h); // remove right child second

		root = createNode('-', ptrToLeftChild->theFrequency + ptrToRightChild->theFrequency); // create interior nodes w/ sum of child's frequencies

		root->ptrToLeftChild = ptrToLeftChild; // assign new left child
		root->ptrToRightChild = ptrToRightChild; // assign new right child

		insertNode(h, root); // insert pointer to new tree back into heap
	}

	return removeNode(h); // return root of final tree
}
/***********************************************************************************/
void inOrderTraverse(HTN *root)
{
	if (root != NULL) // if Huffman tree hasn't been fully traversed
	{
		inOrderTraverse(root->ptrToLeftChild);
		printf(" %c		%d\n", root->theKey, root->theFrequency);
		inOrderTraverse(root->ptrToRightChild);
	}
}
/***********************************************************************************/
void preOrderTraverse(HTN *root)
{
	if (root != NULL) // if Huffman tree hasn't been fully traversed
	{
		printf(" %c		%d\n", root->theKey, root->theFrequency);
		preOrderTraverse(root->ptrToLeftChild);
		preOrderTraverse(root->ptrToRightChild);
	}
}