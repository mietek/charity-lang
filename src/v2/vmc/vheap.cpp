//implement class VHeap using two-space copying garbage collector
#include <cassert>
#include "vmc.h"

//0..15 are pre-created 
#define NEWINT_NUM 16

static VInt* memBase    =0;
static VInt* pBottom    =0;
static VInt* pFree      =0;

static VInt* newMemBase =0;
static int newHeapSize;
static int gcCnt=0;
static int totalAllocated = 0;

static VPtr doAlloc(VInt hdr, int size);

int VHeap::init(int aInitSize)
{
    uninit();
	newHeapSize = aInitSize;

    memBase=new VInt[aInitSize];
    if(memBase==0)
        return -1; //out of memory

    pBottom=memBase + aInitSize;
	//the null tuple, the null tuple occupy two unit
	// so that the forward address in gc can be stored
	memBase[0] = HD_TUPLE(0,0);
	memBase[1] = 0;
    pFree=memBase+2;
    //a piece of memory researved for commonly used boxed int
    for(int i=0;i<NEWINT_NUM;i++){ 
        pFree[0]=HD_TUPLE(1,0);
        pFree[1]=i;
        pFree+=2;
    }

    gcCnt=0;
	totalAllocated = 0;
    return 0;
}
void VHeap::uninit()
{
    delete[] memBase;
    memBase=0;
	delete[] newMemBase;
	newMemBase = 0;
} 

static VPtr doAlloc(VInt hdr, int size)
{
    while(pFree+size+1>=pBottom){ //garbage collection till we have space!
        VHeap::garbageCollection();
    }
    *pFree=hdr;
    pFree+=size+1;
	totalAllocated+=size+1;
    return pFree-size;
}
VPtr VHeap::nullTuple()
{	
	return memBase+1;
}
VPtr VHeap::newTuple(int bCnt, int pCnt)
{
    assert(bCnt<255 && pCnt<128);
	if(bCnt==0 && pCnt==0)
		return nullTuple();
    return doAlloc(HD_TUPLE(bCnt, pCnt), bCnt+pCnt);
}
VPtr VHeap::newBArray(int bytesize)
{
    assert(bytesize>0 && bytesize<0x00400000);
    return doAlloc(HD_BARRAY(bytesize), (bytesize+3)/4);
}
VPtr VHeap::newPArray(int size)
{
    assert(size>0 && size<0x00100000);
    return doAlloc(HD_PARRAY(size), size);
}

VPtr VHeap::newInt(VInt aInt)
{
    if(aInt>=0 && aInt<NEWINT_NUM)
        return memBase+3+aInt*2;
    VPtr p=doAlloc(HD_TUPLE(1,0), 1);
    *p=aInt;
    return p;
}
VPtr VHeap::newStr(const char* aStr)
{
    VPtr p=newBArray(strlen(aStr)+1);
    strcpy((char*)p, aStr);
    return p;
}
int  VHeap::equivalent(VPtr p1, VPtr p2)
{
	if(p1==p2) return 1;
	if(p1==0 || p2==0) return 0;

	if(VPTR_HEADER(p1)!=VPTR_HEADER(p2))
		return 0;
	int bcnt = bFieldCnt(p1);
	int i;
	for(i=0;i<bcnt;i++)
		if(p1[i]!=p2[i])
			return 0;
	int cnt = pFieldCnt(p1)+bcnt;
	for(;i<cnt;i++)
		if(!equivalent(VPtr(p1[i]), VPtr(p2[i])))
			return 0;
	return 1;
}

VInt VHeap::asInt(VPtr p) { return p - memBase; }
VPtr VHeap::asPtr(VInt i) { return memBase+i; }
int  VHeap::spaceUsed()   { return pFree-memBase; }
int  VHeap::spaceAvailable() { return pBottom-pFree; }
int  VHeap::gcOccurredCnt()  { return gcCnt; }
int  VHeap::getTotalAllocated() { return totalAllocated; }
//return the new address of the node
static VPtr copyNode(VPtr p)
{
//    if(p==0)
//        return 0;
//    int tag=VInt(p)&1;  //keep the tag so that if p is tagged, then the new p is also tagged
//    p=VPtr(VInt(p)&0xfffffffe);

    p--;
    if(p[0]==0) //already copied, the next unit is the forward address
        //a bug found: if the node only has the header, such as the null
		//tuple, there is no space to store forward addresses
		//therefore, even a null tuple should occupy two words.
		return VPtr(p[1]);
    
    VPtr src=p;
    VPtr dst=pFree;
    for(int k=HD_NODESIZE(p[0]); k>0; k--)
        *(dst++)=*(src++);
    //set forward address
    p[0]=0;
    p[1]=VInt(pFree+1);
    pFree=dst;
    return VPtr(p[1]);
}
void VHeap::garbageCollection()
{
	//create a new memory block if not already exist
	if(newMemBase==0){
		newMemBase = new VInt[newHeapSize];
		if(newMemBase==0){
			cerr<<"\n Out of memory!\n";
			exit(-1);
		}
	}
	//switch current memBase
	int oldHeapSize = pBottom - memBase;
    VInt* oldMemBase= memBase;
	memBase = newMemBase;
    pFree=memBase;
	pBottom=memBase+newHeapSize;

    int i;
    //copy the null tuple
	pFree[0] = HD_TUPLE(0,0);
	oldMemBase[0] = 0;
	oldMemBase[1] = VInt(pFree+1); //set forward address
	pFree+=2;
	//copy the boxed integers
	for(i=0;i<NEWINT_NUM;i++){
        pFree[0]=HD_TUPLE(1,0);
        pFree[1]=i;
        oldMemBase[2+i*2]=0; //set flag 
        oldMemBase[2+i*2+1]=VInt(&pFree[1]);
        pFree+=2;
    }
//[-<2003.2.18, a small bug found here: forgot to set the
//forward address after copying the pre-created integers.
//    memcpy(memBase, oldMemBase, sizeof(VInt)*2*NEWINT_NUM+sizeof(VInt));
//    pFree+=2*NEWINT_NUM+1;
 
    VInt* pnext=pFree;

    for(i=0;i<VMC_MAXTHREADS;i++){
        if(VMachine::theThreads[i]==0)
            continue;
        VThread* pt=VMachine::theThreads[i];
        for(int j=pt->getPSize()-1; j>=0; j--){
            //copy node pointed by pt->getPItem(j)
            VPtr& t=pt->getPItem(j);
            t=copyNode(t);
        }
    }
    //now all node on stack are copied,copy internal node
    while(pnext<pFree){
        VInt hdr=*(pnext++);
        int pcnt=HD_PFIELDCNT(hdr);
        if(pcnt==0xff)
            pcnt=HD_FIELDCNT(hdr);
        pnext+=HD_BFIELDCNT(hdr);
        for(int i=0;i<pcnt;i++){
            *pnext= VInt(copyNode(VPtr(*pnext)));
            pnext++;
        }
    }

	//If the heap is nearly full, double its size in the future
	if(pFree-memBase > newHeapSize*3/4) { 
		newHeapSize *=2;
	}

	if(oldHeapSize == newHeapSize){ //equal sized, reuse it to avoid constant delete/new
		newMemBase=oldMemBase;
	}
	else{ //don't create the 
		delete[] oldMemBase;
		newMemBase=0;
	}

    gcCnt++;
}
