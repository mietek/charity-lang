//declare class VMC
#ifndef __VMC_H
#define __VMC_H
#pragma warning(disable:4786)
#include <cstdlib>
#include <iostream>
#include <string>
#include <map>
#include <vector>

using namespace std;

//primary classes defined in this header
class VThread;
class VHeap;     //heap manager
class VCodeBase; //VMC code compiler and storage manager
class VMachine;

//primary datatypes supported in VMC, change it according to different compilers
typedef unsigned char VChar;   //8 bit
typedef wchar_t VWChar; //16 bit
typedef short VShort; //16 bit
typedef int   VInt; //32 bit
typedef VInt* VPtr; //32 bit
typedef float VFloat; //32 bit
typedef double VDouble; //64 bit

#ifdef __GNUC__
typedef long long VLong;

#else
typedef __int64 VLong; //64 bit
extern ostream& operator <<(ostream&, VLong&);
extern istream& operator >>(istream&, VLong&);
#endif

struct PChar{ //Printable Char
	unsigned char ch;
	PChar(char x): ch(x){}
};
extern ostream& operator <<(ostream&, PChar);
typedef PChar * PString;
extern ostream& operator <<(ostream&, PChar*);


//maximum valid index for an operator(to indicate the offset on stack)
#define VMC_MAX_INDEX_VALUE 999
#define VMC_INVALID_ADDR  -1
#define VMC_INVALID_CODE  -1
#define VMC_DEFAULT_IP  1
#define VMC_STACK_MARGIN    16
#define VMC_MAXTHREADS     16
#define VMC_STACKSIZE   16384
#define VMC_MAXDATASLOT 16
#define VMC_MAX_B_ARG   20
#define VMC_MAX_P_ARG   20
//#define VMC_MAXSTREAMS 32
#define VMC_MAXARRAYSIZE 0xfffff
typedef VInt VByteCode;  //32 bit, to avoid alignment problem

/* structure of heap node:
	header (32 bit)
	basic data 1 (32 bits)
	basic data 2 (32 bits)
	...
	pointer data 1 (32 bits)
	pointer data 2 (32 bits)
	...
*/


#define VPTR_HEADER(x) (*(x-1))
#define HD_FIELDCNT(x) ((x)&0x000fffff)
#define HD_NODESIZE(x) (HD_FIELDCNT(x)+1)
#define HD_PFIELDCNT(x) (((x)&0x0ff00000)>>20)
#define HD_BFIELDCNT(x) (HD_FIELDCNT(x)-HD_PFIELDCNT(x))

#define HD_BARRAYSIZE(x) ( HD_FIELDCNT(x)*4 - ( ((x)&0x30000000)>>28 ) )
#define HD_PARRAYSIZE(x) HD_FIELDCNT(x)

#define HD_TUPLE(bCnt, pCnt) ( (bCnt)+(pCnt)+ ((pCnt)<<20) )
#define HD_BARRAY(bsize)      ( (((bsize)+3)>>2)+ (((-(bsize))%4)<<28) )
#define HD_PARRAY(size)      ( (size)+ 0x0ff00000 )

class VHeap{
public:
    static int init(int aInitialSize);
    static void uninit();
    static VPtr newTuple(int aCnt, int bCnt); 
    static VPtr newBArray(int byteSize);
    static VPtr newPArray(int cnt);
    static VPtr newInt(VInt aInt);
    static VPtr newStr(const char* aStr);
    static VPtr nullTuple();
    
	//test whether two heap pointers are equivalent 
	static int  equivalent(VPtr p1, VPtr p2);
    static void tag(VPtr p)     { *(p-1) |= 0x80000000; }
    static void untag(VPtr p)   { *(p-1) &= 0x7fffffff; }
    static int  isTagged(VPtr p){ return *(p-1)&0x80000000; }
	
    //get related information about p;
	static int nodeSize(VPtr p) { return HD_NODESIZE(VPTR_HEADER(p)); }
	static int fieldCnt(VPtr p) { return HD_FIELDCNT(VPTR_HEADER(p)); }
	static int bFieldCnt(VPtr p){ return HD_BFIELDCNT(VPTR_HEADER(p)); }
	static int pFieldCnt(VPtr p){ return HD_PFIELDCNT(VPTR_HEADER(p)); }
    static int bArraySize(VPtr p) { return HD_BARRAYSIZE(VPTR_HEADER(p)); }
    static int pArraySize(VPtr p) { return HD_PARRAYSIZE(VPTR_HEADER(p)); }
    //for display purpose
    static VInt asInt(VPtr p);
    static VPtr asPtr(VInt i);

    static void garbageCollection();
    static int spaceUsed();
    static int spaceAvailable();
    static int gcOccurredCnt(); //for statistic purpose
    static int getTotalAllocated(); //statistic purpose
};

enum VStopReason {
    vsrZero    =   0,
    vsrError,   //an error occurred
    vsrStop,    //stop instruction
    vsrHalt,    //halt instruction
    vsrBlock,   //blocked by send/receive
    vsrBreak,   //break point
    
    vsrLast
};
class VThread{
    //stack variables
    //the two stacks share the same piece of memory and grow in opposite direction
    VInt* memBase;
    VPtr* p_base;    //P stack base
    VPtr* p_top;     //P stack pointer
    VPtr* p_fp;      //P stack frame pointer

    VInt* b_base;    //B stack base
    VInt* b_top;     //B stack pointer
    VInt* b_fp;      //B stack frame pointer

    int IP;     //current instruction pointer
    VThread* pParent;
    VStopReason doRun(bool step);
    bool suspended; 
public:
    VThread(VThread* parent,       //parent thread
            int maxStackSize, //maximum total size of B and P stack
            int bCnt=0, //number of basic parameters from parent's B stack
            int pCnt=0  //number of pointer parameters from parent's P stack
            ); 
    ~VThread();
    void suspend() { suspended=true; }
    void resume()  { suspended=false; }
    int isRunnable() { return suspended==false; }
    VThread* getParent() { return pParent; }
    
    int   getBSize() { return b_base - b_top; }
    VInt&  getBItem(int index) { return b_top[index]; }
    int   getPSize() { return p_top-p_base; }
    VPtr&  getPItem(int index) { return p_top[-index]; }
	int   getStackCapacity()  { return b_base-(VInt*)p_base-1-VMC_STACK_MARGIN; }
    int getIP() { return IP; }
    void setIP(int aIP) { IP=aIP; }
    
    //return reason for stop
    VStopReason run();
    VStopReason stepOver();
    VStopReason stepThrough();
};

enum{
    T_FIRST = 300,
    T_CONSTANT,
    T_STRING,
    T_NEWLINE,
    T_INTEGER,
    T_FLOAT,  
    T_IDENTIFIER,
    T_LABEL,
    T_LABELDEF,
	T_EXTFUN,
    T_ERROR,
	T_EOF,
    T_LAST
};

class VTokenizer{
private:
	char buf[512];
	istream* istrm;
	int lineNo;
	char fileName[256];
	const char* errMsg;
public:
	VTokenizer();
	void attach(istream& aIs);
	int getToken();
	const char* curToken() { return buf; }
	const char* curFile()  { return fileName; }
	int curLineNo(){ return lineNo; }
	const char* curErrMsg(){ return errMsg; }
	void setLineNo(int line){ lineNo=line; }
};

class VCodeBase {
private:
    vector<VByteCode> codeVec;

    typedef map<string, int> TLabel2Addr;
    TLabel2Addr label2Addr;

    typedef map<int, VByteCode> TBp2Code;
    TBp2Code bp2Code;

    //build a map from name of operator to index into _instrSpec 
    //to facilitate compilation
    typedef map<string, int> TName2Index;
    TName2Index name2Index;
    
    //forward referenced labels be stored here
    struct TUndefinedLabel{
        string label;
        int addr;
        int lineNo;
    };
    vector<TUndefinedLabel> undefTbl;

    int doCompile(istream&);
    int compileError(const char* errMsg, const char* errCode=0);
public:
    VCodeBase();
    ~VCodeBase();
    VByteCode* getCodeArr() { return &(*codeVec.begin()); }
    int getCodeSize() { return codeVec.size(); }
    
    int loadFromFile(const char* aFilename);
    
    //deal with break points
    int isBreakPoint(int addr) { return bp2Code.find(addr)!=bp2Code.end(); }
    int setBreakPoint(int addr);
    int clearBreakPoint(int addr);
    int clearAllBreakPoints();
    void disableBreakPoint(int addr); //temperatory disable a specific break points
    void enableBreakPoint(int addr);
    int getBreakPointCnt();
    int getBreakPointAddr(int i); 

    bool isLabel(const char* label) { return getAddr(label)!=VMC_INVALID_ADDR; }
    bool hasLabel(int addr) { return getLabel(addr)!=0; } 
    int getAddr(const char* aLabel);
    const char* getLabel(int addr);

    VByteCode getByteCode(int addr);
    int unasm(int addr, ostream& os);
};

//defines internal code for each instruction
enum VOperator { 
    v_firstInstruction = 0,

	v_addI,
	v_addL,
	v_addF,
	v_addD,
	v_subI,
	v_subL,
	v_subF,
	v_subD,
	v_mulI,
	v_mulL,
	v_mulF,
	v_mulD,
	v_divI,
	v_divL,
	v_divF,
	v_divD,
	v_modI,
	v_modL,
	v_modF,
	v_modD,
	v_negI,
	v_negL,
	v_negF,
	v_negD,
    
	v_shl,
	v_shr,
	v_not, 
	v_and, 
	v_or,  
	v_bitnot,    
	v_bitand,
	v_bitor, 
	v_bitxor,
    
	v_eqB,
	v_eqU,
	v_eqP,

	v_gtI,
	v_gtL,
	v_gtF,
	v_gtD,
	v_geI,
	v_geL,
	v_geF,
	v_geD,

	v_c2i,
	v_i2c,
	v_s2i,
	v_i2s,
	v_i2l,
	v_i2f,
	v_i2d,
	v_l2i,
	v_l2f,
	v_l2d,
	v_f2i,
	v_f2l,
	v_f2d,
	v_d2i,
	v_d2l,
	v_d2f,

	v_constI, 
	v_constL, 
	v_constF, 
	v_constD, 
	v_constA, 
    
	v_popB, 
	v_popP, 
	v_dupB, 
	v_dupU, 
	v_dupP, 
	v_moveB, 
	v_moveU, 
	v_moveP, 
	v_swapB,
	v_swapU,
	v_swapP,
    
	v_ifnonzero,
	v_ifzero,
	v_ifpos,
	v_ifneg,
	v_goto,     
	v_case,     
	v_call,     
	v_xcall,    
	v_ret,      
	v_retB,     
	v_retP,     
	v_retU,     
	v_retM,
	v_callnative,


	v_newtuple,
	v_nulltuple,
	v_detuple,  
	v_tuplesizeB,
	v_tuplesizeP,
	v_getfieldB,
	v_getfieldU,
	v_getfieldP,
	v_setfieldB,
	v_setfieldU,
	v_setfieldP,
	v_newint,   

	v_newarrayC,
	v_newarrayS,
	v_newarrayB,
	v_newarrayU,
	v_newarrayP,
	
	v_newstr,   
	v_strlen,
	v_strcat,
	v_strcmp,
	v_str2i,
	v_i2str,

	v_getitemC, 
	v_getitemS, 
	v_getitemB, 
	v_getitemU, 
	v_getitemP, 

	v_setitemC, 
	v_setitemS, 
	v_setitemB, 
	v_setitemU, 
	v_setitemP, 

	v_arraysizeC,
	v_arraysizeS,
	v_arraysizeB,
	v_arraysizeU,
	v_arraysizeP,
    
/*	v_io_open,    
	v_io_close,   
	v_io_read,    
	v_io_readline,
	v_io_write,   
	v_io_seekg,   
	v_io_seekp,   
	v_io_getc, 
	v_io_putc, 
	v_io_getw, 
	v_io_putw, 
	v_io_gets, 
	v_io_puts, 
	v_io_geti, 
	v_io_puti, 
	v_io_getf, 
	v_io_putf, 
	v_io_getl, 
	v_io_putl, 
	v_io_getd, 
	v_io_putd, 
	v_io_getstr, 
	v_io_putstr, 
	v_io_getline,
	v_io_putline,
	v_printstr,
*/
	v_startthread,
	v_stopthread, 
	v_halt,       
	v_send,    
	v_receive,
    
//	v_callExtB,  //these instruction is generated by the call to external functions.
//	v_callExtP,
//	v_callExtU,
	v_callExt,

	v_checkstack,
	v_BREAK,

	v_lastInstruction
};

//possible operand types, for compilation purpose
enum VOperandType {
    otNone,       //no arguments
    otIndex,      // one positive small integer
    otTwoIndex,   // two positive small integer
	otFourIndex,  // four positive small integer
    otInt,        // one integer(32 bit)
    otLong,       // one long integer(64 bit)
    otFloat,      // one float(32 bit)
    otDouble,     // one double(64 bit)
    otString,     // one string
    otLabel,      // one label
    otLabelTwoIndex, //one label and two positive small integer(for new thread and callnative)
    otMultiLabel  // multiple label (for case)
};

//structure of instruction specification
struct VInstrSpec {
    const char* name;
    VByteCode opCode;
    VOperandType opType;
};

typedef VInt  (*ExtFunPtrB)(VInt barg[], VPtr parg[]);
typedef VPtr  (*ExtFunPtrP)(VInt barg[], VPtr parg[]);
typedef VLong (*ExtFunPtrU)(VInt barg[], VPtr parg[]); 
typedef void ExtFun(VInt barg[], VPtr parg[]);

struct VExtFunSpec{
	const char* name;
	ExtFun *funptr;
	int ibcnt;
	int ipcnt;
	int obcnt;
	int opcnt;
//	VOperator oper;
//	void * funptr;
//	int bcnt;
//	int pcnt;
};
extern VExtFunSpec _extFunSpec[];

//instruction specification array, name of the last entry is NULL to indicate the end
//the array is defined in file "vinstrspec.cpp" 
extern VInstrSpec _instrSpec[];

class VMachine {
static bool findActiveThread();
public:    
    //static istream* theIStreams[VMC_MAXSTREAMS];
    //static ostream* theOStreams[VMC_MAXSTREAMS];
    static VPtr dataSlots[VMC_MAXDATASLOT];
    static VThread* blockedThreads[VMC_MAXDATASLOT]; //threads blocked by the send/receive

    //    typedef vector<VThread*> ThreadVector;
	static void clear(int aStackSize, int aHeapSize); //clear the heap, stacks, and threads, but keep codebase intact
    static void init(int aStackSize, int aHeapSize); //clear & reset code base
    static void run(); //run current thread, current IP
    static void stepOver();
    static void stepThrough();

    static VThread* theThreads[VMC_MAXTHREADS];
//  static VHeap* theHeap;
	static VThread* curThread;
    static int curThreadID;
    static VCodeBase* theCodeBase;

};
#endif
