#include <cstdlib>
#include <cctype>
#include <ctime>
#include <iostream>
#include <strstream>
#include <iomanip>
#include <cstring>
using namespace std;
#include "vmc.h"
#include "cmdparse.h"

#define ITEM_TO_SHOW    10
#define ITEM_TO_UNASM   10

int _heapsize=1;
int _stacksize=1;

ostream& operator << (ostream&os, VPtr p)
{
    if(p==0)
        return os<<"NULL";
    os<<VHeap::asInt(p);
    if(VHeap::pFieldCnt(p)==0 && VHeap::bFieldCnt(p)==1)
        os<<"{"<<p[0]<<"}";
    return os;
}

void showStatus(int aThreadID=-1)
{
    if(aThreadID==-1)
        aThreadID=VMachine::curThreadID;
    if(aThreadID<0 || aThreadID>=VMC_MAXTHREADS ||VMachine::theThreads[aThreadID]==0){
        cerr<<"\nInvalid thread id:" <<aThreadID;
        return;
    }
    VThread* pthread=VMachine::theThreads[aThreadID];
	int heapsize = VHeap::spaceUsed()+VHeap::spaceAvailable();
    cout<<"\nThread "<<aThreadID
        <<"  HEAP USAGE:  "
		<<VHeap::spaceUsed()/(heapsize/100)<<"% of "<<heapsize/256<<"K"
        <<" GC="<<VHeap::gcOccurredCnt()
		<<" Total Allocation=" <<VHeap::getTotalAllocated();
	int bsize=pthread -> getBSize();
	int psize=pthread -> getPSize();
	cout<<"\nSTACK USED: "<<bsize+psize<<" / "<<pthread -> getStackCapacity();
    cout<<"\nStack B("<<bsize<<"): ";
    int i;
    for(i=0;i<ITEM_TO_SHOW && i<bsize;i++)
        cout<<pthread->getBItem(i)<<" , ";
    if(bsize>ITEM_TO_SHOW)
        cout<<"...";
    else
        cout<<"#";

    cout<<"\nStack P("<<psize<<"): ";
    for(i=0;i<ITEM_TO_SHOW && i<psize;i++)
        cout<<pthread->getPItem(i)<<" , ";
    if(psize>ITEM_TO_SHOW)
        cout<<"...";
    else
        cout<<"#";
    VMachine::theCodeBase->unasm(pthread->getIP(),cout);
}    
void showHeapNode(VPtr ph)
{
    if(ph==0){
        cout<<"NULL\n";
        return;
    }
    int bCnt=VHeap::bFieldCnt(ph);
	int pCnt=VHeap::pFieldCnt(ph);
	cout<<"["<<unsigned(ph)<<"]  #basic="<<bCnt
                           <<" #pointer="<<pCnt;
    if(bCnt>100 || pCnt>100){
        cout<<" may not be a valid heap node\n";
        return;
    }
    int i;
    for(i=0;i<bCnt ;i++)
        cout<<"\nB["<<i<<"]= "<<ph[i]<<"  HEX="<<hex<<ph[i]<<dec<<" FLOAT="<<((VFloat*)ph)[i];
    for(i=0;i<pCnt ;i++)
        cout<<"\nP["<<i+bCnt<<"]= "<<VPtr(ph[i+bCnt]);
}                                    
typedef void (*CMDProcessor)(const char* arg);
//analyze the arg and get corresponding address
//arg can be a label name(start with '@'), a number(start with a digit), or empty
//return aInitValue if arg is empty, return -1 if argument is error
static int arg2addr(int aInitValue, const char*arg)
{
    if(arg[0]=='\0')
        return aInitValue;
    if(!isdigit(arg[0])){
        int addr=VMachine::theCodeBase->getAddr(arg);
        if(addr==VMC_INVALID_ADDR)
            cerr<<"Label not found -- "<<arg;
        return addr;
    }
    int addr=atoi(arg);
    if(addr<=0 || addr>=VMachine::theCodeBase->getCodeSize()){
        cerr<<"Invalid address -- "<<arg;
        return VMC_INVALID_ADDR;
    }
    return addr;
}

void cmd_help(const char*)
{
    cout<<
      "\n load filename -- load a program into memory, default suffix is .vmc"
	  "\n reload        -- reset the system then load the last file"
      "\n exec filename -- load & run a program"
      "\n run  [address]-- run program from the first instruction or a given address"
      "\n thread n -- change current thread to thread n"
      "\n stat [n] -- show status of thread n, default is current thread"
	  "\n clear    -- clear stacks and heap, keep the loaded program and breakpoints"
      "\n reset    -- restore to initial states"
      "\n quit     -- exit the program"
      "\n ip  [address] -- show or set current address"
      "\n d   address   -- display tuple at heap address"
      "\n dstr address  -- display string at heap address"
      "\n d?  address   -- display array, ? is array type, which can be c,s,i,l,f,d" 
      "\n u   [address] -- unassembly"
      "\n p             -- proceed(step over)"
      "\n t             -- trace(step through)"
      "\n g   [address] -- run from current address and stop at given address"
      "\n setbp address -- set break point"
      "\n clrbp address -- clear break point"
      "\n clrbp         -- clear all break points"
      "\n showbp        -- show current break points"
      "\n istk  n       -- show the nth element on B stack, as an int"
      "\n fstk  n       -- show the nth element on B stack, as an float"
      "\n lstk  n       -- show the nth element on B stack, as an long"
      "\n dstk  n       -- show the nth element on B stack, as an double"
      "\n pstk  n       -- show the nth element on P stack"
	  "\n\n An address is either a number or a label"
      ;
}
static char last_file[256];
void cmd_load(const char*filename)
{
	//check if filename has a suffix
	char buf[256];
	if(strchr(filename, '.')==0){
		strcpy(buf,filename);
		strcat(buf,".vmc");
		filename=buf;
	}
	char cmdline[256];
	strcpy(cmdline, "cpp ");
	strcat(cmdline, filename);
	strcat(cmdline, " vmcload.tmp");
	if(system(cmdline)!=0){
//		cerr<<"load file without pre-processing";
//		VMachine::theCodeBase->loadFromFile(filename);
	}
	else
		VMachine::theCodeBase->loadFromFile("vmcload.tmp");
	if(filename!=last_file)
		strcpy(last_file, filename);
}
void cmd_reload(const char*)
{
    VMachine::init(_stacksize*1024*256, _heapsize*1024*256);
	cmd_load(last_file);
	cout<<"re-initialized and file "<<last_file<<" is loaded";
}
		
void cmd_run(const char* arg)
{
    int addr=arg2addr(1, arg);
	if(addr==VMC_INVALID_ADDR)
        return;
    
	VMachine::curThread->setIP(addr);
    clock_t starttime=clock();
    VMachine::run();
    clock_t t=clock()-starttime;
    cout<<"\n "<<double(t)/CLOCKS_PER_SEC<<" seconds";
    showStatus();
}    
void cmd_exec(const char*filename)
{
    //if(VMachine::theCodeBase->loadFromFile(filename)==0)
	cmd_load(filename);
    cmd_run("");
}
void cmd_stat(const char* arg)
{
    if(!isdigit(arg[0]))
        showStatus();
    else
        showStatus(atoi(arg));
}
void cmd_reset(const char*)
{
    VMachine::init(_stacksize*1024*256, _heapsize*1024*256);
	cout<<"re-initialized";
}
void cmd_clear(const char*)
{
    VMachine::clear(_stacksize*1024*256, _heapsize*1024*256);
}
void cmd_d(const char* addrstr)
{
    if(addrstr[0]=='\0'){
        cerr<<"\nrequire a heap address";
        return;
    }
    int addr=atoi(addrstr);
    if(addr==0)
        cerr<<"Invalid address--"<<addrstr;
    else
        showHeapNode(VHeap::asPtr(addr));
}
void cmd_u(const char* arg)
{
    int addr=arg2addr(VMachine::curThread->getIP(), arg);
    if(addr==-1){
//        addr=1;
        return;
    }
    for(int i=0;i<ITEM_TO_UNASM && addr<VMachine::theCodeBase->getCodeSize();i++)
        addr=VMachine::theCodeBase->unasm(addr, cout);
//    if(addr>=VMC::getCodeSize()){
//        cout<<"\n ------End of Code------";
//        addr=1;
//    }
}
void cmd_p(const char*)
{
    VMachine::stepOver();
    showStatus();
}
void cmd_t(const char*)
{
    VMachine::stepThrough();
    showStatus();
}
void cmd_g(const char*arg)
{
    if(*arg==0){
        VMachine::run();
        showStatus();
        return;
    }
    int addr=atoi(arg);
    if(addr<=0 || addr>=VMachine::theCodeBase->getCodeSize()){
        cerr<<"Invalid argument, expecting an address";
        return;
    }
    VMachine::theCodeBase->setBreakPoint(addr);
    VMachine::run();
    showStatus();
    VMachine::theCodeBase->clearBreakPoint(addr);
}
void cmd_setbp(const char* arg)
{
    int addr=arg2addr(VMachine::curThread->getIP(),arg);
    if(addr==VMC_INVALID_ADDR)
        cerr<<"Invalid argument";
    else{
        VMachine::theCodeBase->setBreakPoint(addr);
        cout<<"Break point added at "<<addr;
    }
}
void cmd_clrbp(const char* arg)
{
    if(arg[0]==0){
        VMachine::theCodeBase->clearAllBreakPoints();
        cout<<"\nAll break points cleared";
        return;
    }
    int addr=arg2addr(0,arg);
    if(addr==VMC_INVALID_ADDR)
        cerr<<"\nInvalide argument";
    else{
        if(VMachine::theCodeBase->clearBreakPoint(addr))
            cerr<<"\nNot a break point";
    }
}
void cmd_showbp(const char* arg)
{
    if(arg[0]!='\0')
        cerr<<"\nInvalid argument -- "<<arg;
    int cnt=VMachine::theCodeBase->getBreakPointCnt();
    for(int i=0;i<cnt;i++)
        VMachine::theCodeBase->unasm(VMachine::theCodeBase->getBreakPointAddr(i), cout);
}
void cmd_ip(const char* arg)
{
    if(arg[0]!='\0'){
        int addr=arg2addr(VMC_INVALID_ADDR,arg);
        if(addr!=VMC_INVALID_ADDR)
            VMachine::curThread->setIP(addr);
    }
    VMachine::theCodeBase->unasm(VMachine::curThread->getIP(),cout);
}   
#define disp_bstack(T, arg) \
{ \
    int i=atoi(arg); \
    if(arg[0]=='\0' || !isdigit(arg[0])){  \
        cerr<<"require an index as the offset into B stack"; \
        return; \
    } \
    if(i<0 || i>=VMachine::curThread->getBSize()){ \
        cerr<<"Invalid offset"; \
        return; \
    } \
    T t=*(T*)&(VMachine::curThread->getBItem(i)); \
    cout<<"B["<<i<<"]= "<< t; \
}
void cmd_istk(const char*arg)
{
    disp_bstack(VInt, arg);
}
void cmd_fstk(const char*arg)
{
    disp_bstack(VFloat,arg);
}
void cmd_lstk(const char*arg)
{
    disp_bstack(VLong, arg);
}
void cmd_dstk(const char*arg)
{
    disp_bstack(VDouble, arg);
}
void cmd_pstk(const char* arg)
{
    if(arg[0]=='\0' || !isdigit(arg[0])){
        cerr<<"require an index as the offset into P stack";
        return;
    }
    int i=atoi(arg);
    if(i<0 || i>=VMachine::curThread->getPSize()){
        cerr<<"Invalid offset";
        return;
    }
    cout<<"PSTACK["<<i<<"]= "<< VMachine::curThread->getPItem(i);
}

VPtr getArgAsPointer(const char* arg)
{
    if(!isdigit(arg[0])){
        cerr<<"require an pointer as argument";
        return 0;
    }
    int i=atoi(arg);
    if(i<0 || i>=VHeap::spaceUsed()){
        cerr<<"Invalid pointer";
        return 0;
    }
    return VHeap::asPtr(i);
}
void cmd_dstr(const char* arg)
{
    VPtr p=getArgAsPointer(arg);
	if(p==0){
		cout<<"<NULL>";
		return;
	}
	cout<<"string=\"";
	const unsigned char* str=(const unsigned char*)p;
	int len=VHeap::bArraySize(p);
	int i;
	for(i=0;i<len && str[i]!=0;i++){
		cout<<PChar(str[i]);
	}
	cout<<"\"";
	if(i>=len)
		cout<<"\nString not ended with zero";
}
void cmd_dc(const char* arg)
{
    VChar* p=(VChar*)getArgAsPointer(arg);
    if(p!=0){
        cout<<"Character Array:";
        for(int i=0;i< VHeap::bArraySize(VPtr(p)); i++){
            if(i%10==0) cout<<endl;
            if(isprint(p[i]))
                cout<<"   '"<<p[i]<<"'";
            else
                cout<<setw(6)<<unsigned(p[i]);
        }
    }
}
void cmd_dw(const char* arg)
{
    VWChar* p=(VWChar*)getArgAsPointer(arg);
    if(p!=0){
        cout<<"Wide Character Array:";
        for(int i=0;i< VHeap::bArraySize(VPtr(p))/sizeof(VWChar); i++){
            if(i%10==0) cout<<endl;
            cout<<setw(6)<<p[i];
        }
    }
}
void cmd_ds(const char* arg)
{
    VShort* p=(VShort*)getArgAsPointer(arg);
    if(p!=0){
        cout<<"Short int Array:";
        for(int i=0;i< VHeap::bArraySize(VPtr(p))/sizeof(VShort); i++){
            if(i%10==0) cout<<endl;
            cout<<setw(6)<<p[i];
        }
    }
}
void cmd_di(const char* arg)
{
    VInt* p=(VInt*)getArgAsPointer(arg);
    if(p!=0){
        cout<<"Integer Array:";
        for(int i=0;i< VHeap::bArraySize(VPtr(p))/sizeof(VInt); i++){
            if(i%5==0) cout<<endl;
            cout<<setw(12)<<p[i];
        }
    }
}
void cmd_dl(const char* arg)
{
    VLong* p=(VLong*)getArgAsPointer(arg);
    if(p!=0){
        cout<<"Long int Array:";
        for(int i=0;i< VHeap::bArraySize(VPtr(p))/sizeof(VLong); i++){
            if(i%3==0) cout<<endl;
            cout<<setw(24)<<p[i];
        }
    }
}
void cmd_df(const char* arg)
{
    VFloat* p=(VFloat*)getArgAsPointer(arg);
    if(p!=0){
        cout<<"Float Array:";
        for(int i=0;i< VHeap::bArraySize(VPtr(p))/sizeof(VFloat); i++){
            if(i%5==0) cout<<endl;
            cout<<setw(12)<<p[i];
        }
    }
}
void cmd_dd(const char* arg)
{
    VDouble* p=(VDouble*)getArgAsPointer(arg);
    if(p!=0){
        cout<<"Double float Array:";
        for(int i=0;i< VHeap::bArraySize(VPtr(p))/sizeof(VDouble); i++){
            if(i%3==0) cout<<endl;
            cout<<setw(24)<<p[i];
        }
    }
}
void cmd_thread(const char* arg)
{
    if(!isdigit(arg[0])){
        cerr<<"require a non-negative number as thread id";
        return;
    }
    int i=atoi(arg);
    if(i<0 || i>=VMC_MAXTHREADS || VMachine::theThreads[i]==0){
        cerr<<"\nInvalid thread ID";
        return;
    }
    VMachine::curThread=VMachine::theThreads[i];
    VMachine::curThreadID=i;
    cout<<"\nCurrent thread set to "<<i;
}
struct CMDItem{
    const char* name;
    CMDProcessor proc;
};
static CMDItem _cmdarr[]= {
    {"help", cmd_help} ,
    {"load", cmd_load} ,
    {"exec", cmd_exec} ,
    {"run" , cmd_run} ,
    {"thread", cmd_thread},
    {"stat", cmd_stat} ,
    {"reset",cmd_reset},
	{"clear",cmd_clear},
    {"reload", cmd_reload},
    {"d"   , cmd_d},
    {"dstr", cmd_dstr},
    {"dc"  , cmd_dc},
    {"dw"  , cmd_dw},
    {"ds"  , cmd_ds},
    {"di"  , cmd_di},
    {"dl"  , cmd_dl},
    {"df"  , cmd_df},
    {"dd"  , cmd_dd},
    {"u"   , cmd_u},
    {"p"   , cmd_p},
    {"t"   , cmd_t},
    {"g"   , cmd_g},
    {"setbp",cmd_setbp},
    {"clrbp",cmd_clrbp},
    {"showbp", cmd_showbp},
    {"ip",   cmd_ip },
    {"istk", cmd_istk},
    {"fstk", cmd_fstk},
    {"lstk", cmd_lstk},
    {"dstk", cmd_dstk},
    {"pstk", cmd_pstk}
};
void usage()
{
	cout<<"Usage: VMC [-Heap:n] [-Stack:n] [filename]\n"
		<<"   Heap:n --- specify the initial heap size(MB), default is 1\n"
		<<"  Stack:n --- specify the stack size(MB), default is 1\n"
		<<" filename --- The VMC file to be executed\n"
		;
}
int main(int argc, char* argv[])
{
	CmdlineParser cp(argc, argv, '-', "Heap:& Stack:& Pstacksize ?");
	int c;
	while((c=cp.getOption())!=0)
		switch(c){
			case 'H':
				_heapsize=atoi(cp.getValue());
				if(_heapsize<=0 || _heapsize>64){
					cerr<<"Invalid heapsize, should be from 1 to 64";
					return -1;
				}
				break;
			case 'S':
				_stacksize=atoi(cp.getValue());
				if(_stacksize<=0 || _stacksize>32){
					cerr<<"Invalid _stacksize, should be from 1 to 32";
					return -1;
				}
				break;
			case '?':
				usage();
				return 0;
			case '&':
				cerr<<"Invalid option "<<cp.getValue()<<"\n";
				usage();
				return -1;
			default:
				cerr<<"Internal error!";
				return -1;
		}
	
    VMachine::init(_stacksize*1024*256, _heapsize*1024*256);
	if(cp.parameterCount()>1){
		cerr<<"too many file names";
		usage();
		return -1;
	}
	if(cp.parameterCount()==1){ 
		//non interactive mode
		const char* filename = cp.getParameter();
		char buf[256];
		if(strchr(filename, '.')==0){
			strcpy(buf,filename);
			strcat(buf,".vmc");
			filename=buf;
		}
		char cmdline[256];
		strcpy(cmdline, "cpp ");
		strcat(cmdline, filename);
		strcat(cmdline, " vmcload.tmp");
		if(system(cmdline)!=0)
			return -1;
		if(VMachine::theCodeBase->loadFromFile("vmcload.tmp")==-1)
			return -1;
	    VMachine::run();
		return 0;
	}
#ifdef _DEBUG
	cout<<"\nVirtual Machine for Charity debug version 1.03\n"
        <<"copyright (c) 2001,2002 Charity Development Group";
#else
	cout<<"\nVirtual Machine for Charity fast version 1.03\n"
        <<"copyright (c) 2001,2002 Charity Development Group"
	    <<"\nWarning: this version should only be used to run trusted VMC code";
#endif	
    cout<<"\nEnter a command, type help for a list of available commands\n";
    char cmd[256]="";
    cout<<"VMC>> ";
    while(cin>>cmd){
        if(strcmp(cmd,"quit")==0)
            break;
        if(cmd[0]=='\0')
            break;
        CMDProcessor proc=0;
        for(int i=0; i< sizeof(_cmdarr)/sizeof(_cmdarr[0]);i++)
            if(strcmp(cmd, _cmdarr[i].name)==0){
                proc=_cmdarr[i].proc;
                break;
            }
        if(proc==0){
            cerr<<"\nInvalid command, type help for available commands";
            cin.getline(cmd,sizeof(cmd));
        }
        else{
            char arg[256];
            cin.getline(arg,sizeof(arg));
            istrstream is(arg);
			char buf[256];
			is>>buf;
            (*proc)(buf);
        }
        cout<<"\nVMC>> ";
    }
	return 0;
}
