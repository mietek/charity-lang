#include <iostream>
#include <fstream>
#include <cassert>
#include <cstdlib>
#include <cstdio>
#include <ctime>
#include <cmath>
using namespace std;
#include "vmc.h"

VThread* VMachine::curThread=0;
VThread* VMachine::theThreads[VMC_MAXTHREADS];
int VMachine::curThreadID=0;
VPtr     VMachine::dataSlots[VMC_MAXDATASLOT];
VThread* VMachine::blockedThreads[VMC_MAXDATASLOT]; //threads blocked by the send/receive


//The structure of the stack block is as this
// [Stack Margin] PPPP>>....[Stack Margin]...<<BBBB[Stack Margin]
//              ^p_base                         ^b_base
//stack margin is extra space at both end of the stack and between P and B stacks
//to prevent underflow and overflow from destroying useful data
//stack overflow occured when p_top and b_top are within distance of stack margin
//stack underflow is occured is p_top or b_top exceeded their corresponding base


#define AS_INT(x) (*(VInt*)&x)
#define B_FIRST(T)  (*(T*)b_top)
#define B_SECOND(T) (((T*)b_top)[1])

//basic operations on B and P stack
#define B_PUSH(x)  *(--b_top)=x
#define P_PUSH(x)  *(++p_top)=x  
#define B_POP (*(b_top++))
#define P_POP (*(p_top--))
#define B_POPN(n) (b_top+=(n))
#define P_POPN(n) (p_top-=(n))
#define B_ITEM(n) b_top[n]
#define P_ITEM(n) p_top[-(n)]
#define B_ENV(n)  b_fp[n]
#define P_ENV(n)  p_fp[-(n)]
#define B_SIZE (b_base-b_top)
#define P_SIZE (p_top-p_base)


VThread::VThread(VThread* parent,       //parent thread
                int maxStackSize, //maximum total size of B and P stack
                int bCnt, //number of basic parameters from parent's B stack
                int pCnt  //number of pointer parameters from parent's P stack
                )
    :pParent(parent)
{
    memBase= new VInt[maxStackSize+VMC_STACK_MARGIN*3];
    p_fp= p_top = p_base = (VPtr*)(memBase+VMC_STACK_MARGIN-1); 
    
    b_fp= b_top = b_base = memBase+maxStackSize+2*VMC_STACK_MARGIN;
    memset(memBase,0,sizeof(VInt)*(maxStackSize+VMC_STACK_MARGIN*3));
    if(pParent!=0){ 
        int i;
        for(i=0;i<bCnt; i++)
            B_PUSH(pParent->getBItem(i));
        for(i=0;i<pCnt; i++)
            P_PUSH(pParent->getPItem(i));
    }
    IP=VMC_DEFAULT_IP;
    suspended=false;
}
VThread::~VThread()
{
    delete[] memBase;
}

VStopReason VThread::run()
{
	//step through first, so that it won't break again if current address is a break point
	VStopReason r=stepThrough();
	if(r==vsrBreak) //no error occured
		r=doRun(false);
	return r;
/*    int addr=IP;
    //if the current address is a breakpoint, disable it 
    VMachine::theCodeBase->disableBreakPoint(addr);
    VStopReason r=doRun(false);
    VMachine::theCodeBase->enableBreakPoint(addr);
    return r; */
}
VStopReason VThread::stepThrough()
{
    int addr=IP;
    VMachine::theCodeBase->disableBreakPoint(addr);
    VStopReason r=doRun(true);
    VMachine::theCodeBase->enableBreakPoint(addr);
    return r;
}
VStopReason VThread::stepOver()
{
    int addr=IP;
    //if the current address is a breakpoint, disable it 
    VMachine::theCodeBase->disableBreakPoint(addr);
    int breakaddr;
    VStopReason r;
    switch(VMachine::theCodeBase->getByteCode(IP)){
    case v_call:
        breakaddr=addr+1+sizeof(VInt)/sizeof(VByteCode);
        break;
    case v_xcall:
        breakaddr=addr+1;
        break;
    default:
        return stepThrough();
    }
    if(VMachine::theCodeBase->isBreakPoint(breakaddr))
        r=doRun(false);
    else{
        VMachine::theCodeBase->setBreakPoint(breakaddr);
        r= doRun(false);
        VMachine::theCodeBase->clearBreakPoint(breakaddr);
    }
    VMachine::theCodeBase->enableBreakPoint(addr);
    return r;
}                            



//macros for checking system validity in real time, can be omit by redefine RUNCHECK
#define __RUNCHECK(cond, str) if(cond){ cerr<<str; stop=vsrError; break; }
#ifdef _DEBUG
#define RUNCHECK(cond,str)   __RUNCHECK(cond,str)
#else
#define RUNCHECK(cond,str)
#endif

#define CHECK_B_OVERFLOW(n)  RUNCHECK(p_top+VMC_STACK_MARGIN+(n) > (VPtr*)b_top, "\nB stack overflow")
#define CHECK_P_OVERFLOW(n)  RUNCHECK(p_top+VMC_STACK_MARGIN+(n) > (VPtr*)b_top, "\nP stack overflow")
#define CHECK_B_UNDERFLOW(n) RUNCHECK(B_SIZE<(n), "B stack underflow\n")
#define CHECK_P_UNDERFLOW(n) RUNCHECK(P_SIZE<(n), "P stack underflow\n")
#define CHECK_B_OFFSET(n)    RUNCHECK(n<0 || B_SIZE<=n, "B stack offset out of range\n")
#define CHECK_P_OFFSET(n)    RUNCHECK(n<0 || P_SIZE<=n, "P stack offset out of range\n")
#define CHECK_B_ENVINDEX(n)  RUNCHECK(b_fp+(n)>=b_base, "\nB frame offset out of range");
#define CHECK_P_ENVINDEX(n)  RUNCHECK(p_fp-(n)<=p_base, "\nP frame offset out of range");

//operators, so that we can use operators in the same form as  a function
#define OP_ADD(x,y) ((x)+(y))
#define OP_SUB(x,y) ((x)-(y))
#define OP_MUL(x,y) ((x)*(y))
#define OP_DIV(x,y) ((x)/(y))
#define OP_NEG(x)   (-(x))
#define OP_MOD(x,y) ((x)%(y))
#define OP_AND(x,y) ((x)&&(y))
#define OP_OR(x,y)  ((x)||(y))
#define OP_NOT(x)   (!(x))
#define OP_BITAND(x,y) ((x)&(y))
#define OP_BITOR(x,y)  ((x)|(y))
#define OP_BITXOR(x,y) ((x)^(y))
#define OP_BITNOT(x) (~(x))
#define OP_SHL(x,y) ((x)<<(y))
#define OP_SHR(x,y) ((x)>>(y))
#define OP_EQ(x,y)  ((x)==(y))
#define OP_GE(x,y)  ((x)>=(y))
#define OP_GT(x,y)  ((x)>(y))

inline VLong lABS(VLong x) { return x>0?x:-x; }

//pop an element from B stack and save it in t
//64 bit data have to be read as 2 32bit data so avoid alignment problem, which
//cause bus error on SUN SPARC
#define LOAD_VChar(t)   t=VChar(B_POP)
#define LOAD_VWChar(t)  t=VWChar(B_POP)
#define LOAD_VShort(t)  t=VShort(B_POP)
#define LOAD_VInt(t)    t=B_POP;
#define LOAD_VFloat(t)  AS_INT(t)=B_POP;
#define LOAD_VLong(t)   { ((VInt*)&t)[0]=B_POP; ((VInt*)&t)[1]=B_POP; }
#define LOAD_VDouble(t) { ((VInt*)&t)[0]=B_POP; ((VInt*)&t)[1]=B_POP; }
#define LOAD_VPtr(t)    t=P_POP;
//push an element onto B stack
#define SAVE_VChar(t)   B_PUSH(VInt(t))
#define SAVE_VWChar(t)  B_PUSH(VInt(t))
#define SAVE_VShort(t)  B_PUSH(VInt(t))
#define SAVE_VInt(t)    B_PUSH(t);
#define SAVE_VFloat(t)  B_PUSH(AS_INT(t));
#define SAVE_VLong(t)   B_PUSH(((VInt*)&t)[1]); B_PUSH(((VInt*)&t)[0]);
#define SAVE_VDouble(t) B_PUSH(((VInt*)&t)[1]); B_PUSH(((VInt*)&t)[0]);
#define SAVE_VPtr(t)    P_PUSH(t)
//for binary operators in most general form. func(T1, T2) -> T3
//T1, T2, T3 can be VInt, VFloat, VLong, VDouble
#define CASE_NORM_BINARY(opCode, T1, T2, T3, func) \
    case opCode: { \
        CHECK_B_UNDERFLOW((sizeof(T1)+sizeof(T2))/sizeof(VInt)); \
        T2 t2; LOAD_##T2(t2); \
        T1 t1; LOAD_##T1(t1); \
        T3 t3=func(t1, t2);   \
        SAVE_##T3(t3);        \
        } break;

//for unary operators in most general form. func(T1) -> T2
//T1 and T2 can be VInt, VFloat, VLong, VDouble
#define CASE_NORM_UNARY(opCode, T1, T2, func) \
    case opCode: { \
        CHECK_B_UNDERFLOW(sizeof(T1)/sizeof(VInt)); \
        T1 t1; LOAD_##T1(t1); \
        T2 t2=func(t1); \
        SAVE_##T2(t2);  \
        } break;

//for binary operators whose operand are all 1 unit datatype(VInt or VFloat)
//hope this is somewhat faster than the general one
#define CASE_FAST_BINARY(opCode, T1, T2, T3, func) \
    case opCode:    \
    B_SECOND(T3)=func(B_SECOND(T2),B_FIRST(T1)); \
    B_POP; \
    break;

//for unary operators whose operand are all 1 unit datatype        
//it should be somewhat faster than the normal one
#define CASE_FAST_UNARY(opCode, T1, T2, func) \
    case opCode:    \
    CHECK_B_UNDERFLOW(1); \
    B_FIRST(T2) = func(B_FIRST(T1)); \
    break;


VStopReason VThread::doRun(bool step)
{
    VByteCode* pcode=VMachine::theCodeBase->getCodeArr();
    int codeSize=VMachine::theCodeBase->getCodeSize();
    VStopReason stop= (step? vsrBreak: vsrZero);
    do{
        switch(pcode[IP++]){
        CASE_FAST_BINARY(v_addI, VInt,    VInt,    VInt,    OP_ADD )
        CASE_FAST_BINARY(v_addF, VFloat,  VFloat,  VFloat,  OP_ADD )
        CASE_NORM_BINARY(v_addL, VLong,   VLong,   VLong,   OP_ADD )
        CASE_NORM_BINARY(v_addD, VDouble, VDouble, VDouble, OP_ADD )
        CASE_FAST_BINARY(v_subI, VInt,    VInt,    VInt,    OP_SUB )
        CASE_FAST_BINARY(v_subF, VFloat,  VFloat,  VFloat,  OP_SUB )
        CASE_NORM_BINARY(v_subL, VLong,   VLong,   VLong,   OP_SUB )
        CASE_NORM_BINARY(v_subD, VDouble, VDouble, VDouble, OP_SUB )
        CASE_FAST_BINARY(v_mulI, VInt,    VInt,    VInt,    OP_MUL )
        CASE_FAST_BINARY(v_mulF, VFloat,  VFloat,  VFloat,  OP_MUL )
        CASE_NORM_BINARY(v_mulL, VLong,   VLong,   VLong,   OP_MUL )
        CASE_NORM_BINARY(v_mulD, VDouble, VDouble, VDouble, OP_MUL )
        CASE_FAST_BINARY(v_divI, VInt,    VInt,    VInt,    OP_DIV )
        CASE_FAST_BINARY(v_divF, VFloat,  VFloat,  VFloat,  OP_DIV )
        CASE_NORM_BINARY(v_divL, VLong,   VLong,   VLong,   OP_DIV )
        CASE_NORM_BINARY(v_divD, VDouble, VDouble, VDouble, OP_DIV )
        CASE_FAST_BINARY(v_modI, VInt,    VInt,    VInt,    OP_MOD )
        CASE_FAST_BINARY(v_modF, VFloat,  VFloat,  VFloat,  fmod   )
        CASE_NORM_BINARY(v_modL, VLong,   VLong,   VLong,   OP_MOD )
        CASE_NORM_BINARY(v_modD, VDouble, VDouble, VDouble, fmod   )
        
        CASE_FAST_UNARY(v_negI, VInt,    VInt,    OP_NEG )
        CASE_FAST_UNARY(v_negF, VFloat,  VFloat,  OP_NEG )
        CASE_NORM_UNARY(v_negL, VLong,   VLong,   OP_NEG )
        CASE_NORM_UNARY(v_negD, VDouble, VDouble, OP_NEG )

        CASE_FAST_BINARY(v_shl, VInt, VInt, VInt, OP_SHL)
        CASE_FAST_BINARY(v_shr, VInt, VInt, VInt, OP_SHR)
        CASE_FAST_BINARY(v_and, VInt, VInt, VInt, OP_AND )
        CASE_FAST_BINARY(v_or,  VInt, VInt, VInt, OP_OR )
		CASE_FAST_UNARY(v_not,  VInt, VInt, OP_NOT)
        CASE_FAST_BINARY(v_bitand, VInt, VInt, VInt, OP_BITAND )
        CASE_FAST_BINARY(v_bitor,  VInt, VInt, VInt, OP_BITOR )
        CASE_FAST_BINARY(v_bitxor, VInt, VInt, VInt, OP_BITXOR )
		CASE_FAST_UNARY(v_bitnot,  VInt, VInt, OP_BITNOT)

        //data type conversion
        CASE_FAST_UNARY(v_i2f, VInt, VFloat, VFloat)
        CASE_NORM_UNARY(v_i2l, VInt, VLong,  VLong)
        CASE_NORM_UNARY(v_i2d, VInt, VDouble, VDouble)
        CASE_FAST_UNARY(v_f2i, VFloat, VInt, VInt)
        CASE_NORM_UNARY(v_f2l, VFloat, VLong, VLong)
        CASE_NORM_UNARY(v_f2d, VFloat, VDouble, VDouble)
        CASE_NORM_UNARY(v_l2i, VLong, VInt,   VInt)
        CASE_NORM_UNARY(v_l2f, VLong, VFloat, VFloat)
        CASE_NORM_UNARY(v_l2d, VLong, VDouble, VDouble)
        CASE_NORM_UNARY(v_d2i, VDouble, VInt, VInt)
        CASE_NORM_UNARY(v_d2f, VDouble, VFloat, VFloat)
        CASE_NORM_UNARY(v_d2l, VDouble, VLong, VLong)

        CASE_FAST_BINARY(v_eqB, VInt,    VInt,    VInt, OP_EQ)
        CASE_NORM_BINARY(v_eqU, VLong,   VLong,   VInt, OP_EQ)
        case v_eqP:
			CHECK_P_UNDERFLOW(2);
			B_PUSH(VHeap::equivalent(P_ITEM(0),P_ITEM(1)));
			P_POPN(2);
			break;
		CASE_FAST_BINARY(v_gtI, VInt,    VInt,    VInt, OP_GT)
        CASE_FAST_BINARY(v_gtF, VFloat,  VFloat,  VInt, OP_GT)
        CASE_NORM_BINARY(v_gtL, VLong,   VLong,   VInt, OP_GT)
        CASE_NORM_BINARY(v_gtD, VDouble, VDouble, VInt, OP_GT)
        CASE_FAST_BINARY(v_geI, VInt,    VInt,    VInt, OP_GE)
        CASE_FAST_BINARY(v_geF, VFloat,  VFloat,  VInt, OP_GE)
        CASE_NORM_BINARY(v_geL, VLong,   VLong,   VInt, OP_GE)
        CASE_NORM_BINARY(v_geD, VDouble, VDouble, VInt, OP_GE)

/*        case v_tag:
            CHECK_P_UNDERFLOW(1);
            VHeap::tag(P_ITEM(0));
            break;
        case v_untag:
            CHECK_P_UNDERFLOW(1);
            VHeap::untag(P_ITEM(0));
            break;
        case v_iftagged:
            CHECK_P_UNDERFLOW(1);
            if(VHeap::isTagged(P_ITEM(0)))
                IP=pcode[IP];
            else
                IP++;
            break; */
        case v_constI:
        case v_constF:
        case v_constA:
            B_PUSH(pcode[IP]);
            IP++;
            break;
        case v_constL:
        case v_constD:
            B_PUSH(pcode[IP+1]);
            B_PUSH(pcode[IP]);
            IP+=2;
            break;
/*        case v_ppushnull:
            P_PUSH(VHeap::getNull());
            break; */
        case v_popB: //pop n
            CHECK_B_UNDERFLOW(pcode[IP]);
            B_POPN(pcode[IP]);
            IP++;
            break;
        case v_popP: //ppop n
            CHECK_P_UNDERFLOW(pcode[IP]);
            P_POPN(pcode[IP]);
            IP++;
            break;
        case v_dupB:
			{
				CHECK_B_OFFSET(pcode[IP]);
    	        VInt t=B_ITEM(pcode[IP]);
				B_PUSH(t);
	            IP++;
			}
            break;
        case v_dupU:
			{
				CHECK_B_OFFSET(pcode[IP]+1);
				int offset=pcode[IP];
				VInt t1=B_ITEM(offset);
				VInt t2=B_ITEM(offset+1);
				B_PUSH(t2);
				B_PUSH(t1);
            	IP++;
			}
            break;
        case v_dupP:
			{
				CHECK_P_OFFSET(pcode[IP]);
				VPtr p=P_ITEM(pcode[IP]); //to avoid problem
	            P_PUSH(p);
	            IP++;
			}
            break;
        case v_moveB:
            CHECK_B_OFFSET(pcode[IP]);
            CHECK_B_OFFSET(pcode[IP+1]);
            B_ITEM(pcode[IP+1])= B_ITEM(pcode[IP]);
            IP+=2;
            break;
        case v_moveU:
            {
                int srcidx=pcode[IP];
                int dstidx=pcode[IP+1];
                CHECK_B_OFFSET(srcidx+1);
                CHECK_B_OFFSET(dstidx+1);
                B_ITEM(dstidx)= B_ITEM(srcidx);
                B_ITEM(dstidx+1) = B_ITEM(srcidx+1);
                IP+=2;
            }
            break;
        case v_moveP:
            CHECK_P_OFFSET(pcode[IP]);
            CHECK_P_OFFSET(pcode[IP+1]);
            P_ITEM(pcode[IP+1])= P_ITEM(pcode[IP]);
            IP+=2;
            break;
        case v_swapB:
            {
                CHECK_B_UNDERFLOW(2);
                VInt t=B_ITEM(0);
                B_ITEM(0)=B_ITEM(1);
                B_ITEM(1)=t;
            }
            break;
        case v_swapU:
            {
                CHECK_B_UNDERFLOW(4);
                VInt t=B_ITEM(0);
                B_ITEM(0)=B_ITEM(2);
                B_ITEM(2)=t;
                t=B_ITEM(1);
                B_ITEM(1)=B_ITEM(3);
                B_ITEM(3)=t;
            }
            break;
        case v_swapP:
            {
                CHECK_P_UNDERFLOW(2);
                VPtr t=P_ITEM(0);
                P_ITEM(0)=P_ITEM(1);
                P_ITEM(1)=t;
            }
            break;
/*        case v_pushframe:
            B_PUSH(b_base-b_fp);   //save the relative position to bottom of the stack
            B_PUSH(p_fp+1-p_base);
            p_fp=p_top;
            b_fp=b_top+2;
            break;
        case v_popframe:   //check the top two element is the pushed frame pointer
            CHECK_B_UNDERFLOW(2);
            RUNCHECK(b_fp<b_top+2, "\nB frame underflow");
            RUNCHECK(b_fp[-1]<0 || b_fp[-2]<0, "\nInvalid frame");
            b_top=b_fp;
            p_top=p_fp;
            b_fp = b_base-b_top[-1];
            p_fp = p_base+b_top[-2]-1;
            RUNCHECK(b_fp<b_top || p_fp>p_top, "\nInvalid frame");
            break;
        case v_loadenv: //loadenv i
            CHECK_B_ENVINDEX(pcode[IP]);
            B_PUSH(B_ENV(pcode[IP]));
            IP++;
            break;
        case v_loadenv2:
            CHECK_B_ENVINDEX(pcode[IP]+1);
            B_PUSH(B_ENV(pcode[IP+1]));
            B_PUSH(B_ENV(pcode[IP]));
            IP++;
            break;
        case v_ploadenv:
            CHECK_P_ENVINDEX(pcode[IP]);
            P_PUSH(p_fp[-pcode[IP]]);
            IP++;
            break;
        case v_saveenv:
            CHECK_B_ENVINDEX(pcode[IP]);
            B_ENV(pcode[IP])=B_ITEM(0);
            B_POP;
            IP++;
            break;
        case v_saveenv2:
            CHECK_B_ENVINDEX(pcode[IP]+1);
            B_ENV(pcode[IP])=B_ITEM(0);
            B_ENV(pcode[IP+1])= B_ITEM(1);
            B_POPN(2);
            IP++;
            break;
        case v_psaveenv:
            CHECK_P_ENVINDEX(pcode[IP]);
            P_ENV(pcode[IP])=P_ITEM(0);
            P_POP;
            IP++;
            break; */
        case v_ifnonzero:
            if(B_POP!=0)
                IP=pcode[IP];
            else
                IP++;
            break;
		case v_ifzero:
            if(B_POP==0)
                IP=pcode[IP];
            else
                IP++;
            break;
		case v_ifpos:
			if(B_POP>0)
                IP=pcode[IP];
            else
                IP++;
			break;
		case v_ifneg:
			if(B_POP<0)
                IP=pcode[IP];
            else
                IP++;
			break; 
        case v_goto:
            IP=pcode[IP];
            break;
        case v_case:
            RUNCHECK(B_ITEM(0)<0 || B_ITEM(0)>= pcode[IP], "\nCase out of range");
            IP=((VInt*)&pcode[IP+1])[B_POP];
            break;
        case v_call:
            B_PUSH(IP+1);
            IP=pcode[IP];
            break;
        case v_xcall:
            {
                VInt t=B_ITEM(0);
                B_ITEM(0)=IP;
                IP=t;
            }
            break;
        case v_ret:
            IP=B_POP;
            break;
/*        case v_retn:  //retn m,n
            {
                CHECK_B_UNDERFLOW(pcode[IP]+1);
                CHECK_P_UNDERFLOW(pcode[IP+1]);
                int newip=B_ITEM(0);
                B_POPN(pcode[IP]+1);
                P_POPN(pcode[IP+1]);
                IP=newip;
            }
            break;
*/		
		case v_retB:
			{
				CHECK_B_UNDERFLOW(pcode[IP]+2);
				CHECK_P_UNDERFLOW(pcode[IP+1]);
				VInt t=B_ITEM(0);
				int ip=B_ITEM(1);
				B_POPN(pcode[IP]+1);
				P_POPN(pcode[IP+1]);
				B_ITEM(0)=t;
				IP=ip;
			}
			break;
		case v_retU:
			{
				CHECK_B_UNDERFLOW(pcode[IP]+3);
				CHECK_P_UNDERFLOW(pcode[IP+1]);
				VInt t1=B_ITEM(0);
				VInt t2=B_ITEM(1);
				int ip=B_ITEM(2);
				B_POPN(pcode[IP]+1);
				P_POPN(pcode[IP+1]);
				IP=ip;
				B_ITEM(0)=t1;
				B_ITEM(1)=t2;
			}
			break;					
		case v_retP:
			{
				CHECK_B_UNDERFLOW(pcode[IP]+1);
				CHECK_P_UNDERFLOW(pcode[IP+1]+1);
				VPtr t=P_ITEM(0);
				int ip=B_ITEM(0);
				B_POPN(pcode[IP]+1);
				P_POPN(pcode[IP+1]);
				P_ITEM(0)=t;
				IP=ip;
			}
			break;
		case v_retM:
			{
				int ibcnt=pcode[IP]; //input b count
				int ipcnt=pcode[IP+1];
				int obcnt=pcode[IP+2];//output
				int opcnt=pcode[IP+3];
				CHECK_B_UNDERFLOW(ibcnt+obcnt+1);
				CHECK_P_UNDERFLOW(ipcnt+opcnt);
				int ip=B_ITEM(obcnt);
				//move the elements on B stack
				for(int i=obcnt-1;i>=0;i--)
					B_ITEM(i+1+ibcnt)=B_ITEM(i);
				//move the elements on P stack
				if(ipcnt>0){
					for(int i=opcnt-1;i>=0;i--)
						P_ITEM(i+ipcnt)=P_ITEM(i);
				}
				B_POPN(ibcnt+1);
				P_POPN(ipcnt);
				IP=ip;
			}
			break;
        case v_callnative:
            assert(0); //unimplemented yet
            break;
		
        case v_newtuple: //newtuple nb, np
            {
                int np=pcode[IP+1];
                int nb=pcode[IP];
				int n=nb+np;
                CHECK_B_UNDERFLOW(nb);
                CHECK_P_UNDERFLOW(np);
                VPtr p=VHeap::newTuple(nb,np);
                int i;
                for(i=0;i<nb;i++)
                    p[i] = B_POP;
                for(;i<n;i++)
                    p[i] = VInt(P_POP);
                P_PUSH(p);            
                IP+=2;
            }
            break;
		case v_nulltuple:
			{
				P_PUSH(VHeap::nullTuple());
			}
			break;
#define CHECK_P_FIELD_INDEX(p,n) RUNCHECK(n< VHeap::bFieldCnt(p) || \
                                          n>=VHeap::fieldCnt(p), \
										  "\nField index out of range")
#define CHECK_B_FIELD_INDEX(p,n) RUNCHECK(n>=VHeap::bFieldCnt(p), \
                                          "\nField index out of range");        
/*        case v_getfield: //getfield n
            CHECK_P_UNDERFLOW(pcode[IP]+1);
            CHECK_B_FIELD_INDEX(P_ITEM(pcode[IP]), pcode[IP+1]);
            B_PUSH( (P_ITEM(pcode[IP]))[pcode[IP+1]] );
            IP+=2;
            break;*/
        case v_getfieldB: 
            CHECK_P_UNDERFLOW(1);
            CHECK_B_FIELD_INDEX(P_ITEM(0), pcode[IP]);
            B_PUSH( (P_POP)[pcode[IP]]);
            IP++;
            break;
/*        case v_getfield2:
            {
                int i=pcode[IP+1];
                VPtr p=P_ITEM(pcode[IP]);
				CHECK_B_FIELD_INDEX(p, i+1);
				B_PUSH( p[i+1]);
				B_PUSH( p[i]);
                IP+=2;
            }
            break; */
        case v_getfieldU:
            {
                CHECK_P_UNDERFLOW(1);
                CHECK_B_FIELD_INDEX(P_ITEM(0), pcode[IP]+1);
                VPtr p=P_POP+pcode[IP];
                B_PUSH(p[1]);
                B_PUSH(p[0]);
                IP++;
            }
            break;
/*        case v_pgetfield:
            {
                VPtr p=P_ITEM(pcode[IP]);
                CHECK_P_FIELD_INDEX(p, pcode[IP+1]);
                P_PUSH(VPtr(p[pcode[IP+1]]));
                IP+=2;
            }
            break; */
        case v_getfieldP:
            CHECK_P_UNDERFLOW(1);
            CHECK_P_FIELD_INDEX(P_ITEM(0), pcode[IP]);
            P_ITEM(0)=VPtr(P_ITEM(0)[pcode[IP]]);
            IP++;
            break;
            
/*        case v_setfield:
            CHECK_B_FIELD_INDEX(P_ITEM(pcode[IP]), pcode[IP+1]);
            (P_ITEM(pcode[IP]))[pcode[IP+1]] = B_ITEM(0);
            B_POP;
            IP+=2;
            break; */
        case v_setfieldB:
            CHECK_P_UNDERFLOW(1);
            CHECK_B_FIELD_INDEX(P_ITEM(0), pcode[IP]);
            (P_POP)[pcode[IP]]=B_POP;
            IP++;
            break;
/*        case v_setfield2:
            {
                int i=pcode[IP+1];
                VPtr p=P_ITEM(pcode[IP]);
				CHECK_B_FIELD_INDEX(p, i+1);
				p[i]  = B_POP;
                p[i+1]= B_POP;
                IP+=2;
            }
            break; */
        case v_setfieldU:
            {
                CHECK_P_UNDERFLOW(1);
                CHECK_B_FIELD_INDEX(P_ITEM(0), pcode[IP]+1);
                VPtr p=P_POP+pcode[IP];
                p[0]=B_POP;
                p[1]=B_POP;
                IP++;
            }
            break;
/*        case v_psetfield:
            {
                VPtr p=P_ITEM(pcode[IP]);
                CHECK_P_FIELD_INDEX(p, pcode[IP+1]);
                p[pcode[IP+1]]=VInt(P_ITEM(0));
                P_POP;
                IP+=2;
            }
            break; */
        case v_setfieldP:
            CHECK_P_UNDERFLOW(2);
            CHECK_P_FIELD_INDEX(P_ITEM(1), pcode[IP]);
//9.26**BUG**            P_ITEM(0)[pcode[IP]]=VInt(P_ITEM(1)); 
			P_ITEM(1)[pcode[IP]]=VInt(P_ITEM(0));
            P_POPN(2);
            IP++;
            break;
        case v_detuple:
            {
                VPtr p=P_POP;
                int bCnt=VHeap::bFieldCnt(p);
                int pCnt=VHeap::pFieldCnt(p);
                CHECK_B_OVERFLOW(bCnt);
                CHECK_P_OVERFLOW(pCnt);
                int i=bCnt+pCnt;
                while(--pCnt>=0)
                    P_PUSH(VPtr(p[--i]));
                while(--bCnt>=0)
                    B_PUSH(p[--i]);
            }
            break;
		case v_tuplesizeB:
			{
				CHECK_P_UNDERFLOW(1);
				VPtr p=P_POP;
				B_PUSH(VHeap::bFieldCnt(p));
			}
			break;
		case v_tuplesizeP:
			{
				CHECK_P_UNDERFLOW(1);
				VPtr p=P_POP;
				B_PUSH(VHeap::pFieldCnt(p));
			}
			break;

        case v_newint:
            P_PUSH(VHeap::newInt(pcode[IP]));
            IP++;
            break;
        case v_newstr:
            {
                VPtr p=VHeap::newStr((char*)&pcode[IP+1]);
                IP+=1+(pcode[IP]+3)/sizeof(VByteCode);
                P_PUSH(p);
            }
            break;
		case v_strlen:
			{
				CHECK_P_UNDERFLOW(1);
				const char* p= (const char*)(P_POP);
				B_PUSH(strlen(p));
			}
			break;
		case v_strcat:
			{

				CHECK_P_UNDERFLOW(2);
				//!! A subtle error found here: if garbage collection
				// occurs during newBArray, p1,p2 no longer point to
				// a valid area.
				//const char* p2=(const char*)(P_POP);
				//const char* p1=(const char*)(P_POP);
				int p1len = strlen((const char*)P_ITEM(1));//VHeap::bArraySize(VPtr(p1))-1;
				int p2len = strlen((const char*)P_ITEM(0));//VHeap::bArraySize(VPtr(p2))-1;

				//2003.3.17 The parameter might be some very large strings,
				//to improve speed, the following:
				//char *p=(char*)VHeap::newBArray(strlen(p1)+strlen(p2)+1);
				//strcpy(p,p1);
				//strcat(p,p2);
				//are changed to
				char *p=(char*)VHeap::newBArray(p1len+p2len+1);
				const char* p2=(const char*)(P_POP);
				const char* p1=(const char*)(P_POP);
				memcpy(p, p1, p1len);
				memcpy(p+p1len, p2, p2len);
				p[p1len+p2len] = 0;
				//end change
				P_PUSH(VPtr(p));
			}
			break;
		case v_strcmp:
			{
				CHECK_P_UNDERFLOW(2);
				const char* p2=(const char*)(P_POP);
				const char* p1=(const char*)(P_POP);
				B_PUSH(strcmp(p1,p2));
			}
			break;
		case v_str2i:
			{
				char *endptr;
				const char*str=(const char*)P_POP;
				VInt i=strtol(str, &endptr, 10);
				if(str==0 || *endptr!='\0'){ //an invalid string
					B_PUSH(i);
					B_PUSH(-1);
				}
				else{
					B_PUSH(i);
					B_PUSH(0);
				}
			}
			break;
		case v_i2str:
			{
				char buf[100];
				int i=B_POP;
				sprintf(buf, "%d", i);
				P_PUSH(VHeap::newStr(buf));
			}
			break;
#define CASE_NEWBARRAY(opCode, T) \
    case opCode: \
		CHECK_B_UNDERFLOW(1); \
        RUNCHECK(B_ITEM(0)<=0 || B_ITEM(0)*sizeof(T)>=VMC_MAXARRAYSIZE, "Invalid array size"); \
        P_PUSH(VHeap::newBArray(B_POP*sizeof(T))); \
        break;

        CASE_NEWBARRAY(v_newarrayC, VChar)
        CASE_NEWBARRAY(v_newarrayS, VShort)
        CASE_NEWBARRAY(v_newarrayB, VInt)
        CASE_NEWBARRAY(v_newarrayU, VLong)
        case v_newarrayP:
            RUNCHECK(B_ITEM(0)<=0 || B_ITEM(0)*sizeof(VPtr)>=VMC_MAXARRAYSIZE, "Invalid array size"); \
            P_PUSH(VHeap::newPArray(B_POP) );
            break;

#define CHECK_ARRAYSIZE(p, size) RUNCHECK(VHeap::bArraySize(p)<=(size),"\nArray index out of range"); 
#define CASE_GETARR(opCode, T) \
    case opCode: { \
        CHECK_P_UNDERFLOW(1); \
		VPtr p=P_POP;  \
        CHECK_ARRAYSIZE(p, B_ITEM(0)*sizeof(T)); \
        B_ITEM(0)= VInt( ((T*)p)[B_ITEM(0)]); \
        } break;
 
        CASE_GETARR(v_getitemC, VChar)        
        CASE_GETARR(v_getitemS, VShort)
        CASE_GETARR(v_getitemB, VInt)

        case v_getitemU:
            {
		        CHECK_P_UNDERFLOW(1); \
                VPtr p=P_POP;
                CHECK_ARRAYSIZE(p, B_ITEM(0)*sizeof(VLong));
                int i=B_POP*(sizeof(VLong)/sizeof(VInt));
                B_PUSH(p[i+1]);
                B_PUSH(p[i]);
            }
            break;
        case v_getitemP:
            {
		        CHECK_P_UNDERFLOW(1); \
                VPtr p=P_POP;
                CHECK_ARRAYSIZE(p, B_ITEM(0)*sizeof(VPtr));
                P_PUSH( ((VPtr*)p)[B_POP]);
            }
            break;
#define CASE_SETARR(opCode, T) \
    case opCode: { \
        CHECK_P_UNDERFLOW(1); \
        VPtr p=P_ITEM(0);  \
		T item=T(B_POP); \
        int i=B_POP; \
        CHECK_ARRAYSIZE(p, i*sizeof(T)); \
        ((T*)p)[i]=item; \
        } break;

        CASE_SETARR(v_setitemC, VChar)
        CASE_SETARR(v_setitemS, VShort)
        CASE_SETARR(v_setitemB, VInt)
        case v_setitemU:
            {
                CHECK_P_UNDERFLOW(1); 
                VPtr p=P_ITEM(0); 
                CHECK_ARRAYSIZE(p, B_ITEM(0)*sizeof(VLong));
                int i=B_ITEM(2)*2;
                p[i]=B_ITEM(0);
                p[i+1]=B_ITEM(1);
				B_POPN(3);
            }
            break;
        case v_setitemP:
            {
                CHECK_P_UNDERFLOW(2); 
				VPtr data=P_POP;
                VPtr p=P_ITEM(0); 
                CHECK_ARRAYSIZE(p, B_ITEM(0)*sizeof(VPtr));
                p[B_POP]=AS_INT(data);
            }
            break;

#define CASE_ARRAYSIZE(opCode, T) \
    case opCode: \
        CHECK_P_UNDERFLOW(1); \
    	B_PUSH(VHeap::bArraySize(P_POP)/sizeof(T));\
        break;

        CASE_ARRAYSIZE(v_arraysizeC, VChar)
        CASE_ARRAYSIZE(v_arraysizeS, VShort)
        CASE_ARRAYSIZE(v_arraysizeB, VInt)
        CASE_ARRAYSIZE(v_arraysizeU, VLong)
    case v_arraysizeP:
        CHECK_P_UNDERFLOW(1); \
        B_PUSH(VHeap::pArraySize(P_POP));
        break;
	case v_callExt:
		{
			ExtFun* fptr=(ExtFun*)(pcode[IP]);
			int ibcnt=pcode[IP+1];
			int ipcnt=pcode[IP+2];
			int obcnt=pcode[IP+3];
			int opcnt=pcode[IP+4];
			RUNCHECK( ibcnt>VMC_MAX_B_ARG || 
	                  ipcnt>VMC_MAX_P_ARG ||
					  obcnt>VMC_MAX_B_ARG ||
					  opcnt>VMC_MAX_P_ARG , "too many arguments for external functions");
			CHECK_B_UNDERFLOW(ibcnt);
			CHECK_P_UNDERFLOW(ipcnt);

			VInt barg[VMC_MAX_B_ARG];
			VPtr parg[VMC_MAX_P_ARG];
			int i;
			for(i=0;i<ibcnt;i++)
				barg[i]=B_POP;
			for(i=0;i<ipcnt;i++)
				parg[i]=P_POP;
			(*fptr)(barg,parg);
			for(i=obcnt-1;i>=0;i--)
				B_PUSH(barg[i]);
			for(i=opcnt-1;i>=0;i--)
				P_PUSH(parg[i]);
			IP+=5;
		}
		break;
/*
	case v_callExtB:
		{
			ExtFunPtrB fptr=ExtFunPtrB(pcode[IP]);
			int bcnt=pcode[IP+1];
			int pcnt=pcode[IP+2];
			CHECK_B_UNDERFLOW(bcnt);
			CHECK_P_UNDERFLOW(pcnt);
			VInt barg[20];
			VPtr parg[10];
			for(int i=0;i<bcnt && i<20;i++)
				barg[i]=B_POP;
			for(int j=0;j<pcnt && j<10;j++)
				parg[j]=P_POP;
			B_PUSH((*fptr)(barg,parg));
			IP+=3;
		}
		break;
	case v_callExtU:
		{
			ExtFunPtrU fptr=ExtFunPtrU(pcode[IP]);
			int bcnt=pcode[IP+1];
			int pcnt=pcode[IP+2];
			CHECK_B_UNDERFLOW(bcnt);
			CHECK_P_UNDERFLOW(pcnt);
			VInt barg[20];
			VPtr parg[10];
			for(int i=0;i<bcnt && i<20;i++)
				barg[i]=B_POP;
			for(int j=0;j<pcnt && j<10;j++)
				parg[j]=P_POP;
			VLong r=(*fptr)(barg,parg);
			SAVE_VLong(r);
			IP+=3;
		}
		break;
	case v_callExtP:
		{
			ExtFunPtrP fptr=ExtFunPtrP(pcode[IP]);
			int bcnt=pcode[IP+1];
			int pcnt=pcode[IP+2];
			CHECK_B_UNDERFLOW(bcnt);
			CHECK_P_UNDERFLOW(pcnt);
			VInt barg[20];
			VPtr parg[10];
			for(int i=0;i<bcnt && i<20;i++)
				barg[i]=B_POP;
			for(int j=0;j<pcnt && j<10;j++)
				parg[j]=P_POP;
			P_PUSH((*fptr)(barg,parg));
			IP+=3;
		}
		break;
*/
/*

#define IS_ISTREAM(h) (h>=0 && h<VMC_MAXSTREAMS && VMachine::theIStreams[h]!=0)
#define IS_OSTREAM(h) (h>=0 && h<VMC_MAXSTREAMS && VMachine::theOStreams[h]!=0)
#define ISTREAM(h) (*VMachine::theIStreams[h])
#define OSTREAM(h) (*VMachine::theOStreams[h])
        case v_io_open:
            {
                CHECK_P_UNDERFLOW(1); //p top is file name
                CHECK_B_UNDERFLOW(1);
                fstream* fs=new fstream((char*)P_POP, B_POP);
                if(fs){
                    int i;
                    for(i=3;i<VMC_MAXSTREAMS;i++){
                        if(VMachine::theIStreams[i]==0 && VMachine::theOStreams[i]==0){
                            VMachine::theIStreams[i]=fs;
                            VMachine::theOStreams[i]=fs;
                            B_PUSH(i);
                            break;
                        }
                    }
                    if(i>=VMC_MAXSTREAMS){
                        cerr<<"\nToo many files open";
                        B_PUSH(-1);
                    }
                }
                else
                    B_PUSH(-1);
            }
            break;
        case v_io_close:
            {
                int handle=B_POP;
                if(handle<3 || handle>=VMC_MAXSTREAMS || 
                    (VMachine::theIStreams[handle]==0 && VMachine::theOStreams[handle]==0)){
                    B_PUSH(-1);
                }
                else{
                    if(VMachine::theIStreams[handle]!=0) //delete only once
                        delete VMachine::theIStreams[handle];
                    else
                        delete VMachine::theOStreams[handle];
                    VMachine::theIStreams[handle]=0;
                    VMachine::theOStreams[handle]=0;
                    B_PUSH(0);
                }
            }
            break;
        case v_io_read:
            {   
                CHECK_B_UNDERFLOW(2);
                CHECK_P_UNDERFLOW(1);
                int handle=B_POP;
                int size=B_POP;
                VPtr p=P_POP;
                CHECK_ARRAYSIZE(p, size);
                if(IS_ISTREAM(handle) && ISTREAM(handle).read((char*)p, size))
                    B_PUSH(ISTREAM(handle).gcount());
                else
                    B_PUSH(-1);
            }
            break;
        case v_io_readline:
            {
                CHECK_B_UNDERFLOW(2);
                int handle=B_POP;
                VPtr p=P_POP;
                int maxsize=VHeap::bArraySize(p);
                if(IS_ISTREAM(handle) && ISTREAM(handle).getline((char*)p, maxsize) )
                    B_PUSH(0);
                else
                    B_PUSH(-1);
            }
            break;
        case v_io_write:
            {   //on top of B: handle, size
                CHECK_B_UNDERFLOW(2);
                CHECK_P_UNDERFLOW(1);
                int handle=B_POP;
                int size=B_POP;
                VPtr p=P_POP;
                CHECK_ARRAYSIZE(p, size);
                if(IS_OSTREAM(handle) && OSTREAM(handle).write((char*)p, size))
                    B_PUSH(0);
                else
                    B_PUSH(-1);
            }
            break;
        case v_io_seekg:
            {
                CHECK_B_UNDERFLOW(2);
                int handle=B_POP;
                int offset=B_POP;
                if(IS_ISTREAM(handle) && ISTREAM(handle).seekg(offset))
                    B_PUSH(0);
                else
                    B_PUSH(-1);
            }
            break;
        case v_io_seekp:
            {
                CHECK_B_UNDERFLOW(2);
                int handle=B_POP;
                int offset=B_POP;
                if(IS_OSTREAM(handle) && OSTREAM(handle).seekp(offset))
                    B_PUSH(0);
                else
                    B_PUSH(-1);
            }
            break;
#define CASE_IO_GETDATA(opCode, T) \
        case opCode: { \
            int handle=B_POP; \
            T t; \
            if(IS_ISTREAM(handle) && (ISTREAM(handle)>> t) ){ \
                SAVE_##T(t); \
                B_PUSH(0);   \
            } \
            else \
                B_PUSH(-1);  \
            } break;
                
        CASE_IO_GETDATA(v_io_getc, VChar)
//        CASE_IO_GETDATA(v_io_getw, VWChar)
        CASE_IO_GETDATA(v_io_gets, VShort)
        CASE_IO_GETDATA(v_io_geti, VInt)
        CASE_IO_GETDATA(v_io_getf, VFloat)
        CASE_IO_GETDATA(v_io_getl, VLong)
        CASE_IO_GETDATA(v_io_getd, VDouble)

#define CASE_IO_PUTDATA(opCode, T) \
    case opCode: { \
        CHECK_B_UNDERFLOW(1+sizeof(T)/sizeof(VInt)); \
        int handle=B_POP; \
        T t; LOAD_##T(t); \
        if(IS_OSTREAM(handle) && (OSTREAM(handle)<< t) ) \
            B_PUSH(0); \
        else \
            B_PUSH(-1); \
        } break;

        CASE_IO_PUTDATA(v_io_putc, VChar)
        CASE_IO_PUTDATA(v_io_putw, VWChar)
        CASE_IO_PUTDATA(v_io_puts, VShort)
        CASE_IO_PUTDATA(v_io_puti, VInt)
        CASE_IO_PUTDATA(v_io_putf, VFloat)
        CASE_IO_PUTDATA(v_io_putl, VLong)
        CASE_IO_PUTDATA(v_io_putd, VDouble)

        case v_io_getstr:
            {
                CHECK_B_UNDERFLOW(1);
                char buf[1024];
                int handle=B_POP;
                if(IS_ISTREAM(handle) && (ISTREAM(handle)>>buf) ){ 
                    P_PUSH(VHeap::newStr(buf));
                    B_PUSH(0);
                }    
                else 
                    B_PUSH(-1);  
            } 
            break;

        case v_io_putstr:
            {
                CHECK_B_UNDERFLOW(1);
                CHECK_P_UNDERFLOW(1);
                int handle=B_POP;
                char* str= (char*)(P_POP);
                if(IS_OSTREAM(handle) && OSTREAM(handle)<<str)
                    B_PUSH(0);
                else
                    B_PUSH(-1);
            }
            break;
        case v_io_getline:
            {
                CHECK_B_UNDERFLOW(1);
                char buf[1024];
                int handle=B_POP;
                if(IS_ISTREAM(handle) && ISTREAM(handle).getline(buf,sizeof(buf))){
                    P_PUSH(VHeap::newStr(buf));
                    B_PUSH(0);
                }
                else
                    B_PUSH(-1);
            }
            break;
        case v_io_putline:
            {
                CHECK_P_UNDERFLOW(1);
                CHECK_B_UNDERFLOW(1);
                int handle=B_POP;
                char* p=(char*)P_POP;
                if(IS_OSTREAM(handle) && (OSTREAM(handle)<<p<<endl))
                    B_PUSH(0);
                else
                    B_PUSH(-1);
            }
            break;
        case v_printstr:
            {
                OSTREAM(1)<<(char*)&pcode[IP+1];
                IP+=1+(pcode[IP]+3)/sizeof(VByteCode);
            }
            break;
		*/
		case v_startthread: //startthread label bcnt pcnt
            {
                //find an empty slot
                int i;
                for(i=0;i<VMC_MAXTHREADS;i++)
                    if(VMachine::theThreads[i]==0)
                        break;
                if(i>=VMC_MAXTHREADS){
                    cerr<<"\nToo many threads";
                    stop=vsrError;
                }
                else{
                    int bcnt=pcode[IP+sizeof(VInt)/sizeof(VByteCode)];
                    int pcnt=pcode[IP+1+sizeof(VInt)/sizeof(VByteCode)];
                    CHECK_B_UNDERFLOW(bcnt);
                    CHECK_P_UNDERFLOW(pcnt);
                    VMachine::theThreads[i]=new VThread(this, VMC_STACKSIZE, bcnt,pcnt);
                    VMachine::theThreads[i]->setIP(pcode[IP]);
                    IP+=sizeof(VInt)/sizeof(VByteCode)+2;
                    B_POPN(bcnt);
                    P_POPN(pcnt);
                }
            }
            break;
        case v_stopthread:
            // stop execution and destroy the thread
            IP--;
            stop=vsrStop;
            break;
        case v_send:
            {
                CHECK_P_UNDERFLOW(1);
                RUNCHECK(pcode[IP]>=VMC_MAXDATASLOT, "Interface number out of range");
                int i=pcode[IP];
                VMachine::dataSlots[i]=P_POP;
                IP++;
                //suspend oneself and wake up the other thread
                if(VMachine::blockedThreads[i]!=0)
                    VMachine::blockedThreads[i]->resume();
                VMachine::blockedThreads[i]=this;
                suspend();
                stop=vsrBlock;
            }
            break;
        case v_receive:
            {
                RUNCHECK(pcode[IP]>=VMC_MAXDATASLOT, "Interface number out of range");
                int i=pcode[IP];
                if(VMachine::blockedThreads[i]==0){ //no data yet, suspend at this instruction
                    IP--;
                    VMachine::blockedThreads[i]=this;
                    suspend();
                    stop=vsrBlock;
                }
                else{
                    P_PUSH(VMachine::dataSlots[i]);
                    VMachine::blockedThreads[i]->resume();
                    IP++;
                }
            }
            break;

        case v_halt:
            stop=vsrHalt;
            IP--; //keep IP unchanged (IP was ++ previous)
            break;
        case v_BREAK:
            stop=vsrBreak;
            IP--; 
            break;            
        default:
            cerr<<"unknown instruction id -- "<<pcode[IP-1]<<endl;
            stop=vsrError;
        }//end switch
        CHECK_B_OVERFLOW(0);
        RUNCHECK(IP<0 || IP>=codeSize, "IP out of range");
    }while(stop==vsrZero);//end while
    if(stop==vsrError)
        IP--;
    return stop;
}

bool VMachine::findActiveThread()
{
    int cnt=0;
    for(int i=0;i<VMC_MAXTHREADS;i++)
        if(theThreads[i]!=0){
            cnt++;
            if(theThreads[i]->isRunnable()){
                curThreadID=i;
                curThread=theThreads[i];
                return true;
            }
        }
//    if(cnt>0)
//        cerr<<"\nDead lock detected, no thread to run";

    curThread=theThreads[0];
    curThreadID=0;
    return false; 
}
void VMachine::run()
{
    if(curThread==0){
        cerr<<"\nNo thread to run";
        return;
    }
    while(1){
        switch(curThread->run()){
        case vsrStop:
            //the thread is to be destroyed
            if(curThreadID!=0){
                delete curThread;
                theThreads[curThreadID]=0;
            }
            else
                curThread->suspend();
            if(findActiveThread()==false) //
                return;
            break;
        case vsrError:
        case vsrHalt:
        case vsrBreak:   //break point
            return;
        case vsrBlock:   //blocked by send/receive
            if(findActiveThread()==false) //
                return;
        }
    }//end while
}
void VMachine::stepOver()
{
    if(curThread==0){
        cerr<<"\nNo thread to run";
        return;
    }
    switch(curThread->stepOver()){
    case vsrStop:
        //the thread is to be destroyed
        if(curThreadID!=0){
            delete curThread;
            theThreads[curThreadID]=0;
        }
        else
            curThread->suspend();
        findActiveThread();
        break;
    case vsrBlock:   //blocked by send/receive
        findActiveThread();
        break;
    default:
        break;
    }
}
void VMachine::stepThrough()
{
    if(curThread==0){
        cerr<<"\nNo thread to run";
        return;
    }
    switch(curThread->stepThrough()){
    case vsrStop:
        //the main thread is not to be destroyed
        if(curThreadID!=0){
            delete curThread;
            theThreads[curThreadID]=0;
        }
        else
            curThread->suspend();
        findActiveThread();
        break;
    case vsrBlock:   //blocked by send/receive
        findActiveThread();
        break;
    default:
        break;
    }
}
void VMachine::clear(int aStackSize, int aHeapSize)
{
    int i;
    for(i=0;i<VMC_MAXTHREADS;i++){
        delete theThreads[i];
        theThreads[i]=0;
    }
    for(i=0;i<VMC_MAXDATASLOT; i++){
        dataSlots[i]=0;
        blockedThreads[i]=0;
    }
    VHeap::init(aHeapSize);
	theThreads[0]=new VThread(0, aStackSize);
    curThread=theThreads[0];
    curThreadID = 0;
}
void VMachine::init(int aStackSize, int aHeapSize)
{
	clear(aStackSize, aHeapSize);
	delete theCodeBase;
	theCodeBase=new VCodeBase;
}
/*
int VMachine::openStream(const char* name, int mode)
{
    fstream* fs=new fstream(name, mode);
    if(fs){
        for(int i=3;i<VMC_MAXSTREAMS;i++)
            if(theIStreams[i]==0 && theOStreams[i]==0){
                theIStreams[i]=fs;
                theOStreams[i]=fs;
                return i;
            }
    }
    delete fs;
    return -1;
}

int VMachine::closeStream(int handle)
{
    if(handle<3 || handle>=VMC_MAXSTREAMS)
        return -1;
    if(theIStreams[handle]!=0)
        delete theIStreams[handle];
    else
        delete theOStreams[handle];
    theIStreams[handle]=0;
    theOStreams[handle]=0;
    return 0;
}
int VMachine::seekp(int handle, int offset)
{
    if(handle<3 || handle>=VMC_MAXSTREAMS || theOStreams[handle]==0)
        return -1;
    if(theOStreams[handle]->seekp(offset))
        return 0;
    return -1;
}
int VMachine::seekg(int handle, int offset)
{
    if(handle<0 || handle>=VMC_MAXSTREAMS || theIStreams[handle]==0)
        return -1;
    if(theOStreams[handle]->seekp(offset))
        return 0;
    return -1;
}
int VMachine::getChar(int handle)
{
    if(handle<0 || handle>=VMC_MAXSTREAMS || theIStreams[handle]==0)
        return -1;
    return theIStreams[handle]->get();
}
int VMachine::putChar(int handle, int aChar)
{
    if(handle<0 || handle>=VMC_MAXSTREAMS || theOStreams[handle]==0)
        return -1;
    if(theOStreams[handle]->put(aChar))
        return 0;
    return -1;
}
int VMachine::read(int handle, char* buf, int size)
{
    if(handle<0 || handle>=VMC_MAXSTREAMS || theIStreams[handle]==0)
        return -1;
    if(theIStreams[handle]->read(buf, size))
        return theIStreams[handle]->gcount();
    return -1;
}
int VMachine::write(int handle, char* buf, int size)
{
    if(handle<0 || handle>=VMC_MAXSTREAMS || theOStreams[handle]==0)
        return -1;
    if(theOStreams[handle]->write(buf, size))
        return theIStreams[handle]->gcount();
    return -1;
}
int VMachine::getString(int handle, char* aStr)
{
    if(handle<0 || handle>=VMC_MAXSTREAMS || theIStreams[handle]==0)
        return -1;
    if(*theIStreams[handle] >> aStr)
        return strlen(aStr);
    return -1;
}
    
int VMachine::putString(int handle, char* aStr)
{
    if(handle<0 || handle>=VMC_MAXSTREAMS || theOStreams[handle]==0)
        return -1;
    if(*theOStreams[handle] <<aStr)
        return 0;
    return -1;
}*/

