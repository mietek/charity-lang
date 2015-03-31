#include "vmc.h"

#include <cassert>
#include <iomanip>
#include <strstream>
#include <fstream>
#include <cstdio>

#ifndef __GNUC__
ostream& operator <<(ostream&os, VLong&t)
{
    char buf[40];
    _i64toa(t,buf,10);
    return os<<buf;
}
istream& operator >>(istream&is, VLong&t)
{
    char buf[40];
    _i64toa(t,buf,10);
    return is>>t;
}
#endif

VCodeBase* VMachine::theCodeBase=0;

//implement class VCodeBase
VCodeBase::VCodeBase()
{
    //buildup the index to lookup the _instrSpec[].
    int i;
    for(i=0;_instrSpec[i].name!=0;i++)
        name2Index.insert(TName2Index::value_type(_instrSpec[i].name, i));
    codeVec.push_back(v_halt);
}

VCodeBase::~VCodeBase()
{
}
VTokenizer* _curTk=0;
int VCodeBase::loadFromFile(const char* aFilename)
{
    ifstream is(aFilename);
    if(!is){
        cerr<<"Unable to open file "<<aFilename<<endl;
        return -1;
    }
    //save the old code & map so that we can restore them if compile not successful
    vector<VByteCode> oldcode(codeVec);
    TLabel2Addr oldmap(label2Addr);
    int r=doCompile(is);
    if(r){ //compile failed, restore
        label2Addr.swap(oldmap);
        codeVec.swap(oldcode);
    }
    undefTbl.clear();
    return r;
}
int VCodeBase::compileError(const char* errMsg, const char* errCode)
{
    cerr<<endl
        <<_curTk->curFile()<<" "
        <<"line "<<_curTk->curLineNo()<<":"<<errMsg;
    if(errCode!=0)
            cerr<<" -- "<<errCode;
    return -1;
}
/*
template <class T>
void putData(vector<VByteCode>& vec, const char* text)
{
    T t;
    istrstream is(text);
    is>>t;
    for(int i=0;i<sizeof(T)/sizeof(VByteCode);i++)
        vec.push_back(((VByteCode*)&t)[i]);
}*/
    VTokenizer tk;

int VCodeBase::doCompile(istream& theIS)
{
    tk.attach(theIS);
    _curTk=&tk;
    _curTk->setLineNo(1);
    int token;
    while((token=tk.getToken())!=T_EOF){
        switch(token){
        case T_ERROR:
            return compileError(tk.curErrMsg(),tk.curToken());
        case T_EXTFUN:
            { //the first character is "!", ignore it
              const char* extname=tk.curToken();
              VExtFunSpec* p;
              for(p=_extFunSpec; p->name!=0; p++){
                  if(strcmp(p->name,extname)==0){
                      codeVec.push_back(v_callExt);
                      codeVec.push_back(int(p->funptr));
                      codeVec.push_back(p->ibcnt);
                      codeVec.push_back(p->ipcnt);
                      codeVec.push_back(p->obcnt);
                      codeVec.push_back(p->opcnt);
                      int t=tk.getToken();
                      if(t!=T_EOF && t!=T_NEWLINE)
                            return compileError("Too many parameters");
                      break;
                  }
              }
              if(p->name==0)
                  return compileError("Undefined external function",extname);
            }
            break;
        case T_LABELDEF:
            if(isLabel(tk.curToken()))
                return compileError("Duplicate label", tk.curToken());
            label2Addr[tk.curToken()]=codeVec.size();
            break;
        case T_NEWLINE:
            break;
        case T_IDENTIFIER:
            {
                TName2Index::iterator it=name2Index.find(tk.curToken());
                if(it==name2Index.end())
                    return compileError("Invalid operator", tk.curToken());
                int index=it->second;
                codeVec.push_back(_instrSpec[index].opCode);
                bool expectNewline=true;
                switch(_instrSpec[index].opType){
                case otNone:
                    break;
                case otIndex:
                    {
                        if(tk.getToken()!=T_INTEGER)
                            return compileError("Expecting an index");
                        int n=atoi(tk.curToken());
                        if(n<0 || n> VMC_MAX_INDEX_VALUE)
                            return compileError("Index must be a small non-negative integer");
                        codeVec.push_back(n);
                    }
                    break;
                case otTwoIndex:
                    {
                        if(tk.getToken()!=T_INTEGER)
                            return compileError("Expecting two index");
                        int n1=atoi(tk.curToken());
                        if(tk.getToken()!=T_INTEGER)
                            return compileError("Expecting two index");
                        int n2=atoi(tk.curToken());
                        if(n1<0 || n2<0 || n1>VMC_MAX_INDEX_VALUE || n2>VMC_MAX_INDEX_VALUE)
                            return compileError("Index must be a small non-negative integer");
                        codeVec.push_back(n1);
                        codeVec.push_back(n2);
                    }
                    break;
                case otFourIndex:
                    {
                        for(int i=0;i<4;i++){
                            if(tk.getToken()!=T_INTEGER)
                                return compileError("Expecting four index");
                            int num=atoi(tk.curToken());
                            if(num<0 || num>VMC_MAX_INDEX_VALUE)
                                return compileError("Index must be a small non-negative integer");
                            codeVec.push_back(num);
                        }
                    }
                    break;
//VC6's template function generation seems to be problematic, so I use macro here
#define SAVE_DATA(T) { \
    T t; istrstream is(tk.curToken());  is>>t; \
    for(int i=0;i<sizeof(T)/sizeof(VByteCode);i++) \
        codeVec.push_back(((VByteCode*)&t)[i]); \
    }
                case otInt:
                    {
                        if(tk.getToken()!=T_INTEGER)
                            return compileError("Expecting one integer");
                        SAVE_DATA(VInt);
                    }
                    break;
                case otLong:
                    {
                        if(tk.getToken()!=T_INTEGER)
                            return compileError("Expecting one integer");
                        SAVE_DATA(VLong);
                    }
                    break;
                case otFloat:
                    {
                        if(tk.getToken()!=T_FLOAT)
                            return compileError("Expecting one float number");
                        SAVE_DATA(VFloat);
                    }
                    break;
                case otDouble:
                    {
                        if(tk.getToken()!=T_FLOAT)
                            return compileError("Expecting one float number");
                        SAVE_DATA(VDouble);
                    }
                    break;
                case otString:
                    {
                        if(tk.getToken()!=T_STRING)
                            return compileError("Expecting one string");
                        VByteCode n=strlen(tk.curToken())/sizeof(VByteCode)+1;
                        codeVec.push_back(strlen(tk.curToken())+1);
                        for(int i=0; i<n; i++)
                            codeVec.push_back(*(VByteCode*)(tk.curToken()+i*sizeof(VByteCode)));
                    }
                    break;
                case otLabel:
                    {
                        if(tk.getToken()!=T_LABEL)
                            return compileError("Expecting one label");
                        int addr=getAddr(tk.curToken());
                        if(addr==VMC_INVALID_ADDR){ //might be a forward reference
                            TUndefinedLabel ul;
                            ul.label= tk.curToken();
                            ul.addr = codeVec.size();
                            ul.lineNo=tk.curLineNo();
                            codeVec.push_back(0); //occupy the position
                            undefTbl.push_back(ul);
                        }
                        else{
                            codeVec.push_back(addr);
                        }
                    }
                    break;
                case otLabelTwoIndex:
                    {
                        if(tk.getToken()!=T_LABEL)
                            return compileError("First argument should be a label");
                        int addr=getAddr(tk.curToken());
                        if(addr==VMC_INVALID_ADDR){ //might be a forward reference
                            TUndefinedLabel ul;
                            ul.label= tk.curToken();
                            ul.addr = codeVec.size();
                            ul.lineNo=tk.curLineNo();
                            codeVec.push_back(0); //occupy the position
                            undefTbl.push_back(ul);
                        }
                        else{
                            codeVec.push_back(addr);
                        }
                        if(tk.getToken()!=T_INTEGER)
                            return compileError("Expecting one label and two index");
                        int n1=atoi(tk.curToken());
                        if(tk.getToken()!=T_INTEGER)
                            return compileError("Expecting one label and two index");
                        int n2=atoi(tk.curToken());
                        if(n1<0 || n2<0 || n1>VMC_MAX_INDEX_VALUE || n2>VMC_MAX_INDEX_VALUE)
                            return compileError("Index must be a small non-negative integer");
                        codeVec.push_back(n1);
                        codeVec.push_back(n2);
                    }
                    break;
                case otMultiLabel:
                    {
                        int t;
                        int pos=codeVec.size(); //remember the position to put label count
                        codeVec.push_back(0); //occupy the position first
                        int argcnt=0;
                        while((t=tk.getToken())!=T_NEWLINE && t!=T_EOF){
                            if(t!=T_LABEL)
                                return compileError("Expecting labels");
                            argcnt++;
                            int addr=getAddr(tk.curToken());
                            if(addr==VMC_INVALID_ADDR){ //might be a forward reference
                                TUndefinedLabel ul;
                                ul.label= tk.curToken();
                                ul.addr = codeVec.size();
                                ul.lineNo=tk.curLineNo();
                                codeVec.push_back(0); //occupy the position
                                undefTbl.push_back(ul);
                            }
                            else{
                                codeVec.push_back(addr);
                            }
                        }
                        if(argcnt==0)
                            return compileError("expecting at least one label");
                        //now fill the argument count
                        codeVec[pos]=argcnt;

                        expectNewline=false;
                    }
                    break;
                default:
                    assert(0); //unknow operator type
                }//end case
                if(expectNewline){
                    int t=tk.getToken();
                    if(t!=T_NEWLINE && t!=T_EOF)
                            return compileError("Too many parameters");
                }
            }//end case T_IDENTIFIER
            break;
        default:
            return compileError("Syntax error", tk.curToken());
        }//end switch
    }//end while
    //now try to resolve unsolved labels
    vector<TUndefinedLabel>::iterator it;
    for(it=undefTbl.begin(); it!=undefTbl.end(); it++){
        int addr=getAddr(it->label.c_str());
        if(addr==VMC_INVALID_ADDR){ //undefined label
            _curTk->setLineNo(it->lineNo);
            return compileError("Undefined label", it->label.c_str());
        }
        codeVec[it->addr]=addr; //set the true address
    }
    TLabel2Addr::iterator lit=label2Addr.begin();
    //ok, compile successful, now remove temperatory labels
    while(lit!=label2Addr.end()){
        if(lit->first[0]=='@'){
            label2Addr.erase(lit);
            lit=label2Addr.begin();
        }
        else
            lit++;
    }
    codeVec.push_back(v_halt); //end the program with a stop to avoid problem
    return 0;
}

VByteCode VCodeBase::getByteCode(int addr)
{
    if(addr<=0 || addr>=getCodeSize())
        return VMC_INVALID_CODE;
    if(isBreakPoint(addr))
        return bp2Code[addr];
    return codeVec[addr];
}
//return 0 if label not found
int VCodeBase::getAddr(const char* aLabel)
{
    TLabel2Addr::iterator it=label2Addr.find(aLabel);
    if(it==label2Addr.end())
        return VMC_INVALID_ADDR;
    return it->second;
}

const char* VCodeBase:: getLabel(int addr)
{
    TLabel2Addr::iterator it;
    for(it=label2Addr.begin(); it!=label2Addr.end(); it++)
        if(it->second==addr)
            return it->first.c_str();
    return 0;
}

ostream& operator <<(ostream& os, PChar pch)
{
    const char* s;
    char buf[10];
    ostrstream ss(buf,10);

    switch(pch.ch){
    case '\a':
        s="\\a";
        break;
    case '\b':
        s="\\b";
        break;
    case '\f':
        s="\\f";
        break;
    case '\n':
        s="\\n";
        break;
    case '\r':
        s="\\r";
        break;
    case '\t':
        s="\\t";
        break;
    case '\v':
        s="\\v";
        break;
    case '\"':
        s="\\\"";
        break;
    case '\'':
        s="\\\'";
        break;
    case '\\':
        s="\\\\";
        break;
    default:
        if(isprint(pch.ch)){
            return os<<pch.ch;
        }
        ss<<"\\x"<<setw(2)<<hex<<setfill('0')<<int(pch.ch)<<ends;
        s=buf;
        break;
    }
    return os<<s;
}
ostream& operator <<(ostream& os, PString s)
{
    while(s->ch!='\0')
        os<<*(s++);
    return os;
}
//un-assembly the code at addr, put the string in buf, return the
//next code's address, return 0 if error
int VCodeBase::unasm(int addr, ostream& os)
{
    VByteCode code=getByteCode(addr);
    if(code==VMC_INVALID_CODE)
        return VMC_INVALID_ADDR;

    //check if there is a label defined here
    const char* ps=getLabel(addr);
    if(ps!=0)
        os<<"\n     "<<ps<<':';
    os<<endl<<setw(5)<<setfill('0')<<addr<<setfill(' ');
    if(isBreakPoint(addr))
        os<<"*";
    else
        os<<" ";
    //find the name corresponding to the byte code
    int i;
    if(code==v_callExt){
        VExtFunSpec* efs;
        for(efs= _extFunSpec; efs->name!=0; efs++)
            if(efs->funptr==(ExtFun*)(codeVec[addr+1])){
                os<<"    !"<<efs->name;
                break;
            }
        if(efs->name==0)
            os<<"Undefined external function";
        return addr+6;
    }

    for(i=0; _instrSpec[i].name!=0; i++)
        if(_instrSpec[i].opCode==code)
            break;
    if(_instrSpec[i].name==0){
        os<<"     Unrecorgnized instruction: "<<code<<'\0';
        return addr+1;
    }
    VInstrSpec& spec=_instrSpec[i];
    os<<"    "<<spec.name;
    for(i=10-strlen(spec.name); i>=0; i--)
        os<<' ';

    addr++;
    switch(spec.opType){
    case otNone:
        break;
    case otIndex:
        os<<codeVec[addr];
        addr++;
        break;
    case otTwoIndex:
        os<<codeVec[addr]<<"  "<<codeVec[addr+1];
        addr+=2;
        break;
    case otFourIndex:
        for(i=0;i<4;i++)
            os<<codeVec[addr+i]<<"  ";
        addr+=4;
        break;
    case otInt:
        os<<*(VInt*)&codeVec[addr];
        addr+=sizeof(VInt)/sizeof(VByteCode);
        break;
    case otLong:
        os<<*(long*)&codeVec[addr];
        addr+=sizeof(VLong)/sizeof(VByteCode);
        break;
    case otFloat:
        os<<*(VFloat*)&codeVec[addr];
        addr+=sizeof(VFloat)/sizeof(VByteCode);
        break;
    case otDouble:
        os<<*(VDouble*)&codeVec[addr];
        addr+=sizeof(VDouble)/sizeof(VByteCode);
        break;
    case otString:
        {
//          char buf[512];
//          str2cstr(buf,(const char*)&codeVec[addr+1]);
//          os<<'"'<<buf<<'"';
            os<<'"'<<PString(&codeVec[addr+1])<<'"';
            addr+= 1+(codeVec[addr]+3)/sizeof(VByteCode);
        }
        break;
    case otLabel:
        {
            const char* s=getLabel(*(VInt*)&codeVec[addr]);
            if(s==0)
                os<<'@'<< (*(VInt*)&codeVec[addr]);
            else
                os<<s;
            addr+=sizeof(VInt)/sizeof(VByteCode);
        }
        break;
    case otLabelTwoIndex:
        {
            const char* s=getLabel(*(VInt*)&codeVec[addr]);
            if(s==0)
                os<<'@'<< (*(VInt*)&codeVec[addr]);
            else
                os<<s;
            addr+=sizeof(VInt)/sizeof(VByteCode);
            os<<codeVec[addr]<<"  "<<codeVec[addr+1];
            addr+=2;
        }
        break;
    case otMultiLabel:
        {
            VInt* pLabel=(VInt*)&codeVec[addr+1];
            for(int i=0;i<codeVec[addr];i++, pLabel++){
                const char* s=getLabel(*pLabel);
                if(s==0)
                    os<<'@'<<*pLabel;
                else
                    os<<s;
                os<<"  ";
            }
        }
        addr+=1+ codeVec[addr]*sizeof(VInt)/sizeof(VByteCode);
        break;
    default:
        assert(0);
    }
    return addr;
}

int VCodeBase::setBreakPoint(int addr)
{
    if(addr<=0 || addr>= codeVec.size() || isBreakPoint(addr))
        return -1;
    bp2Code[addr]=codeVec[addr];
    codeVec[addr]=v_BREAK;
    return 0;
}

int VCodeBase::clearBreakPoint(int addr)
{
    if(!isBreakPoint(addr))
        return -1;
    assert(codeVec[addr]==v_BREAK);
    codeVec[addr]=bp2Code[addr];
    bp2Code.erase(bp2Code.find(addr));
    return 0;
}
int VCodeBase::clearAllBreakPoints()
{
    TBp2Code::iterator it;
    for(it=bp2Code.begin(); it!=bp2Code.end(); it++){
        assert(codeVec[it->first]==v_BREAK);
        codeVec[it->first]=it->second;
    }
    bp2Code.clear();
    return 0;
}
int VCodeBase::getBreakPointCnt() { return bp2Code.size(); }
int VCodeBase::getBreakPointAddr(int i) //return the address of the ith break point
{
    TBp2Code::iterator it=bp2Code.begin();
    for(; i>0 && it!=bp2Code.end(); i--)
        it++;
    return it==bp2Code.end()? VMC_INVALID_ADDR: it->first;
}
void VCodeBase::disableBreakPoint(int addr)
{
    if(isBreakPoint(addr))
        codeVec[addr] = bp2Code[addr];
}
void VCodeBase::enableBreakPoint(int addr)
{
    if(isBreakPoint(addr))
        codeVec[addr] = v_BREAK;
}

