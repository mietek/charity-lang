//implementation of class CmdlineParser
//[-<1993.6.16
//modified 2002.3.15
#include "cmdparse.h"
#include <ctype.h>
/* modified in 1993.9.16:
        when switchchar=='\0',all the argument on command line are
        considered both option and parameters.
*/

CmdlineParser::CmdlineParser(int argc, char*argv[], char sw,const char*s)
{
    _argc=argc;
    _argv=argv;
    switchchar=sw;
    format=s;
    curparam=0;
    curoption=0;
    value = 0;
}

char CmdlineParser::getOption()
{       //curoption points to the argument parsed last time.
        //find next option
        do{
                if(++curoption>=_argc) return 0;
                value=_argv[curoption];
        //}while(*value!=switchchar);
        }while(*value!=switchchar && switchchar!='\0');/*93.9.16*/
        //now value points to the option string currently dealing.
        const char *pf=format;
        const char *pa;

        while(1){
                while(isspace(*pf)) pf++; //skip all spaces

                if(*pf=='\0') break;  //end of format,can't find a match
                char key=*pf; //the 'key' letter to be returned.
                //pa=value+1;   //skip switch char
                pa=value; /*93.9.16*/
                if(switchchar) pa++; /*93.9.16*/
                while( toupper(*pf++)==toupper(*pa++) ){
                        //options without argument?
                        if(isspace(*pf) || *pf=='\0'){
                                if(*pa=='\0' ) return key; // match
                                break; //not match,continue
                        }
                        //options with argument?
                        if(*pf=='&'){
                                value=pa;
                                return key;
                        }
                        //if found a upcase letter for the first time,
                        //change key.
                        if(!isupper(key) && isupper(*pf))
                                key=*pf;
                }//end while

                //find next option string in format
                while(!isspace(*pf) && *pf!='\0') pf++;
        }//end while
        //illegal option
        return '&';
}

const char* CmdlineParser::getParameter()
{
        while(++curparam<_argc){
                register const char*p=_argv[curparam];
                if(*p!=switchchar)
                        return p;
        }
        return 0;
}

int CmdlineParser::parameterCount()
{
        int count=0;
        for(int i=1;i<_argc;i++)
                if(*_argv[i]!=switchchar) count++;
        return count;
}
