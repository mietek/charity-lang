#include <assert.h>
#include <ctype.h>
#include "vmc.h"
/*
comment     ";".*
delimiter   [ \t]
continue    \\\n
newline     \n
whitespace  {delimiter}+
letter      [A-Z]|[a-z]
digit       [0-9]
identifier  {letter}(_|{letter}|{digit})*
label       (@|_)(_|{letter}|{digit})*
labeldef    {label}:
integer     (\-)?{digit}+
float       (\-)?{digit}+\.{digit}*
extfun      !{identifier}
*/

VTokenizer::VTokenizer()
:istrm(0)
{
    lineNo=1;
    fileName[0]=0;
}
void VTokenizer::attach(istream& aIs)
{
    istrm=&aIs;
}
enum{
    stReady,
    stIdentifier,
    stNumber,
    stFloat,
    stComment,
    stExtFun,
    stLabel,
    stLocalLabel,
    stString,
    stEscape,
    stEscape0,
    stEscapeHex,
};

int VTokenizer::getToken()
{
    int state=stReady;
    int bufptr=0;
    buf[bufptr]=0;
    while(1){
        int c=istrm->get();

        switch(state){
        case stReady:
            switch(c){
            case -1:
                return T_EOF;
            case '\n':
                lineNo++;
                return T_NEWLINE;
            case '|':
                return T_NEWLINE; //equivalent to a newline

            case '\r': //ignore the \r
            case '\t': //delimiters
            case ' ':
                break;
            case ';': //comments
                state=stComment;
                break;
            case '!': //external functions
                state=stExtFun;
                break;
            case '@':
                buf[bufptr++]=c;
                buf[bufptr]=0;
                state=stLocalLabel;
                break;

            case '_':
                buf[bufptr++]=c;
                buf[bufptr]=0;
                state=stLabel;
                break;
            case '"':
                state=stString;
                break;
            case '#': //pre-process command
                {
                    char tempbuf[256];
                    istrm->putback(c);
                    (*istrm)>>tempbuf;
                    if(strcmp(tempbuf,"#")==0 ||
                       strcmp(tempbuf,"#line")==0 ||
                       strcmp(tempbuf,"#LINE")==0 ||
                       strcmp(tempbuf,"#Line")==0){
                        (*istrm)>>lineNo;
                        (*istrm)>>fileName;
                        istrm->ignore(255,'\n');
                    }
                    else{ //unknown directive, simply ignore
                        istrm->ignore(255,'\n');
                    }
                }
                break;

            default:
                buf[bufptr++]=c;
                buf[bufptr]=0;
                if(c=='-' || c=='+' || isdigit(c))
                    state=stNumber;
                else if(isalpha(c))
                    state=stIdentifier;
                else{
                    errMsg="Undefined character";
                    return T_ERROR;
                }
            }//end switch(c)
            break;
        case stComment:
            switch(c){
            case '\n':
                lineNo++;
                return T_NEWLINE;
            case -1:
                return T_EOF;
            default:
                break;
            }
            break;
        case stExtFun:
            if(c=='_' || isalnum(c)){
                buf[bufptr++]=c;
            }
            else{
                buf[bufptr]=0;
                istrm->putback(c);
                return T_EXTFUN;
            }
            break;
        case stNumber:
            if(isdigit(c))
                buf[bufptr++]=c;
            else if(c=='.'){
                buf[bufptr++]=c;
                state=stFloat;
            }
            else{
                buf[bufptr]=0;
                istrm->putback(c);
                return T_INTEGER;
            }
            break;
        case stFloat:
            if(isdigit(c))
                buf[bufptr++]=c;
            else{
                buf[bufptr]=0;
                istrm->putback(c);
                return T_FLOAT;
            }
            break;
        case stLabel:
            if(isalnum(c) || c=='_' || c=='$')
                buf[bufptr++]=c;
            else if(c==':'){
                buf[bufptr]=0;
                return T_LABELDEF;
            }
            else{
                buf[bufptr]=0;
                istrm->putback(c);
                return T_LABEL;
            }
            break;
        case stLocalLabel:
            if(isalnum(c) || c=='_' || c=='$')
                buf[bufptr++]=c;
            else if(c==':'){
                buf[bufptr]=0;
                //suffix local label with the filename to make it unique
                strcat(buf, " in ");
                strcat(buf, fileName);
                return T_LABELDEF;
            }
            else{
                buf[bufptr]=0;
                strcat(buf, " in ");
                strcat(buf, fileName);
                istrm->putback(c);
                return T_LABEL;
            }
            break;
        case stIdentifier:
            if(isalnum(c))
                buf[bufptr++]=c;
            else{
                buf[bufptr]=0;
                istrm->putback(c);
                return T_IDENTIFIER;
            }
            break;
        case stString:
            switch(c){
            case '\\':
                state=stEscape;
                break;
            case '\n':
                buf[bufptr]=0;
                errMsg="String without right \"";
                return T_ERROR;
            case '"':
                buf[bufptr]=0;
                return T_STRING;
            default:
                buf[bufptr++]=c;
                break;
            }
            break;
        case stEscape:
            state=stString;
            switch(c){
            case 'n':
                buf[bufptr++]='\n';
                break;
            case 'r':
                buf[bufptr++]='\r';
                break;
            case 't':
                buf[bufptr++]='\t';
                break;
            case 'b':
                buf[bufptr++]='\b';
                break;
            case 'f':
                buf[bufptr++]='\f';
                break;
/*          case '0':
                state=stEscape0;
                break;*/
            case '\n':
                buf[bufptr]=0;
                return T_ERROR;
            default:
                if(isdigit(c)){ //oct not supported
                    buf[bufptr]=0;
                    errMsg="Oct escape sequence in string not supported";
                    return T_ERROR;
                }
                else
                    buf[bufptr++]=c;
                break;
            }
            break;
        default:
            assert(0);
        }//end switch state
    }//end while
    return 0;
}


























