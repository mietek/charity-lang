//declarations for a command line CmdlineParser
//[-<1993.6.16
/*modified in 1993.9.16
  if switch char is '\0',all the argument are considered options as well
  as parameters.

  modified in 2002.3.15
  it is no longer  a static class
  changed class name to CmdlineCmdlineParser;
*/


#ifndef __CMDPARSE_H
#define __CMDPARSE_H

/*
   Concept:
    . switch char: a char being used as a flag,identify the string following
                   it to be an option.
                   the most commonly used switch char is '/' and '-',the
                   default value in CmdlineParser is '/',you can use setSwitchChar
                   to change it.
        --93.9.16--:if switch char is '\0',all the argument are considered
                    options as well as parameters,that is,no switch char
                    needed.
    . option     : a string in command line preleaded by an switch char.
    . option's argument:
                   the string following an option string in command line.
                   there should be no spaces or other chars between an
                   option and an option's argument.
    . parameter  : all the strings in command line except options.

   Description of member functions
    .CmdlineParser(int argc, char* argv[], char sw,const char*s)
       --constructor,set switch char and option format;
       --example: CmdlineParser Arg('/',"L DD&");
    .setSwitchChar(char CH)--use CH as the switch char
    .setOptions(const char*OPTIONS)
        --tell CmdlineParser what option strings are legal.
        the string OPTIONS contains all the legal options strings seperated by
        spaces,an option string ended with a '&' means the option can have
        argument.The first uppercase letter in an option string is the 'key'
        of the option,if no uppercase letter found,the 'key' is the first
        letter.
        Note: an legal option string shouldn't contain '&' and space
    .getOption()
        --get next option in command line,return the 'key' of the option.
        --if an illegal option encountered,return '&'.
        --if no more options in command line,return '\0'.
    .getValue()
        --if getOption encounter an option with argument,getValue will return
          the option's argument.
        --if getOption encounter an illegal option,getValue will return the
          whole string.
        --if getOption encounter an option without argument,getValue return
          the whole string
          Note: use getValue right after getOption.
    .getName()--get the program's path and name,equal to argv[0]
    .getParameter()
        --get next parameter in command line,argv[0] not included.
    .parameterCount()
        --return the number of parameters in command line.
    .reset()--make getOption() and getParameter() start from begining.
              switch char and legal options not changed.
              this is useful when you need to scan the command line for
              several times.
   Examples:
        CmdlineParser Arg;
        Arg.setSwitchChar('-');
        Arg.setOptions("L:& L& debUg  ?  U  batch");
        int ch;
        while((ch=Arg.getOption())!='\0')
               if(ch==-1) printf("-1    %s\n",Arg.getValue());
               else printf("%c     %s\n",ch,Arg.getValue());

   assume we have the following command line:
      Parse -L:50 -L60 -? -debug -U3 -batch
   then the result will look like:
       L    50
       L    60
       ?    -?
       U    -debug
       -1   -U3
       b    -batch
    --1993.9.16--: example
        CmdlineParser arg('\0'," On oFf Remove index:&");
    and we have the following command line:
        on dddd remove  index:34
    then
         getOption() getValue() getParameter():
            'O'       "on"       "on"
            '&'       "dddd"     "dddd"
            'R'       "remove"   "remove"
            'i'        "34"      "index:34"

*/
class CmdlineParser {
    private:
        int _argc;
        char **_argv;
        char switchchar;
        const char*format;
        const char*value;  //
        int curparam; //point current parameter
        int curoption;//point current option
    public:
        CmdlineParser(int argc, char*argv[], char sw,const char*s);
        void setSwitchChar(char c) { switchchar=c;}
        void setOptions(const char* s) { format=s;}
        char getOption();
        const char*getValue() { return value; } //current option value
        const char*getParameter();
        int parameterCount();
        void reset() { curparam=curoption=0; }
};


#endif






