/*Specification for external function definition
  An external function must be in one of the following forms:
  VInt f(VInt barg[], VPtr parg[]);
  VLong f(VInt barg[], VPtr parg[]);
  VPtr  f(VInt barg[], VPtr parg[]);
To define a new external function f which have 2 basic argument
and 1 pointer argument, and return a basic value, add a line:
    extern VInt my_f(VInt barg[], VPtr parg[]);
then add an entry in _extFunSpec:
{ "f", v_callExtB, (void*)my_f, 2, 1}
then define the body of my_f in this file only,
or both unixapi.cpp and winapi.cpp.
*/
#include "vmc.h"
#define DEF_MY(f) extern void f(VInt barg[], VPtr parg[]);
typedef void EXT_FUN(VInt barg[], VPtr parg[]);
EXT_FUN
my_system,
my_pipe,
my_newpipe,
my_spawn,
my_open,
my_close,
my_read,
my_write,
my_seek,
my_readchar,
my_writechar
;
/*
extern VInt my_pipe(VInt barg[], VPtr parg[]);
extern VInt my_spawn(VInt barg[], VPtr parg[]);
extern VInt my_system(VInt barg[], VPtr parg[]);
extern VInt my_open(VInt barg[], VPtr parg[]);
extern VInt my_close(VInt barg[], VPtr parg[]);
extern VInt my_read(VInt barg[], VPtr parg[]);
extern VInt my_write(VInt barg[], VPtr parg[]);
extern VInt my_seek(VInt barg[], VPtr parg[]);
extern VInt my_readchar(VInt barg[], VPtr parg[]);
extern VInt my_writechar(VInt barg[], VPtr parg[]);
extern VInt my_eof(VInt barg[], VPtr parg[]);
*/
VExtFunSpec _extFunSpec[]=
{
    //name,     function, in bcnt, in pcnt, out bcnt, out pcnt
    { "system",   my_system,   0,1,  1,0 },
    { "pipe",     my_pipe,     0,1,  1,0 },
    { "newpipe",  my_newpipe,  0,0,  2,0 },
    { "spawn",    my_spawn,    2,2,  1,0 },
    { "open",     my_open,     1,1,  1,0 },
    { "close",    my_close,    1,0,  1,0 },
    { "read",     my_read,     2,1,  1,0 },
    { "write",    my_write,    2,1,  1,0 },
    { "seek",     my_seek,     2,0,  1,0 },
    { "readchar", my_readchar, 1,0,  1,0 },
    { "writechar",my_writechar,2,0,  1,0 },
//  { "eof",      my_eof,      1,0,  1,0 }, //no such function in unix
    { 0, 0, 0,0,0,0 }
/*
    //name       type          function     bcnt pcnt
//  {"sum",    v_callExtB, (void*)my_sum,    2,  0},
    {"pipe",   v_callExtB, (void*)my_pipe,   0,  1},
    {"spawn",  v_callExtB, (void*)my_spawn,  2,  2},
    {"system", v_callExtB, (void*)my_system, 0,  1},
    {"open",   v_callExtB, (void*)my_open,   1,  1},
    {"close",  v_callExtB, (void*)my_close,  1,  0},
    {"read",   v_callExtB, (void*)my_read,   2,  1},
    {"write",  v_callExtB, (void*)my_write,  2,  1},
    {"seek",   v_callExtB, (void*)my_seek,   2,  0},
    {"readchar", v_callExtB, (void*)my_readchar, 1, 0},
    {"writechar",v_callExtB, (void*)my_writechar,2,0},
    {"eof",     v_callExtB, (void*)my_eof, 1, 0 },
    {0, v_callExtB,0,0}
*/
};

void my_system(VInt barg[], VPtr parg[])
{
    barg[0]=system((char*)parg[0]);
}
