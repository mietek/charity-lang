//implement external functions by windows API
#include <process.h>
#include <io.h>
#include <fcntl.h>
#include "vmc.h"

void my_pipe(VInt barg[], VPtr parg[])
{
	barg[0]=_pipe((int*)(parg[0]), 4096, O_BINARY);
}
void my_newpipe(VInt barg[], VPtr parg[])
{
	int buf[2];
	if(_pipe(buf,4096,O_BINARY)==-1){
		buf[0]=-1;
		buf[1]=-1;
	}
	barg[0]=buf[0];
	barg[1]=buf[1];
}
void my_spawn(VInt barg[], VPtr parg[])
{
	const char* prog=(const char*)parg[1];
	const char* param= (const char*) parg[0];
	int newstdin = barg[1];
	int newstdout= barg[0];
	
	int oldstdin=-1;
	int oldstdout=-1;
	int oldstderr=-1;

	//change standard file handle so that 
	//the new process can inherit them.
	if(newstdin!=-1){
		oldstdin=_dup(0);
		_dup2(newstdin, 0);
	}
	if(newstdout!=-1){
		oldstdout=_dup(1);
		oldstderr=_dup(2);
		_dup2(newstdout, 1);
		_dup2(newstdout, 2);
	}
	int pid=_spawnlp(_P_NOWAIT, prog, prog, param, 0);
	//restore standard file handle
	if(newstdin!=-1){
		_dup2(oldstdin,0);
		_close(oldstdin);
	}
	if(newstdout!=-1){
		_dup2(oldstdout,1);
		_dup2(oldstderr,2);
		_close(oldstdout);
		_close(oldstderr);
	}

	if(pid==-1)
		barg[0]=0;
	else
		barg[0]=pid;
	
/*	STARTUPINFO si;
	GetStartupInfo(&si);      //set startupinfo for the spawned process
	si.dwFlags = STARTF_USESTDHANDLES|STARTF_USESHOWWINDOW;
	si.wShowWindow = SW_HIDE;
	si.hStdOutput = newstdout;
	si.hStdError = newstdout;     //set the new handles for the child process
	si.hStdInput = newstdin;
	char cmdline[200];
	strcpy(cmdline, prog);
	strcat(cmdline, " ");
	strcat(cmdline, param);
	//spawn the child process
	if (!CreateProcess(prog,cmdline,NULL,NULL,TRUE,CREATE_NEW_CONSOLE,
			 NULL,NULL,&si,&pi))
		return 0;
	return -1;*/
}

void my_open(VInt barg[], VPtr parg[])
{
    barg[0]=open((char*)parg[0], barg[0]);
}
void my_close(VInt barg[], VPtr parg[])
{
    barg[0]=close(barg[0]);
}
void my_read(VInt barg[], VPtr parg[])
{
    //n=read(fd,buf,length)
    char* buf=(char*)parg[0];
    int fd=barg[1];
    int len=barg[0];
    barg[0]=read(fd,buf,len);
} 
void my_write(VInt barg[], VPtr parg[])
{
    //n=write(fd,buf,length)
    char* buf=(char*)parg[0];
    int fd=barg[1];
    int len=barg[0];
    barg[0]=write(fd,buf,len);
} 
void my_seek(VInt barg[], VPtr parg[])
{
    //n=seek(fd,pos)
    int fd=barg[1];
    int pos=barg[0];
    barg[0]=lseek(fd,pos,SEEK_SET);
}                 
            
void my_readchar(VInt barg[], VPtr parg[])
{
	char buf[2];
	int n=read(barg[0],buf,1);
	if(n==0)
		barg[0]=-1;
	else
		barg[0]=(unsigned char)buf[0];
}
void my_writechar(VInt barg[], VPtr parg[])
{
	char buf[2];
	buf[0]=(unsigned char)barg[0];
	barg[0]=write(barg[1],buf,1);
}/*
void my_eof(VInt barg[], VPtr parg[])
{
	barg[0]=eof(barg[0]);
}
*/