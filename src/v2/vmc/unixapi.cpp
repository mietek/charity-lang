#include "vmc.h"
#include <unistd.h>
#include <fcntl.h>

void my_pipe(VInt barg[], VPtr parg[])
{
	barg[0]=pipe((int*)(parg[0]));
}
void my_newpipe(VInt barg[], VPtr parg[])
{
	int buf[2];
	if(pipe(buf)==-1){
		buf[0]=-1;
		buf[1]=-1;
	}
	barg[0]=buf[0];
	barg[1]=buf[1];
}
void my_spawn(VInt barg[], VPtr parg[])
{
	//spawn(prog, arg, newstdin, newstdout)
	char * prog=(char*)parg[1];
	char * arg=(char*) parg[0];
	int newstdin=barg[1];
	int newstdout=barg[0];
	int pid=fork();
	switch(pid){
	case -1:
		barg[0]=0;
		return;
	case 0: //child
		//redirect stdin and stdout
		if(newstdin!=-1){
			dup2(newstdin,0);
			close(newstdin);
		}
		if(newstdout!=-1){
			dup2(newstdout,1);
			dup2(newstdout,2);
			close(newstdout);
		}
		execlp(prog, arg, 0);
		barg[0]=0;
		return;
	default: //parent
		break;
	}
	barg[0]=pid;
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
	char ch;
	int n=read(barg[0],&ch,1);
	if(n==0)
		barg[0]=-1;
	else
		barg[0]=(unsigned char)ch;
}
void my_writechar(VInt barg[], VPtr parg[])
{
	char buf[2];
	buf[0]=(unsigned char)barg[0];
	barg[0]=write(barg[1],buf,1);
}

