/* BEGIN debugbreak.c */

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0501
#endif

#if _WIN32_WINNT < 0x0501
#error Must target Windows NT 5.0.1 or later for DebugBreakProcess
#endif

/* debugbreak.c:95:9: note: include '<stdio.h>' or provide a declaration of 'printf' */
#include <stdio.h>

#include <Windows.h>
#include <stddef.h>
#include <stdlib.h>

/* Compile with this line:

gcc -o debugbreak -mno-cygwin -mthreads debugbreak.c

If you have ever tried to interrupt a program running under cygwin gdb, 
you have probably experienced some frustration. 
Especially if the program was built with -mno-cygwin.

Here is a workaround.

You've probably discovered that pressing Ctrl-C in the gdb window/ prompt 
while the program being debugged is running is a fruitless exercise. 
Build the small debugbreak utility (source included in this message below). 
Open a new cygwin command line prompt. 
Use ps - W to find the WINPID of the process being debugged and 
then execute debugbreak with that WINPID. 
gdb will then regain control and the program being debugged 
will break just as though Ctrl-C was working properly. 

Be sure to use the WINPID of the process being debugged 
rather than the WINPID of gdb.

Program text follows below. 
Note that this workaround is also useful for 
GUI front ends for the gdb debugger (such as ddd) 
that suffer from this same problem.

Kyle


FROM
Workaround for GDB Ctrl-C Interrupt
Posted May 21st, 2010 by Dan Osborne
http://www.mingw.org/wiki/Workaround_for_GDB_Ctrl_C_Interrupt
AND (ORIGINALLY)
GDB Ctrl-C Interrupt Fails WORKAROUND
https://cygwin.com/ml/cygwin/2006-06/msg00321.html


I ACTUALLY DID
AnonymousUser@ANONYMOUST MINGW64 ~
gcc -o debugbreak -mthreads debugbreak.c

PRODUCED OUTPUT
debugbreak.exe

*/

static char errbuffer[256];

static const char *geterrstr(DWORD errcode)
{
size_t skip = 0;
DWORD chars;
chars = FormatMessage(
FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
NULL, errcode, 0, errbuffer, sizeof(errbuffer)-1, 0);
errbuffer[sizeof(errbuffer)-1] = 0;
if (chars) {
while (errbuffer[chars-1] == '\r' || errbuffer[chars-1] == '\n') {
errbuffer[--chars] = 0;
}
}
if (chars && errbuffer[chars-1] == '.') errbuffer[--chars] = 0;
if (chars >= 2 && errbuffer[0] == '%' && errbuffer[1] >= '0'
&& errbuffer[1] <= '9')
{
skip = 2;
while (chars > skip && errbuffer[skip] == ' ') ++skip;
if (chars >= skip+2 && errbuffer[skip] == 'i'
&& errbuffer[skip+1] == 's')
{
skip += 2;
while (chars > skip && errbuffer[skip] == ' ') ++skip;
}
}
if (chars > skip && errbuffer[skip] >= 'A' && errbuffer[skip] <= 'Z') {
errbuffer[skip] += 'a' - 'A';
}
return errbuffer+skip;
}

int main(int argc, char *argv[])
{
    HANDLE proc;
    unsigned proc_id = 0;
    BOOL break_result;

    if (argc != 2) {
        printf("Usage: debugbreak process_id_number\n");
        return 1;
    }
    proc_id = (unsigned) strtol(argv[1], NULL, 0);
    if (proc_id == 0) {
        printf("Invalid process id %u\n", proc_id);
        return 1;
    }
    proc = OpenProcess(PROCESS_ALL_ACCESS, FALSE, (DWORD)proc_id);
    if (proc == NULL) {
        DWORD lastError = GetLastError();
        printf("Failed to open process %u\n", proc_id);
        printf("Error code is %lu (%s)\n", (unsigned long)lastError,
            geterrstr(lastError));
        return 1;
    }
    break_result = DebugBreakProcess(proc);
    if (!break_result) {
        DWORD lastError = GetLastError();
        printf("Failed to debug break process %u\n", proc_id);
        printf("Error code is %lu (%s)\n", (unsigned long)lastError,
            geterrstr(lastError));
        CloseHandle(proc);
        return 1;
    }
    printf("DebugBreak sent successfully to process id %u\n", proc_id);
    CloseHandle(proc);
    return 0;
}

/* END debugbreak.c */
