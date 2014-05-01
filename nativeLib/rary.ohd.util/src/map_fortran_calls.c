/* map fortran calls to c functions */

#define create_map(c_function_name,arguments_with_type,arguments_without_type)\
	void c_function_name ##_ arguments_with_type  { c_function_name arguments_without_type; }

create_map(putenv,(char * string),(string))
create_map(system,(char * command),(command))
create_map(checksystem,(int * nflag),(nflag))
create_map(getcpu,(int * cpu_thsdth_sec),(cpu_thsdth_sec))
create_map(ddrmcl,(int *y1,int *m1,int *d1,int *h1,int *n1,int *s1),(y1,m1,d1,h1,n1,s1))
create_map(ofsclean,(int * stat),(stat))
create_map(getpid2,(int * pid),(pid))
create_map(gethostid2,(long int * hostid),(hostid))
create_map(utim,(long int* clk_thsdth, long int * usr_thsdth, long int * sys_thsdth),(clk_thsdth, usr_thsdth, sys_thsdth))
create_map(lockset,(char * in_name,int * in_len, char * type),(in_name,in_len, type))
create_map(upfsiz,(char * a,int * b, int * c),(a,b,c))
create_map(enterrtn,(const char* a, const char * b),(a,b))
create_map(initmsgc,(int * a, char * b, char * c, char * d, int * e),(a,b,c,d,e))
create_map(leavertn,(const char * a),(a))
create_map(writemsgc,(const int * a, const char * b),(a,b))
create_map(lockfree,(char * a,int * b),(a,b))
create_map(shcurd,(int * a, int * b, int *c),(a,b,c))


