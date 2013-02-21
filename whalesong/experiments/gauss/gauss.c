#include <stdio.h>
#include <sys/time.h>

int main(int argc, char**argv) {
  unsigned long long i, n, acc=0;
  struct timeval start, end;
  sscanf(argv[1], "%llu", &n);


  gettimeofday(&start, NULL);
  for (i = 0; i <= n; i++) {
    acc = acc + i;
  }
  gettimeofday(&end, NULL);
  printf("%llu (%f milliseconds)\n", 
	 acc,
	 (1000.0*(end.tv_sec - start.tv_sec) + 
	  ((end.tv_usec - start.tv_usec) / 1000.0) ));
}
