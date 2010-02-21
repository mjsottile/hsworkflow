#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>

void usage(char *av0) {
  printf("usage: %s -f infile -o outdir\n\n",av0);
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv) {
  char *filename;
  char *outdir;
  char outfname[BUFSIZ];
  char buf[BUFSIZ];
  FILE *fpIN, *fpOUT;
  int n,i;
  char ch;

  /* initialize to null. */
  filename = 0;
  outdir = 0;

  while ((ch = getopt(argc, argv, "f:o:")) != -1) {
    switch (ch) {
    case 'f':
      filename = malloc(sizeof(char)*(strlen(optarg)+1));
      strcpy(filename,optarg);
      break;
    case 'o':
      outdir = malloc(sizeof(char)*(strlen(optarg)+1));
      strcpy(outdir,optarg);
      break;
    default:
      usage(argv[0]);
    }
  }

  if (filename == 0 || outdir == 0) {
    usage(argv[0]);
  }

  fpIN = fopen(filename,"r");
  assert(fpIN != 0);

  sprintf(outfname,"%s/capitalized.out",outdir);
  fpOUT = fopen(outfname,"w");
  assert(fpOUT != 0);

  while (!feof(fpIN)) {
    n = fread(buf,BUFSIZ,sizeof(char),fpIN);
    n = strlen(buf);
    for (i=0;i<n;i++) {
      if (buf[i] >= 'a' && buf[i] <= 'z') {
	buf[i] += 'A'-'a';
      }
    }
    fwrite(buf,n,sizeof(char),fpOUT);
  }

  fclose(fpIN);
  fclose(fpOUT);

  free(filename);
  free(outdir);

  exit(EXIT_SUCCESS);
}
