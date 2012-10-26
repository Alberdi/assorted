/*
 * Copyright 2008 Marcelino Alberdi <marcelino.alberdi@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/wait.h>
#include <time.h>

int main(int argc, char* argv[]) {
  int i, pid, bytes, status = EXIT_FAILURE;
  FILE* fp;

  srand(time(0));

  if (argc != 4) {
    printf("Usage: progrand path/to/bin output_file bytes\n");
    exit(EXIT_FAILURE);
  }

  bytes = strtol(argv[3], NULL, 10);
  char s[bytes+1];
  s[bytes] = '\0';
  argv[3] = NULL;

  while(status != EXIT_SUCCESS) {
    if ((fp = fopen(argv[2], "w+")) == NULL) {
      perror("Cannot open file");
      exit(EXIT_FAILURE);
    }

    for(i=0; i<bytes; i++)
      s[i] = (int)(((double)rand()/((double)(RAND_MAX)+(double)(1)))*95) + 32;

    fprintf(fp, "%s\n",s);
    fclose(fp);

    if ((pid = vfork()) < 0) {
      perror("Cannot create fork");
      exit(EXIT_FAILURE);
    }

    if (pid == 0) {
      execv(argv[1], &argv[1]);
      exit(EXIT_FAILURE);
    }

    waitpid(pid, &status, WNOHANG|WUNTRACED|WCONTINUED);
  }

  printf("Last candidate exited with status %i\n", status);
  return EXIT_SUCCESS;
} 
