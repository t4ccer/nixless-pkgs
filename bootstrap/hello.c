#include "base.h"

int main(int argc, char **argv, char **envp) {
  sys_write_str(STDOUT, "Hello from Haskell and tcc!\n");
  sys_exit(0);
}
