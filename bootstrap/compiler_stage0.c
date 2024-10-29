#include "base.h"

char scratch[4096] = {0};

int main(int argc, char **argv, char **envp) {
  char *dest_dir = h_getenv("out", envp);

  h_strcat(scratch, dest_dir);
  h_strcat(scratch, "/");
  h_strcat(scratch, NIXLESS_PKGS_OUT_PATH);
  create_nested_dir_for_file(scratch);

  int comp_res;
  tcc_freestanding(comp_res, scratch, NIXLESS_PKGS_SRCS);
  assert(comp_res == 0);

  sys_exit(0);
}
