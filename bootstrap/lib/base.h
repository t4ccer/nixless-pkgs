// base lib

int main(int argc, char **argv, char **envp);

#define NULL ((void *)0)

void _start(void) {
  long int argc;
  char **argv;
  char **envp;

  // x86_64 linux
  asm volatile("mov %%rdi, %0" : "=r"(argc));
  asm volatile("mov %%rsi, %0" : "=r"(argv));
  asm volatile("mov %%rdx, %0" : "=r"(envp));

  main(argc, argv, envp);
}

void memset(void *s, int c, unsigned long n) {
  char *p = s;
  while (n--) {
    *p++ = c;
  }
}

int h_strlen(const char *s) {
  int len = 0;
  while (*s++) {
    len++;
  }
  return len;
}

void h_strcpy(char *dst, const char *src) {
  while ((*dst++ = *src++)) {
  }
}

char *h_strcat(char *dst, const char *src) {
  while (*dst) {
    dst++;
  }
  h_strcpy(dst, src);
  return dst;
}

char *h_getenv(const char *name, char **envp) {
  for (; *envp; envp++) {
    char *env = *envp;
    int i = 0;
    for (; env[i] && env[i] != '='; i++) {
      if (env[i] != name[i]) {
        break;
      }
    }
    if (env[i] == '=' && name[i] == '\0') {
      return env + i + 1;
    }
  }

  return 0;
}

// x86_64 linux syscalls

int sys_mkdir(const char *pathname, int mode) {
  int res;
  asm volatile("syscall"
               : "=a"(res)
               : "0"(83), "D"(pathname), "S"(mode)
                : "rcx", "r11", "memory");
  return res;

}

int sys_rmdir(const char *pathname) {
  int res;
  asm volatile("syscall"
               : "=a"(res)
               : "0"(84), "D"(pathname)
                : "rcx", "r11", "memory");
  return res;
}

#define O_CREAT  00100
#define O_WRONLY 00001
#define O_TRUNC  01000

int sys_open(const char *pathname, int flags, int mode) {
  int res;
  asm volatile("syscall"
               : "=a"(res)
               : "0"(2), "D"(pathname), "S"(flags), "d"(mode)
               : "rcx", "r11", "memory");
  return res;
}

int sys_write(int fd, const char *buf, int count) {
  int res;
  asm volatile("syscall"
               : "=a"(res)
               : "0"(1), "D"(fd), "S"(buf), "d"(count)
               : "rcx", "r11", "memory");
  return res;
}

#define sys_write_str(fd, str) sys_write((fd), (str), (sizeof(str) - 1))

int sys_fork() {
  int res;
  asm volatile("syscall"
               : "=a"(res)
               : "0"(57)
               : "rcx", "r11", "memory");
  return res;
}

void sys_exit(int status) {
  asm volatile("syscall"
               :
               : "a"(60), "D"(status)
               : "rcx", "r11", "memory");
}

int sys_execve(const char *filename, const char **argv, const char **envp) {
  int res;
  asm volatile("syscall"
               : "=a"(res)
               : "0"(59), "D"(filename), "S"(argv), "d"(envp)
               : "rcx", "r11", "memory");
  return res;
}

int sys_wait4(int pid, int *status, int options, void *rusage) {
  int res;
  asm volatile("syscall"
               : "=a"(res)
               : "0"(61), "D"(pid), "S"(status), "d"(options), "r"(rusage)
               : "rcx", "r11", "memory");
  return res;
}

struct h_timespec {
  long int tv_sec;
  long int tv_nsec;
};

struct h_stat {
  unsigned long int st_dev;
  unsigned long int st_ino;
  unsigned long int st_nlink;
  unsigned int st_mode;
  unsigned int st_uid;
  unsigned int st_gid;
  int pad0;
  unsigned long int st_rdev;
  long int st_size;
  long int st_blksize;
  long int st_blocks;
  struct h_timespec st_atim;
  struct h_timespec st_mtim;
  struct h_timespec st_ctim;
};

int sys_stat(const char *pathname, struct h_stat *statbuf) {
  int res;
  asm volatile("syscall"
               : "=a"(res)
               : "0"(4), "D"(pathname), "S"(statbuf)
               : "rcx", "r11", "memory");
  return res;
}

void create_nested_dir(const char *path) {
  char buf[256];
  char *p = buf;
  h_strcpy(p, path);
  p += h_strlen(path);
  *p++ = '/';
  *p = '\0';

  for (int i = 0; buf[i]; i++) {
    if (buf[i] == '/') {
      buf[i] = '\0';
      sys_mkdir(buf, 0755);
      buf[i] = '/';
    }
  }
}

#define STDOUT 1
#define STDERR 2

void create_nested_dir_for_file(const char *path) {
  create_nested_dir(path);
  sys_rmdir(path);
}

#define _quote(v) #v
#define assert(v) \
	while (!(v)) { \
		sys_write_str(STDERR, "Assertion "); \
		sys_write_str(STDERR, #v); sys_write_str(STDERR, " failed at "); \
		sys_write_str(STDERR, __FILE__); sys_write_str(STDERR, ":"); \
		sys_write_str(STDERR, __func__); sys_write_str(STDERR, ":"); \
		sys_write_str(STDERR, _quote(__LINE__)); sys_write_str(STDERR, "!\n"); \
		sys_exit(134);  \
	}

char *lookup_hex_digit(int digit) {
  return "0123456789abcdef" + digit;
}

int run_command(const char *cmd, const char **args, const char **env) {
  int pid;
  int status;
  int termsig;

  sys_write(STDOUT, "Running: ", 9);
  sys_write(STDOUT, cmd, h_strlen(cmd));
  sys_write(STDOUT, " ", 1);
  for (int i = 0; args[i]; i++) {
    sys_write(STDOUT, args[i], h_strlen(args[i]));
    sys_write(STDOUT, " ", 1);
  }
  sys_write(STDOUT, "\n", 1);

  if ((pid = sys_fork())) {
    assert(sys_wait4(pid, &status, 0, NULL) == pid);
    termsig = status & 0x7f;
    if (termsig) {
      sys_exit(termsig);
    } else {
      status = (status & 0xff00) >> 8;
      sys_write(STDOUT, "Exit status: ", 13);
      sys_write(STDOUT, "0x", 2);
      sys_write(STDOUT, lookup_hex_digit((status & 0xf0) >> 4), 1);
      sys_write(STDOUT, lookup_hex_digit(status & 0xf), 1);
      sys_write(STDOUT, "\n", 1);
      return status;
    }
  } else {
    sys_exit(sys_execve(cmd, args, env));
  }

  return 42; // unreachable
}


#define CONCAT_INNER(a, b) a ## b
#define CONCAT(a, b) CONCAT_INNER(a, b)
#define make_var(n) CONCAT(n##_unique_, __COUNTER__)

#define tcc_freestanding(var, out, input1, rest...) do {                                              \
 char *cmd__ = NIXLESS_PKGS_TCC_PATH;                                                                 \
 char *args__[] = {"-nostdinc", "-nostdlib", "-I"NIXLESS_PKGS_TCC_LIB, "-o", out, input1, ##rest, 0}; \
 char *env__[] = {0};                                                                                 \
 var = run_command(cmd__, args__, env__);                                                             \
    } while(0)
