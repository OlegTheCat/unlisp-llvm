#include <stdlib.h>
#include <stdarg.h>

union UntaggedObject {
  long long i;
  void *list;
  void *symbol;
  void *function;
};

enum ObjType { Int64 = 1, List = 2, Symbol = 3, Function = 4 };

struct Object {
  enum ObjType ty;
  union UntaggedObject obj;
};

extern struct Object *va_list_to_obj_array(unsigned long long n, va_list list) {
  struct Object *arr = malloc(n * sizeof(struct Object));
  unsigned long long i = 0;

  for (i = 0; i < n; i++) {
    arr[i] = va_arg(list, struct Object);
  }

  return arr;
}
