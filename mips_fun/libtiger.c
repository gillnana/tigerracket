
#include "libtiger.h"

#include </course/cs195r/lib/mips-rt/mips-rt.h>

/*
This library assumes some things about the representation of Tiger data types.
Integers are assumed to be
Strings must be represented as Tiger arrays of integers, where each
integer fits into a C unsigned char.
Arrays must store their length as the first word.
 */


void out_of_memory_fail() {
  // ALWAYS change len if change msg!!
  char* msg = "Error: out of memory\n"; 
  int len = 21;
  write(2, msg, len);
  abort();
}

array_t* alloc_array(int num_elem) {
  array_t* ans = malloc(sizeof(array_t) + num_elem*sizeof(int));
  if (ans) {
    return ans;
  } else {
    out_of_memory_fail();
  }
}

string_t* alloc_string(int num_elem) {
  alloc_array(num_elem);
}

extern void lt_print(string_t* str) {
  int len = str->length;
  int i=0;
  for (i=0; i<len; i++) {
    // print the char
    unsigned char c = str->values[i];
    // TODO: 1st arg: int fd   is 1 stdout?
    write(1, &c, 1);
  }
}

extern void lt_print_int(int n) {
  char buffer[10];
  int place = 1;
  int mag = n;
  if (mag < 0) {
    mag = -mag;
  }
  while (place <= mag) {
    place = place * 10;
  }
  // place is now the smallest power of 10 greater than mag
  // the 1 is in the first *unused* place value of mag
  if (mag) place = place / 10;
  // place is now the value of the highest-order place value
  int i=0;
  if (n < 0) {
    buffer[i] = '-';
    i++;
  }
  do {
    buffer[i] = mag/place + '0';
    mag = mag%place;
    place = place / 10;
    i++;
  } while (place);
  // i is now the length of the string written to the buffer
  write(1, buffer, i);
}

extern string_t* lt_getchar() {
  // TODO: is there a failure case for read(...) ?
  string_t* ans = alloc_string(1);
  unsigned char c;
  read(0, &c, 1);
  ans->values[0] = c;
  return ans;
}

extern int lt_ord(string_t* str) {
  if (str->length) {
    return str->values[0];
  } else {
    return -1;
  }
}

extern string_t* lt_chr(int n) {
  string_t* ans = alloc_string(1);
  ans->values[0] = n;
}

extern int lt_size(string_t* str) {
  return str->length;
}

extern string_t* lt_substring(string_t* str, int first, int n) {
  string_t* ans = alloc_string(n);
  int i;
  for (i=0; i<n; i++) {
    ans->values[i] = str->values[n+i];
  }
  return ans;
}

extern string_t* lt_concat(string_t* s1, string_t* s2) {
  int len1 = s1->length;
  int len2 = s2->length;
  int newlen = len1 + len2;
  string_t* ans = alloc_string(newlen);
  int i;
  for (i=0; i<len1; i++) {
    ans->values[i] = s1->values[i];
  }
  for (i=0; i<len2; i++) {
    ans->values[i+len1] = s2->values[i];
  }
  return ans;
}

extern int lt_not(int b) {
  if (b) {
    return 0;
  } else {
    return 1;
  }
}

extern void lt_exit(int code) {
  exit(code);
}

extern void lt_flush() {
  // lol who cares about buffered IO?
  // especially considering our garbage collection strategy
}
