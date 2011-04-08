#ifndef LIBTIGER_H
#define LIBTIGER_H

// struct array is a tiger array
typedef struct array {
  int length;
  int values[];
} array_t;

// for now, a tiger string is a tiger array
//   where each character is its own word-sized element of the array
typedef array_t string_t;


extern void lt_flush();
extern void lt_print(string_t* str);
extern void lt_print_int(int n);
extern array_t* lt_getchar();
extern int lt_ord(string_t* str);
extern string_t* lt_chr(int n);
extern int lt_size(string_t* str);
extern string_t* lt_substring(string_t* str, int first, int n);
extern string_t* lt_concat(string_t* s1, string_t* s2);
extern int lt_not(int b);
extern void lt_exit(int code);

#endif
