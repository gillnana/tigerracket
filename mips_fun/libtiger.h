#ifndef LIBTIGER_H
#define LIBTIGER_H

// struct array is a tiger array
typedef struct array {
  int length;
  int values[];
} array_t;

typedef struct pair {
  void* left;
  void* right;
} pair_t;

// for now, a tiger string is a tiger array
//   where each character is its own word-sized element of the array
typedef array_t string_t;

extern int* alloc_block(int num_words, int init_val);

extern array_t* alloc_array(int num_elem, int init_val);
extern string_t* alloc_string(int num_elem);

extern pair_t* alloc_closure(void* code, void* ar);

extern void assert_nonnil(void* thing);

extern void assert_inbounds(array_t* arr_ptr, void* elem_ptr);

extern int string_comp(string_t* a, string_t* b);


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

extern void call_test(int a0, int a1, int a2, int a3, int a4, int a5);

#endif
