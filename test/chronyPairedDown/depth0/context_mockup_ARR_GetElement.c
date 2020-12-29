/* abstracting the context of the call to
   get_record in client_log.c  
*/

typedef long unsigned int size_t;
typedef struct ARR_Instance_Record * ARR_Instance;
struct ARR_Instance_Record {
    void * data;
    unsigned int elem_size;
    unsigned int used;
    unsigned int allocated;
};
void * ARR_GetElement(ARR_Instance array, unsigned int index)
{
    if (index < array->used)
    {
        ;
    }
    else
    {
        __VERIFIER_error();
    }
    return (void *) ((char *) array->data + (size_t) index * array->elem_size);
}

int slots = __VERIFIER_nondet_int();
__VERIFIER_assume(slots > 0 && slots < 15);

void get_record_mock(ARR_Instance array) {
  int cond1 = __VERIFIER_nondet_int();
  if (!cond1) {
    __VERIFIER_error();
  }
  int first;
  while(1) {
    first = __VERIFIER_nondet_uint() % slots;

    for (int i=0; i < 10; i++) {
      record = ARR_GetElement(records, first+1);

      int cond2 = __VERIFIER_nondet_int();
      if (cond2) {
        break;
      }	
    }
  }
}
ARR_Instance __alpaca_param_array;
unsigned int __alpaca_param_index;
int main()
{
    struct ARR_Instance_Record __alpaca_ref___alpaca_param_array;
    void *__alpaca_struct_mem_data;
    unsigned int __alpaca_struct_mem_elem_size;
    unsigned int __alpaca_struct_mem_used;
    unsigned int __alpaca_struct_mem_allocated;
    __alpaca_struct_mem_data = __VERIFIER_nondet_pointer();
    __alpaca_struct_mem_elem_size = __VERIFIER_nondet_uint();
    __alpaca_struct_mem_used = __VERIFIER_nondet_uint();
    __alpaca_struct_mem_allocated = __VERIFIER_nondet_uint();
    __alpaca_ref___alpaca_param_array = (struct ARR_Instance_Record){__alpaca_struct_mem_data,__alpaca_struct_mem_elem_size,__alpaca_struct_mem_used,__alpaca_struct_mem_allocated};
    __alpaca_param_array= &__alpaca_ref___alpaca_param_array;
    //    __alpaca_param_index = __VERIFIER_nondet_uint();
    //    ARR_GetElement(__alpaca_param_array, __alpaca_param_index);
    get_record_mock(__alpaca_param_array);
}
