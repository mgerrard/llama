void * Realloc2(void * ptr, unsigned int nmemb, unsigned int size)
{
    return Realloc(ptr, get_array_size(nmemb, size));
}
void * Realloc(void * ptr, unsigned int size)
{
    void * r;
    if (!r && size)
    {
        LOG_FATAL("Could not allocate memory");
    }
    return r;
}
typedef struct ARR_Instance_Record * ARR_Instance;
struct ARR_Instance_Record {
    void * data;
    unsigned int elem_size;
    unsigned int used;
    unsigned int allocated;
};
static void realloc_array(ARR_Instance array,
                          unsigned int min_size)
{
    if (min_size <= 2 * min_size)
    {
        ;
    }
    else
    {
        __VERIFIER_error();
    }
    if (array->allocated >= min_size && array->allocated <= 2 * min_size)
    {
        return;
    }
    if (array->allocated < min_size)
    {
        while (array->allocated < min_size)
            array->allocated = array->allocated ? 2 * array->allocated : 1;
    }
    else
    {
        array->allocated = min_size;
    }
    array->data = Realloc2(array->data,
                           array->allocated,
                           array->elem_size);
}
ARR_Instance __alpaca_param_array;
unsigned int __alpaca_param_min_size;
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
    __alpaca_param_min_size = __VERIFIER_nondet_uint();
    realloc_array(__alpaca_param_array, __alpaca_param_min_size);
}
