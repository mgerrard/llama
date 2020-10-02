typedef struct ARR_Instance_Record * ARR_Instance;
struct ARR_Instance_Record {
    void * data;
    unsigned int elem_size;
    unsigned int used;
    unsigned int allocated;
};
ARR_Instance ARR_CreateInstance(unsigned int elem_size)
{
    ARR_Instance array;
    if (elem_size > 0)
    {
        ;
    }
    else
    {
        __VERIFIER_error();
    }
    array = (struct ARR_Instance_Record *) Malloc(sizeof(struct ARR_Instance_Record));
    array->data = (void *) 0;
    array->elem_size = elem_size;
    array->used = 0;
    array->allocated = 0;
    return array;
}
unsigned int __alpaca_param_elem_size;
int main()
{
    __alpaca_param_elem_size = __VERIFIER_nondet_uint();
    ARR_CreateInstance(__alpaca_param_elem_size);
}
