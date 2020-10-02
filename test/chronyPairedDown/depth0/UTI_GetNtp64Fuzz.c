void arc4random_buf(char * buf, unsigned int len)
{
    for (int i = 0; i < len; i++)
    {
        buf[i] = __VERIFIER_nondet_char();
    }
}
typedef unsigned int __uint32_t;
typedef __uint32_t uint32_t;
typedef struct __llama_struct_id_3 {
            uint32_t hi; uint32_t lo;
        } NTP_int64;
void UTI_GetNtp64Fuzz(NTP_int64 * ts, int precision)
{
    int start, bits;
    if (precision >= -32 && precision <= 32)
    {
        ;
    }
    else
    {
        __VERIFIER_error();
    }
    if (sizeof(*ts) == 8)
    {
        ;
    }
    else
    {
        __VERIFIER_error();
    }
    start = sizeof(*ts) - (precision + 32 + 7) / 8;
    ts->hi = ts->lo = 0;
    UTI_GetRandomBytes((unsigned char *) ts + start,
                       sizeof(*ts) - start);
    bits = (precision + 32) % 8;
    if (bits)
    {
        ((unsigned char *) ts)[start] %= 1u << bits;
    }
}
void UTI_GetRandomBytes(void * buf, unsigned int len)
{
    arc4random_buf(buf, len);
}
NTP_int64 * __alpaca_param_ts;
int __alpaca_param_precision;
int main()
{
    __alpaca_param_precision = __VERIFIER_nondet_int();
    struct __llama_struct_id_3 __alpaca_ref___alpaca_param_ts;
    unsigned int __alpaca_struct_mem_hi;
    unsigned int __alpaca_struct_mem_lo;
    __alpaca_struct_mem_hi = __VERIFIER_nondet_uint();
    __alpaca_struct_mem_lo = __VERIFIER_nondet_uint();
    __alpaca_ref___alpaca_param_ts = (struct __llama_struct_id_3){__alpaca_struct_mem_hi,__alpaca_struct_mem_lo};
    __alpaca_param_ts= &__alpaca_ref___alpaca_param_ts;
    UTI_GetNtp64Fuzz(__alpaca_param_ts, __alpaca_param_precision);
}
