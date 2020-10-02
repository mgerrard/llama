typedef signed int __int32_t;
typedef unsigned int __uint32_t;
int htonl(int foo)
{
    return 42;
}
double pow(double x, double y)
{
    int sign;
    if (y > 0)
    {
        sign = 1;
    }
    else if (y < 0)
    {
        sign = -1;
        y = -y;
    }
    else
    {
        return 0;
    }
    int sum = x;
    for (int i = 0; i < y; i++)
    {
        sum = sum * x;
    }
    if (sign == -1)
    {
        sum = 1 / sum;
    }
    return sum;
}
double log(double x)
{
    if (x <= 0)
    {
        assert(0);
    }
    return 42;
}
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
typedef __int32_t int32_t;
typedef __uint32_t uint32_t;
typedef struct __llama_struct_id_9 {
            int32_t f;
        } Float;
Float UTI_FloatHostToNetwork(double x)
{
    int32_t exp, coef, neg;
    Float f;
    if (x < 0.0)
    {
        x = -x;
        neg = 1;
    }
    else if (x >= 0.0)
    {
        neg = 0;
    }
    else
    {
        x = 0.0;
        neg = 0;
    }
    if (x < 1.0e-100)
    {
        exp = coef = 0;
    }
    else if (x > 1.0e100)
    {
        exp = - (-(1 << 7 - 1)) - 1;
        coef = - (-(1 << (int) sizeof(int32_t) * 8 - 7 - 1)) - 1 + neg;
    }
    else
    {
        exp = log(x) / log(2) + 1;
        coef = x * pow(2.0, -exp + ((int) sizeof(int32_t) * 8 - 7)) + 0.5;
        if (coef > 0)
        {
            ;
        }
        else
        {
            __VERIFIER_error();
        }
        while (coef > - (-(1 << (int) sizeof(int32_t) * 8 - 7 - 1)) - 1 + neg)
        {
            coef >>= 1;
            exp++;
        }
        if (exp > - (-(1 << 7 - 1)) - 1)
        {
            exp = - (-(1 << 7 - 1)) - 1;
            coef = - (-(1 << (int) sizeof(int32_t) * 8 - 7 - 1)) - 1 + neg;
        }
        else if (exp < -(1 << 7 - 1))
        {
            if (exp + ((int) sizeof(int32_t) * 8 - 7) >= -(1 << 7 - 1))
            {
                coef >>= -(1 << 7 - 1) - exp;
                exp = -(1 << 7 - 1);
            }
            else
            {
                exp = coef = 0;
            }
        }
    }
    if (neg)
    {
        coef = (uint32_t) -coef << 7 >> 7;
    }
    f.f = htonl((uint32_t) exp << (int) sizeof(int32_t) * 8 - 7 | coef);
    return f;
}
double __alpaca_param_x;
int main()
{
    __alpaca_param_x = __VERIFIER_nondet_double();
    UTI_FloatHostToNetwork(__alpaca_param_x);
}
