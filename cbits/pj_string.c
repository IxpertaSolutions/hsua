#include <pjsua-lib/pjsua.h>
#include <string.h>

pj_str_t* create_pj_str(char *str)
{
    pj_str_t *p_str = (pj_str_t*) malloc(sizeof(pj_str_t));
    return pj_strset(p_str, strdup(str), strlen(str));
}

void delete_pj_str(pj_str_t *p_str)
{
    free(p_str->ptr);
    free(p_str);
}
