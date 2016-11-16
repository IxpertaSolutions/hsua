#include <pjsua-lib/pjsua.h>
#include <stdbool.h>

pjsua_acc_config* crete_pjsua_acc_config()
{
    return malloc(sizeof(pjsua_acc_config));
}

void set_account_id(pjsua_acc_config *account, pj_str_t *p_str)
{
    account->id = *p_str;
}

void set_account_reg_uri(pjsua_acc_config *account, pj_str_t *p_str)
{
    account->reg_uri = *p_str;
}

void set_account_cred_count(pjsua_acc_config *account, unsigned i)
{
    account->cred_count = i;
}

void set_account_realm(pjsua_acc_config *account, unsigned i, pj_str_t *p_str)
{
    account->cred_info[i].realm = *p_str;
}

void set_account_scheme(pjsua_acc_config *account, unsigned i, pj_str_t *p_str)
{
    account->cred_info[i].scheme = *p_str;
}

void set_account_username(pjsua_acc_config *account, unsigned i, pj_str_t *p_str)
{
    account->cred_info[i].username = *p_str;
}

void set_account_data_type(pjsua_acc_config *account, unsigned i, int val)
{
    account->cred_info[i].data_type = val;
}

void set_account_data(pjsua_acc_config *account, unsigned i, pj_str_t *p_str)
{
    account->cred_info[i].data = *p_str;
}

void set_account_register_on_add(pjsua_acc_config *account, pj_bool_t val)
{
    account->register_on_acc_add = val;
}

int is_account_registered(pjsua_acc_id id)
{
    pjsua_acc_info info;
    pjsua_acc_get_info(id, &info);
    return (info.status/100 == 2 && info.expires > 0);
}
