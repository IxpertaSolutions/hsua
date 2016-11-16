#include <pjsua-lib/pjsua.h>
#include <stdbool.h>

pjsua_acc_config* crete_pjsua_acc_config()
{
    return malloc(sizeof(pjsua_acc_config));
}

void setAccountId(pjsua_acc_config *account, pj_str_t *p_str)
{
    account->id = *p_str;
}

void setAccountRegUri(pjsua_acc_config *account, pj_str_t *p_str)
{
    account->reg_uri = *p_str;
}

void setAccountCredCount(pjsua_acc_config *account, unsigned i)
{
    account->cred_count = i;
}

void setAccountRealm(pjsua_acc_config *account, unsigned i, pj_str_t *p_str)
{
    account->cred_info[i].realm = *p_str;
}

void setAccountScheme(pjsua_acc_config *account, unsigned i, pj_str_t *p_str)
{
    account->cred_info[i].scheme = *p_str;
}

void setAccountUsername(pjsua_acc_config *account, unsigned i, pj_str_t *p_str)
{
    account->cred_info[i].username = *p_str;
}

void setAccountDataType(pjsua_acc_config *account, unsigned i, int val)
{
    account->cred_info[i].data_type = val;
}

void setAccountData(pjsua_acc_config *account, unsigned i, pj_str_t *p_str)
{
    account->cred_info[i].data = *p_str;
}

void setAccountRegisterOnAdd(pjsua_acc_config *account, pj_bool_t val)
{
    account->register_on_acc_add = val;
}

int is_account_registered(pjsua_acc_id id)
{
    pjsua_acc_info info;
    pjsua_acc_get_info(id, &info);
    return (info.status/100 == 2 && info.expires > 0);
}
