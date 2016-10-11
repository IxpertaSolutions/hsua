#include <pjsua-lib/pjsua.h>

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

void setAccountSchema(pjsua_acc_config *account, unsigned i, pj_str_t *p_str)
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

