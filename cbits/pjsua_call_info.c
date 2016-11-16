#include <pjsua-lib/pjsua.h>

pjsua_call_info* create_pjsua_call_info()
{
    return malloc(sizeof(pjsua_call_info));
}

pjsua_acc_id get_account_id(pjsua_call_info *call_info)
{
    return call_info->acc_id;
}

pjsip_inv_state get_call_state(pjsua_call_info *call_info)
{
    return call_info->state;
}
