#include <pjsua-lib/pjsua.h>

pjsua_call_info* create_pjsua_call_info()
{
    return malloc(sizeof(pjsua_call_info));
}

pjsua_acc_id getAccountId(pjsua_call_info *callInfo)
{
    return callInfo->acc_id;
}

pjsip_inv_state getCallState(pjsua_call_info *callInfo)
{
    return callInfo->state;
}
