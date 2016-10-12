#include <pjsua-lib/pjsua.h>

pjsua_config* create_pj_config()
{
    return malloc(sizeof(pjsua_config));
}

typedef void (*on_call_state_t)(pjsua_call_id call_id, pjsip_event *e);

void pjsua_set_on_call_state(pjsua_config *cfg, on_call_state_t call_back)
{
    cfg->cb.on_call_state = call_back;
}

typedef void (*on_incoming_call_t)
    (pjsua_acc_id acc_id, pjsua_call_id call_id, pjsip_rx_data *rdata);

void pjsua_set_on_incoming_call(pjsua_config *cfg, on_incoming_call_t call_back)
{
    cfg->cb.on_incoming_call = call_back;
}

typedef void(*on_reg_state_t)(pjsua_acc_id acc_id);

void pjsua_set_on_reg_state(pjsua_config *cfg, on_reg_state_t call_back)
{
    cfg->cb.on_reg_state = call_back;
}

typedef void(*on_reg_started_t)(pjsua_acc_id acc_id, pj_bool_t renew);

void pjsua_set_on_reg_started(pjsua_config *cfg, on_reg_started_t call_back)
{
    cfg->cb.on_reg_started = call_back;
}

typedef void(*on_call_media_state_t) (pjsua_call_id call_id);

void pjsua_set_on_media_state(pjsua_config *cfg, on_call_media_state_t call_back)
{
    cfg->cb.on_call_media_state = call_back;
}

