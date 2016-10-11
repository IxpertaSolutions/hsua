#include <pjsua-lib/pjsua.h>

pjsua_config* create_pj_config()
{
    return malloc(sizeof(pjsua_config));
}

pjsua_logging_config* create_pjsua_logging_config()
{
    return malloc(sizeof(pjsua_logging_config));
}

pjsua_media_config* create_pjsua_media_config()
{
    return malloc(sizeof(pjsua_media_config));
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

typedef void(*on_call_media_state_t) (pjsua_call_id call_id);

void pjsua_set_on_media_state(pjsua_config *cfg, on_call_media_state_t call_back)
{
    cfg->cb.on_call_media_state = call_back;
}

pjsua_transport_config* create_pjsua_transport_config()
{
    return malloc(sizeof(pjsua_transport_config));
}

void pjsua_transport_config_set_port(pjsua_transport_config *cfg, unsigned port)
{
    cfg->port = port;
}

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

void printDevices()
{
    int dev_count;
    pjmedia_aud_dev_index dev_idx;
    pj_status_t status;
    dev_count = pjmedia_aud_dev_count();
    printf("Got %d audio devices\n", dev_count);
    for (dev_idx=0; dev_idx<dev_count; ++dev_idx) 
    {
        pjmedia_aud_dev_info info;
        status = pjmedia_aud_dev_get_info(dev_idx, &info);
        printf("%d. %s (in=%d, out=%d)\n",
            dev_idx, info.name,
            info.input_count, info.output_count);
    }
}

