#include <pjsua-lib/pjsua.h>

int get_tsx_type(pjsip_event *event)
{
    return event->body.tsx_state.type;
}

pjsip_rx_data* get_tsx_rx_data(pjsip_event *event)
{
    return event->body.tsx_state.src.rdata;
}

pjsip_msg* get_msg_from_event(pjsip_event *event)
{
    return event->body.rx_msg.rdata->msg_info.msg;
}

