#include <pjsua-lib/pjsua.h>

pjsip_msg* get_msg_from_event(pjsip_event *event)
{
    return event->body.rx_msg.rdata->msg_info.msg;
}

int get_tsx_type(pjsip_event *event)
{
    return event->body.tsx_state.type;
}

pjsip_rx_data* get_tsx_rx_data(pjsip_event *event)
{
    return event->body.tsx_state.src.rdata;
}

pjsip_msg* get_rx_msg(pjsip_rx_data *rx)
{
    return rx->msg_info.msg;
}

void print_devices()
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
