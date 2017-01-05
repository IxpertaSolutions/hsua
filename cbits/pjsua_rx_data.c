#include <pjsua-lib/pjsua.h>

pjsip_msg* get_rx_msg(pjsip_rx_data *rx)
{
    return rx->msg_info.msg;
}

