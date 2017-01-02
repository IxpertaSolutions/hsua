#include <pjsua-lib/pjsua.h>

pjsip_hdr* get_msg_next_hdr(pjsip_hdr *hdr)
{
    return hdr->next;
}

pjsip_hdr* get_msg_hdr(pjsip_msg *msg)
{
    return &msg->hdr;
}

