#include <pjsua-lib/pjsua.h>

pjsua_transport_config* create_pjsua_transport_config()
{
    return malloc(sizeof(pjsua_transport_config));
}

void pjsua_transport_config_set_port(pjsua_transport_config *cfg, unsigned port)
{
    cfg->port = port;
}

