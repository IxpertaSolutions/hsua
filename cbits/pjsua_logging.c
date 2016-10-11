#include <pjsua-lib/pjsua.h>

pjsua_logging_config* create_pjsua_logging_config()
{
    return malloc(sizeof(pjsua_logging_config));
}

