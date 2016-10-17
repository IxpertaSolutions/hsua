#include <pjsua-lib/pjsua.h>

pjsua_logging_config* create_pjsua_logging_config()
{
    return malloc(sizeof(pjsua_logging_config));
}

void pjsua_logging_set_console_level(pjsua_logging_config* cfg, unsigned int level)
{
    cfg->console_level = level;
}
