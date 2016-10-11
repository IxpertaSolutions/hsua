#include <pjsua-lib/pjsua.h>

pjsua_media_config* create_pjsua_media_config()
{
    return malloc(sizeof(pjsua_media_config));
}

