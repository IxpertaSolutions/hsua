#include <pjsua-lib/pjsua.h>

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
