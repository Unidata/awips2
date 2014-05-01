#ifndef SAVEDINFO_H
#define SAVEDINFO_H

int
savedInfo_set(
    const prod_info* const	info);

const prod_info*
savedInfo_get();

const char*
savedInfo_strerror(
    int				error);

int
savedInfo_wasSet(void);

void
savedInfo_reset(void);

#endif
