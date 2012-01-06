#ifndef LOG_H

#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

void log_clear();
void log_vadd(
    const char *const   fmt,
    va_list		args);
void log_start(
    const char* const	fmt,
    ...);
void log_errno(void);
void log_add(
    const char *const   fmt,
    ...);
void log_log(
    const int		level);

#ifdef __cplusplus
}
#endif

#endif
