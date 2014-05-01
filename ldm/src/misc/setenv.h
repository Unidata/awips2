#ifndef LDM_SETENV_H
#define LDM_SETENV_H

#ifdef NO_SETENV
extern int
setenv(const char *name, const char *value, int rewrite);
#else /* NO_SETENV */
void
_nada_setenv(void);
#endif /* NO_SETENV */

#endif /* !LDM_SETENV_H */
