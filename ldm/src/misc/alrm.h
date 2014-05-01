/*
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: alrm.h,v 1.10.18.2 2005/06/24 21:05:18 steve Exp $ */

#ifndef ALRM_H
#define ALRM_H

/*
 * Careful alarm interface
 */
#include <unistd.h>
#include <signal.h>
#include <setjmp.h>

/*
 * implement a signal function with known,
 * desired characteristics. -Stevens
 */
#if defined(__STDC__)
typedef void Sigfunc(int) ;
#else
typedef void Sigfunc() ;
#endif

static Sigfunc *
mysignal(int signo, Sigfunc *handler)
{
	struct sigaction sigact, oact ;

	sigact.sa_handler = handler ;
	sigemptyset(&sigact.sa_mask) ;
	sigact.sa_flags = 0 ;

    /* Don't restart after alarms */
#ifdef SA_INTERRUPT /* SunOS 4.x */
    sigact.sa_flags |= SA_INTERRUPT ;
#endif

	if(sigaction(signo, &sigact, &oact) == -1)
		return(SIG_ERR)	;
	/* else */
	return (oact.sa_handler)  ;
}


/* set when the jmp_buf is valid */
static volatile sig_atomic_t validJmpbuf = 0 ;
static sigjmp_buf env ;
static Sigfunc *sav_handler = SIG_ERR ;

/*ARGSUSED*/
static void
alrm_handler(int sig)
{
	if(!validJmpbuf)
	{
		/* TODO: something rational */
		return ;
	}
	/* else, safe to do the jump */
	validJmpbuf = 0 ;
	(void) siglongjmp(env, 1) ;
}


/*
 * Safe to call even when set_alarm()
 * has not been called
 */
#define CLR_ALRM() \
{ \
	(void) alarm(0) ; \
	if(sav_handler != SIG_ERR) \
	{ \
		(void) mysignal(SIGALRM, sav_handler) ; \
		sav_handler = SIG_ERR ; \
	} \
	validJmpbuf = 0 ; \
}


#define SET_ALARM(seconds, jumplabel) \
{ \
	if( sigsetjmp(env, 1) ) \
	{ \
		/* \
	 	 * Result of a signal_handler call to siglongjmp(). \
		 */ \
		if(sav_handler != SIG_ERR) \
		{ \
			(void) mysignal(SIGALRM, sav_handler) ; \
			sav_handler = SIG_ERR ; \
		} \
		goto jumplabel ; \
	} \
	/* else, first time, we set the jmp env */ \
	validJmpbuf = 1 ; \
 \
	sav_handler = mysignal(SIGALRM, alrm_handler) ; \
	if(sav_handler == SIG_ERR) \
	{ \
		validJmpbuf = 0 ; \
	} else { \
		(void) alarm((unsigned)(seconds)) ; \
	} \
}

#endif /* ALRM_H */
