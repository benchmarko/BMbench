/*
 * BM Bench - bmbench_f2clib.c (Fortran f2c library)
 * (c) Marco Vieth, 2002
 * http://www.benchmarko.de
 *
 * 26.01.2003  0.01
 *
 *
 * This library supplies the function cpu_time__(real) for f2c (Convert Fortran 77 to C or C++)
 *
 */

#include <stdio.h>
#include <stdlib.h>
/* #include <time.h> */ /* time */

#ifdef __BORLANDC__
#define Use_Windows
#define _ftime ftime
#define _timeb timeb
#include <dos.h>  /* _8087 */
#else /* UNIX, windows 32 bit */
#endif

/* #define Use_Windows */
#ifdef Use_Windows
#include <sys/timeb.h>
#else
#include <sys/time.h> /* gettimeofday */
#endif


#include "f2c.h"

typedef real bmtime_t;

int cpu_time__(bmtime_t *val) {
  long ltime;
#ifndef Use_Windows
  struct timeval tv;
  gettimeofday(&tv, NULL);
  ltime = (long)tv.tv_sec * 1000 + tv.tv_usec / 1000;
#else
  struct _timeb tb;
  _ftime(&tb);
  ltime = (long)tb.time * 1000 + tb.millitm;
#endif
  *val = ltime / 1000.0;
  /* printf("DEBUG: val=%f\n", (double)*val); */
  return 0;
}
