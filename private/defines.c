#include <sys/ioctl.h>
#include "escheme.h"

#define MODULE_NAME "defines"

Scheme_Object* scheme_reload(Scheme_Env* env)
{
  Scheme_Env *mod;
  Scheme_Object* obj;
  
  mod = scheme_primitive_module(scheme_intern_symbol(MODULE_NAME), env);

#define DEF(a) \
  obj = scheme_make_integer(a); \
  scheme_add_global(#a, obj, mod);


  DEF(TIOCMGET);
  DEF(TIOCMBIS);
  DEF(TIOCMBIC);
  DEF(TIOCMSET);

  DEF(FIONREAD);

  DEF(TIOCSBRK);
  DEF(TIOCCBRK);

  DEF(TIOCM_DTR);
  DEF(TIOCM_RTS);
  DEF(TIOCM_CTS);
  DEF(TIOCM_CD);
  DEF(TIOCM_RI);
  DEF(TIOCM_DSR);

  scheme_finish_primitive_module(mod);
  return scheme_void;
}

Scheme_Object* scheme_initialize(Scheme_Env* env)
{
  return scheme_reload(env);
}

Scheme_Object* scheme_module_name()
{
  return scheme_intern_symbol(MODULE_NAME);
}
