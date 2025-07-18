#ifndef __omega_h__
#define __omega_h__

#if defined(__cplusplus)
extern "C" {
#endif

  int _lotriOmega_matSize(void);
  void _lotriOmega_mat_sqrt(int *dm, double *_t, int *length_theta, int  *_tn, double *ret);
  void _lotriOmega_mat_log(int *dm, double *_t, int *length_theta, int  *_tn, double *ret);
  void _lotriOmega_mat(int *dm, double *_t, int *length_theta, int  *_tn, double *ret);

#if defined(__cplusplus)
}
#endif

#endif
