
#ifndef TS_tsim_argp_H_
#define TS_tsim_argp_H_

int tsa_speed();
int tsa_audio();
int tsa_priority();
int tsa_verbose();
int tsa_height();
int tsa_width();

char* parse(
      int argc,
      char **argv);

#endif /* TS_tsim_argp_H_ */

