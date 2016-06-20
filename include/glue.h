#include <svdlib.h>

DMat get_svdrec_ut(SVDRec s);
double *get_svdrec_s(SVDRec s);
DMat get_svdrec_vt(SVDRec s);
long get_svdrec_rank(SVDRec s);

long get_dmat_rows(DMat d);
long get_dmat_cols(DMat d);
double *get_dmat_buffer(DMat d);

void free_dmat(DMat d);


long get_smat_rows(SMat m);
long get_smat_cols(SMat m);
long *get_smat_pointr(SMat m);
long *get_smat_rowind(SMat m);
double *get_smat_value(SMat m);
SMat svd_new_smat_from_csr(int rows, int cols, int vals, long *pointr, long *rowind, double *value);

void free_smat(SMat m);

void set_verbosity(long v);
