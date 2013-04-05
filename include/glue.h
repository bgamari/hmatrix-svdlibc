#include <svdlib.h>

DMat get_svdrec_ut(SVDRec s);
double *get_svdrec_s(SVDRec s);
DMat get_svdrec_vt(SVDRec s);
long get_svdrec_rank(SVDRec s);

long get_dmat_rows(DMat d);
long get_dmat_cols(DMat d);
double **get_dmat_value(DMat d);

void free_dmat(DMat d);
