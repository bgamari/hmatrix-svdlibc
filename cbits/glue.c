#include <stdlib.h>
#include <svdlib.h>
#include <glue.h>

DMat get_svdrec_ut(SVDRec s) { return s->Ut; }
double *get_svdrec_s(SVDRec s) { return s->S; }
DMat get_svdrec_vt(SVDRec s) { return s->Vt; }
long get_svdrec_rank(SVDRec s) { return s->d; }

long get_dmat_rows(DMat d) { return d->rows; }
long get_dmat_cols(DMat d) { return d->cols; }
double **get_dmat_value(DMat d) { return d->value; }

void free_dmat(DMat d) {
  free(d->value);
  free(d);
}
