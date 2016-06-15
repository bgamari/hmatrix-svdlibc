#include <stdlib.h>
#include <svdlib.h>
#include <glue.h>

/* For reference, this is from svdlib.h
Row-major dense matrix.  Rows are consecutive vectors.
struct dmat {
  long rows;
  long cols;
  double **value; // Accessed by [row][col]. Free value[0] and value to free.
}; */

DMat get_svdrec_ut(SVDRec s) { return s->Ut; }
double *get_svdrec_s(SVDRec s) { return s->S; }
DMat get_svdrec_vt(SVDRec s) { return s->Vt; }
long get_svdrec_rank(SVDRec s) { return s->d; }

long get_dmat_rows(DMat d) { return d->rows; }
long get_dmat_cols(DMat d) { return d->cols; }
double *get_dmat_buffer(DMat d) { return d->value[0]; }

void free_dmat(DMat d) {
  free(d->value);
  free(d);
}

/* For reference, this is in svdlib.h: Harwell-Boeing sparse matrix.
struct smat {
  long rows;
  long cols;
  long vals;     // Total non-zero entries.
  long *pointr;  // For each col (plus 1), index of first non-zero entry.
  long *rowind;  // For each nz entry, the row index.
  double *value; // For each nz entry, the value.
}; */

long get_smat_rows(SMat m) { return m->rows; };
long get_smat_cols(SMat m) { return m->cols; };
long *get_smat_pointr(SMat m) { return m->pointr; };
long *get_smat_rowind(SMat m) { return m->rowind; };
double *get_smat_value(SMat m) { return m->value; };

void free_smat(SMat m) {
  free(m->pointr);
  free(m->rowind);
  free(m->value);
  free(m);
};

void set_verbosity(long v) { SVDVerbosity = v; }
