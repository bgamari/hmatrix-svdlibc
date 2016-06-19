#include <stdlib.h>
#include <stdio.h>
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

/** Take a matrix in CSR format (which is what HMatrix gives) and turn it into
    CSC format, meanwhile making a copy so that GC is happy.*/
SMat svd_new_smat_from_csrT(int rows, int cols, int vals, long *rowstart, long *colind, double *csr_value) {
  /*fprintf(stderr, "rows %d cols %d vals %d\nvals ", rows, cols, vals);
  for (int val=0; val < vals; val++) fprintf(stderr, " %0.3f", csr_value[val]);
  fprintf(stderr, "\n");
  for (int row=0; row < rows; row++) {
    for (int i=rowstart[row]; i < rowstart[row+1]; i++) {
      fprintf(stderr, "m[r=%d c=%ld, i=%d]: %.03f", row, colind[i], i, csr_value[i]);
    }
    fprintf(stderr, "\n");
  }*/

  struct smat S1;
  // A CSR matrix is the same as a Transposed CSC
  S1.rows = cols;
  S1.cols = rows;
  S1.vals = vals;
  S1.pointr = rowstart;
  S1.rowind = colind;
  S1.value  = csr_value;
  fprintf(stderr, "Transposing\n");
  SMat S2 = svdTransposeS(&S1);
  fprintf(stderr, "transposed\n");
  return S2;
}

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
