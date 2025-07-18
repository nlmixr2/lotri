//Generated from ::document() for 12 dimensions
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>
#include <Rmath.h>
void _lotriOmega_mat(int *dm, double *_t, int *length_theta, int  *_tn, double *ret){
if (*dm == 1) {
  if (*_tn== NA_INTEGER){
    ret[0]=4;
    return;  
}

if (*_tn == -2){
    ret[0] = 1;
    return;
  }
  else if (*_tn < -3 || *_tn > 1){
    ret[0] = NA_REAL;ret[1] = -1; //error("d(Omega^-1) derivative outside bounds");
  }
  else if (*length_theta != 1){
    ret[0] = NA_REAL;ret[1] = *length_theta;//error("requires vector with 1 arguments");
  }
    if (*_tn == 0){
      ret[0] = (_t[0]);
    }
    else if (*_tn == -1){
      ret[0] = pow(_t[0], 2);
    }
    else if (*_tn == 1){
      ret[0] = 2*_t[0];
    }
    return;
  }
else if (*dm == 2) {
  if (*_tn== NA_INTEGER){
    ret[0]=4;
    ret[1]=5;
    ret[2]=4;
    return;  
}

if (*_tn == -2){
    ret[0] = 3;
    return;
  }
  else if (*_tn < -5 || *_tn > 3){
    ret[0] = NA_REAL;ret[1] = -1; //error("d(Omega^-1) derivative outside bounds");
  }
  else if (*length_theta != 3){
    ret[0] = NA_REAL;ret[1] = *length_theta;//error("requires vector with 3 arguments");
  }
    if (*_tn == 0){
      ret[0] = (_t[0]);
      ret[2] = _t[1];
      ret[3] = (_t[2]);
    }
    else if (*_tn == -1){
      ret[0] = pow(_t[0], 2);
      ret[1] = _t[1]*_t[0];
      ret[2] = _t[1]*_t[0];
      ret[3] = pow(_t[1], 2)+pow(_t[2], 2);
    }
    else if (*_tn == 1){
      ret[0] = 2*_t[0];
      ret[1] = _t[1];
      ret[2] = _t[1];
    }
    else if (*_tn == 2){
      ret[1] = _t[0];
      ret[2] = _t[0];
      ret[3] = 2*_t[1];
    }
    else if (*_tn == 3){
      ret[3] = 2*_t[2];
    }
    return;
  }
else if (*dm == 3) {
  if (*_tn== NA_INTEGER){
    ret[0]=4;
    ret[1]=5;
    ret[2]=4;
    ret[3]=5;
    ret[4]=5;
    ret[5]=4;
    return;  
}

if (*_tn == -2){
    ret[0] = 6;
    return;
  }
  else if (*_tn < -8 || *_tn > 6){
    ret[0] = NA_REAL;ret[1] = -1; //error("d(Omega^-1) derivative outside bounds");
  }
  else if (*length_theta != 6){
    ret[0] = NA_REAL;ret[1] = *length_theta;//error("requires vector with 6 arguments");
  }
    if (*_tn == 0){
      ret[0] = (_t[0]);
      ret[3] = _t[1];
      ret[4] = (_t[2]);
      ret[6] = _t[3];
      ret[7] = _t[4];
      ret[8] = (_t[5]);
    }
    else if (*_tn == -1){
      ret[0] = pow(_t[0], 2);
      ret[1] = _t[1]*_t[0];
      ret[2] = _t[0]*_t[3];
      ret[3] = _t[1]*_t[0];
      ret[4] = pow(_t[1], 2)+pow(_t[2], 2);
      ret[5] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[6] = _t[0]*_t[3];
      ret[7] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[8] = pow(_t[3], 2)+pow(_t[4], 2)+pow(_t[5], 2);
    }
    else if (*_tn == 1){
      ret[0] = 2*_t[0];
      ret[1] = _t[1];
      ret[2] = _t[3];
      ret[3] = _t[1];
      ret[6] = _t[3];
    }
    else if (*_tn == 2){
      ret[1] = _t[0];
      ret[3] = _t[0];
      ret[4] = 2*_t[1];
      ret[5] = _t[3];
      ret[7] = _t[3];
    }
    else if (*_tn == 3){
      ret[4] = 2*_t[2];
      ret[5] = _t[4];
      ret[7] = _t[4];
    }
    else if (*_tn == 4){
      ret[2] = _t[0];
      ret[5] = _t[1];
      ret[6] = _t[0];
      ret[7] = _t[1];
      ret[8] = 2*_t[3];
    }
    else if (*_tn == 5){
      ret[5] = _t[2];
      ret[7] = _t[2];
      ret[8] = 2*_t[4];
    }
    else if (*_tn == 6){
      ret[8] = 2*_t[5];
    }
    return;
  }
else if (*dm == 4) {
  if (*_tn== NA_INTEGER){
    ret[0]=4;
    ret[1]=5;
    ret[2]=4;
    ret[3]=5;
    ret[4]=5;
    ret[5]=4;
    ret[6]=5;
    ret[7]=5;
    ret[8]=5;
    ret[9]=4;
    return;  
}

if (*_tn == -2){
    ret[0] = 10;
    return;
  }
  else if (*_tn < -12 || *_tn > 10){
    ret[0] = NA_REAL;ret[1] = -1; //error("d(Omega^-1) derivative outside bounds");
  }
  else if (*length_theta != 10){
    ret[0] = NA_REAL;ret[1] = *length_theta;//error("requires vector with 10 arguments");
  }
    if (*_tn == 0){
      ret[0] = (_t[0]);
      ret[4] = _t[1];
      ret[5] = (_t[2]);
      ret[8] = _t[3];
      ret[9] = _t[4];
      ret[10] = (_t[5]);
      ret[12] = _t[6];
      ret[13] = _t[7];
      ret[14] = _t[8];
      ret[15] = (_t[9]);
    }
    else if (*_tn == -1){
      ret[0] = pow(_t[0], 2);
      ret[1] = _t[1]*_t[0];
      ret[2] = _t[0]*_t[3];
      ret[3] = _t[6]*_t[0];
      ret[4] = _t[1]*_t[0];
      ret[5] = pow(_t[1], 2)+pow(_t[2], 2);
      ret[6] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[7] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[8] = _t[0]*_t[3];
      ret[9] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[10] = pow(_t[3], 2)+pow(_t[4], 2)+pow(_t[5], 2);
      ret[11] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[12] = _t[6]*_t[0];
      ret[13] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[14] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[15] = pow(_t[6], 2)+pow(_t[7], 2)+pow(_t[8], 2)+pow(_t[9], 2);
    }
    else if (*_tn == 1){
      ret[0] = 2*_t[0];
      ret[1] = _t[1];
      ret[2] = _t[3];
      ret[3] = _t[6];
      ret[4] = _t[1];
      ret[8] = _t[3];
      ret[12] = _t[6];
    }
    else if (*_tn == 2){
      ret[1] = _t[0];
      ret[4] = _t[0];
      ret[5] = 2*_t[1];
      ret[6] = _t[3];
      ret[7] = _t[6];
      ret[9] = _t[3];
      ret[13] = _t[6];
    }
    else if (*_tn == 3){
      ret[5] = 2*_t[2];
      ret[6] = _t[4];
      ret[7] = _t[7];
      ret[9] = _t[4];
      ret[13] = _t[7];
    }
    else if (*_tn == 4){
      ret[2] = _t[0];
      ret[6] = _t[1];
      ret[8] = _t[0];
      ret[9] = _t[1];
      ret[10] = 2*_t[3];
      ret[11] = _t[6];
      ret[14] = _t[6];
    }
    else if (*_tn == 5){
      ret[6] = _t[2];
      ret[9] = _t[2];
      ret[10] = 2*_t[4];
      ret[11] = _t[7];
      ret[14] = _t[7];
    }
    else if (*_tn == 6){
      ret[10] = 2*_t[5];
      ret[11] = _t[8];
      ret[14] = _t[8];
    }
    else if (*_tn == 7){
      ret[3] = _t[0];
      ret[7] = _t[1];
      ret[11] = _t[3];
      ret[12] = _t[0];
      ret[13] = _t[1];
      ret[14] = _t[3];
      ret[15] = 2*_t[6];
    }
    else if (*_tn == 8){
      ret[7] = _t[2];
      ret[11] = _t[4];
      ret[13] = _t[2];
      ret[14] = _t[4];
      ret[15] = 2*_t[7];
    }
    else if (*_tn == 9){
      ret[11] = _t[5];
      ret[14] = _t[5];
      ret[15] = 2*_t[8];
    }
    else if (*_tn == 10){
      ret[15] = 2*_t[9];
    }
    return;
  }
else if (*dm == 5) {
  if (*_tn== NA_INTEGER){
    ret[0]=4;
    ret[1]=5;
    ret[2]=4;
    ret[3]=5;
    ret[4]=5;
    ret[5]=4;
    ret[6]=5;
    ret[7]=5;
    ret[8]=5;
    ret[9]=4;
    ret[10]=5;
    ret[11]=5;
    ret[12]=5;
    ret[13]=5;
    ret[14]=4;
    return;  
}

if (*_tn == -2){
    ret[0] = 15;
    return;
  }
  else if (*_tn < -17 || *_tn > 15){
    ret[0] = NA_REAL;ret[1] = -1; //error("d(Omega^-1) derivative outside bounds");
  }
  else if (*length_theta != 15){
    ret[0] = NA_REAL;ret[1] = *length_theta;//error("requires vector with 15 arguments");
  }
    if (*_tn == 0){
      ret[0] = (_t[0]);
      ret[5] = _t[1];
      ret[6] = (_t[2]);
      ret[10] = _t[3];
      ret[11] = _t[4];
      ret[12] = (_t[5]);
      ret[15] = _t[6];
      ret[16] = _t[7];
      ret[17] = _t[8];
      ret[18] = (_t[9]);
      ret[20] = _t[10];
      ret[21] = _t[11];
      ret[22] = _t[12];
      ret[23] = _t[13];
      ret[24] = (_t[14]);
    }
    else if (*_tn == -1){
      ret[0] = pow(_t[0], 2);
      ret[1] = _t[1]*_t[0];
      ret[2] = _t[0]*_t[3];
      ret[3] = _t[6]*_t[0];
      ret[4] = _t[0]*_t[10];
      ret[5] = _t[1]*_t[0];
      ret[6] = pow(_t[1], 2)+pow(_t[2], 2);
      ret[7] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[8] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[9] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[10] = _t[0]*_t[3];
      ret[11] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[12] = pow(_t[3], 2)+pow(_t[4], 2)+pow(_t[5], 2);
      ret[13] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[14] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[15] = _t[6]*_t[0];
      ret[16] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[17] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[18] = pow(_t[6], 2)+pow(_t[7], 2)+pow(_t[8], 2)+pow(_t[9], 2);
      ret[19] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[20] = _t[0]*_t[10];
      ret[21] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[22] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[23] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[24] = pow(_t[10], 2)+pow(_t[11], 2)+pow(_t[12], 2)+pow(_t[13], 2)+pow(_t[14], 2);
    }
    else if (*_tn == 1){
      ret[0] = 2*_t[0];
      ret[1] = _t[1];
      ret[2] = _t[3];
      ret[3] = _t[6];
      ret[4] = _t[10];
      ret[5] = _t[1];
      ret[10] = _t[3];
      ret[15] = _t[6];
      ret[20] = _t[10];
    }
    else if (*_tn == 2){
      ret[1] = _t[0];
      ret[5] = _t[0];
      ret[6] = 2*_t[1];
      ret[7] = _t[3];
      ret[8] = _t[6];
      ret[9] = _t[10];
      ret[11] = _t[3];
      ret[16] = _t[6];
      ret[21] = _t[10];
    }
    else if (*_tn == 3){
      ret[6] = 2*_t[2];
      ret[7] = _t[4];
      ret[8] = _t[7];
      ret[9] = _t[11];
      ret[11] = _t[4];
      ret[16] = _t[7];
      ret[21] = _t[11];
    }
    else if (*_tn == 4){
      ret[2] = _t[0];
      ret[7] = _t[1];
      ret[10] = _t[0];
      ret[11] = _t[1];
      ret[12] = 2*_t[3];
      ret[13] = _t[6];
      ret[14] = _t[10];
      ret[17] = _t[6];
      ret[22] = _t[10];
    }
    else if (*_tn == 5){
      ret[7] = _t[2];
      ret[11] = _t[2];
      ret[12] = 2*_t[4];
      ret[13] = _t[7];
      ret[14] = _t[11];
      ret[17] = _t[7];
      ret[22] = _t[11];
    }
    else if (*_tn == 6){
      ret[12] = 2*_t[5];
      ret[13] = _t[8];
      ret[14] = _t[12];
      ret[17] = _t[8];
      ret[22] = _t[12];
    }
    else if (*_tn == 7){
      ret[3] = _t[0];
      ret[8] = _t[1];
      ret[13] = _t[3];
      ret[15] = _t[0];
      ret[16] = _t[1];
      ret[17] = _t[3];
      ret[18] = 2*_t[6];
      ret[19] = _t[10];
      ret[23] = _t[10];
    }
    else if (*_tn == 8){
      ret[8] = _t[2];
      ret[13] = _t[4];
      ret[16] = _t[2];
      ret[17] = _t[4];
      ret[18] = 2*_t[7];
      ret[19] = _t[11];
      ret[23] = _t[11];
    }
    else if (*_tn == 9){
      ret[13] = _t[5];
      ret[17] = _t[5];
      ret[18] = 2*_t[8];
      ret[19] = _t[12];
      ret[23] = _t[12];
    }
    else if (*_tn == 10){
      ret[18] = 2*_t[9];
      ret[19] = _t[13];
      ret[23] = _t[13];
    }
    else if (*_tn == 11){
      ret[4] = _t[0];
      ret[9] = _t[1];
      ret[14] = _t[3];
      ret[19] = _t[6];
      ret[20] = _t[0];
      ret[21] = _t[1];
      ret[22] = _t[3];
      ret[23] = _t[6];
      ret[24] = 2*_t[10];
    }
    else if (*_tn == 12){
      ret[9] = _t[2];
      ret[14] = _t[4];
      ret[19] = _t[7];
      ret[21] = _t[2];
      ret[22] = _t[4];
      ret[23] = _t[7];
      ret[24] = 2*_t[11];
    }
    else if (*_tn == 13){
      ret[14] = _t[5];
      ret[19] = _t[8];
      ret[22] = _t[5];
      ret[23] = _t[8];
      ret[24] = 2*_t[12];
    }
    else if (*_tn == 14){
      ret[19] = _t[9];
      ret[23] = _t[9];
      ret[24] = 2*_t[13];
    }
    else if (*_tn == 15){
      ret[24] = 2*_t[14];
    }
    return;
  }
else if (*dm == 6) {
  if (*_tn== NA_INTEGER){
    ret[0]=4;
    ret[1]=5;
    ret[2]=4;
    ret[3]=5;
    ret[4]=5;
    ret[5]=4;
    ret[6]=5;
    ret[7]=5;
    ret[8]=5;
    ret[9]=4;
    ret[10]=5;
    ret[11]=5;
    ret[12]=5;
    ret[13]=5;
    ret[14]=4;
    ret[15]=5;
    ret[16]=5;
    ret[17]=5;
    ret[18]=5;
    ret[19]=5;
    ret[20]=4;
    return;  
}

if (*_tn == -2){
    ret[0] = 21;
    return;
  }
  else if (*_tn < -23 || *_tn > 21){
    ret[0] = NA_REAL;ret[1] = -1; //error("d(Omega^-1) derivative outside bounds");
  }
  else if (*length_theta != 21){
    ret[0] = NA_REAL;ret[1] = *length_theta;//error("requires vector with 21 arguments");
  }
    if (*_tn == 0){
      ret[0] = (_t[0]);
      ret[6] = _t[1];
      ret[7] = (_t[2]);
      ret[12] = _t[3];
      ret[13] = _t[4];
      ret[14] = (_t[5]);
      ret[18] = _t[6];
      ret[19] = _t[7];
      ret[20] = _t[8];
      ret[21] = (_t[9]);
      ret[24] = _t[10];
      ret[25] = _t[11];
      ret[26] = _t[12];
      ret[27] = _t[13];
      ret[28] = (_t[14]);
      ret[30] = _t[15];
      ret[31] = _t[16];
      ret[32] = _t[17];
      ret[33] = _t[18];
      ret[34] = _t[19];
      ret[35] = (_t[20]);
    }
    else if (*_tn == -1){
      ret[0] = pow(_t[0], 2);
      ret[1] = _t[1]*_t[0];
      ret[2] = _t[0]*_t[3];
      ret[3] = _t[6]*_t[0];
      ret[4] = _t[0]*_t[10];
      ret[5] = _t[0]*_t[15];
      ret[6] = _t[1]*_t[0];
      ret[7] = pow(_t[1], 2)+pow(_t[2], 2);
      ret[8] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[9] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[10] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[11] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[12] = _t[0]*_t[3];
      ret[13] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[14] = pow(_t[3], 2)+pow(_t[4], 2)+pow(_t[5], 2);
      ret[15] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[16] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[17] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[18] = _t[6]*_t[0];
      ret[19] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[20] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[21] = pow(_t[6], 2)+pow(_t[7], 2)+pow(_t[8], 2)+pow(_t[9], 2);
      ret[22] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[23] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[24] = _t[0]*_t[10];
      ret[25] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[26] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[27] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[28] = pow(_t[10], 2)+pow(_t[11], 2)+pow(_t[12], 2)+pow(_t[13], 2)+pow(_t[14], 2);
      ret[29] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[30] = _t[0]*_t[15];
      ret[31] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[32] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[33] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[34] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[35] = pow(_t[15], 2)+pow(_t[16], 2)+pow(_t[17], 2)+pow(_t[18], 2)+pow(_t[19], 2)+pow(_t[20], 2);
    }
    else if (*_tn == 1){
      ret[0] = 2*_t[0];
      ret[1] = _t[1];
      ret[2] = _t[3];
      ret[3] = _t[6];
      ret[4] = _t[10];
      ret[5] = _t[15];
      ret[6] = _t[1];
      ret[12] = _t[3];
      ret[18] = _t[6];
      ret[24] = _t[10];
      ret[30] = _t[15];
    }
    else if (*_tn == 2){
      ret[1] = _t[0];
      ret[6] = _t[0];
      ret[7] = 2*_t[1];
      ret[8] = _t[3];
      ret[9] = _t[6];
      ret[10] = _t[10];
      ret[11] = _t[15];
      ret[13] = _t[3];
      ret[19] = _t[6];
      ret[25] = _t[10];
      ret[31] = _t[15];
    }
    else if (*_tn == 3){
      ret[7] = 2*_t[2];
      ret[8] = _t[4];
      ret[9] = _t[7];
      ret[10] = _t[11];
      ret[11] = _t[16];
      ret[13] = _t[4];
      ret[19] = _t[7];
      ret[25] = _t[11];
      ret[31] = _t[16];
    }
    else if (*_tn == 4){
      ret[2] = _t[0];
      ret[8] = _t[1];
      ret[12] = _t[0];
      ret[13] = _t[1];
      ret[14] = 2*_t[3];
      ret[15] = _t[6];
      ret[16] = _t[10];
      ret[17] = _t[15];
      ret[20] = _t[6];
      ret[26] = _t[10];
      ret[32] = _t[15];
    }
    else if (*_tn == 5){
      ret[8] = _t[2];
      ret[13] = _t[2];
      ret[14] = 2*_t[4];
      ret[15] = _t[7];
      ret[16] = _t[11];
      ret[17] = _t[16];
      ret[20] = _t[7];
      ret[26] = _t[11];
      ret[32] = _t[16];
    }
    else if (*_tn == 6){
      ret[14] = 2*_t[5];
      ret[15] = _t[8];
      ret[16] = _t[12];
      ret[17] = _t[17];
      ret[20] = _t[8];
      ret[26] = _t[12];
      ret[32] = _t[17];
    }
    else if (*_tn == 7){
      ret[3] = _t[0];
      ret[9] = _t[1];
      ret[15] = _t[3];
      ret[18] = _t[0];
      ret[19] = _t[1];
      ret[20] = _t[3];
      ret[21] = 2*_t[6];
      ret[22] = _t[10];
      ret[23] = _t[15];
      ret[27] = _t[10];
      ret[33] = _t[15];
    }
    else if (*_tn == 8){
      ret[9] = _t[2];
      ret[15] = _t[4];
      ret[19] = _t[2];
      ret[20] = _t[4];
      ret[21] = 2*_t[7];
      ret[22] = _t[11];
      ret[23] = _t[16];
      ret[27] = _t[11];
      ret[33] = _t[16];
    }
    else if (*_tn == 9){
      ret[15] = _t[5];
      ret[20] = _t[5];
      ret[21] = 2*_t[8];
      ret[22] = _t[12];
      ret[23] = _t[17];
      ret[27] = _t[12];
      ret[33] = _t[17];
    }
    else if (*_tn == 10){
      ret[21] = 2*_t[9];
      ret[22] = _t[13];
      ret[23] = _t[18];
      ret[27] = _t[13];
      ret[33] = _t[18];
    }
    else if (*_tn == 11){
      ret[4] = _t[0];
      ret[10] = _t[1];
      ret[16] = _t[3];
      ret[22] = _t[6];
      ret[24] = _t[0];
      ret[25] = _t[1];
      ret[26] = _t[3];
      ret[27] = _t[6];
      ret[28] = 2*_t[10];
      ret[29] = _t[15];
      ret[34] = _t[15];
    }
    else if (*_tn == 12){
      ret[10] = _t[2];
      ret[16] = _t[4];
      ret[22] = _t[7];
      ret[25] = _t[2];
      ret[26] = _t[4];
      ret[27] = _t[7];
      ret[28] = 2*_t[11];
      ret[29] = _t[16];
      ret[34] = _t[16];
    }
    else if (*_tn == 13){
      ret[16] = _t[5];
      ret[22] = _t[8];
      ret[26] = _t[5];
      ret[27] = _t[8];
      ret[28] = 2*_t[12];
      ret[29] = _t[17];
      ret[34] = _t[17];
    }
    else if (*_tn == 14){
      ret[22] = _t[9];
      ret[27] = _t[9];
      ret[28] = 2*_t[13];
      ret[29] = _t[18];
      ret[34] = _t[18];
    }
    else if (*_tn == 15){
      ret[28] = 2*_t[14];
      ret[29] = _t[19];
      ret[34] = _t[19];
    }
    else if (*_tn == 16){
      ret[5] = _t[0];
      ret[11] = _t[1];
      ret[17] = _t[3];
      ret[23] = _t[6];
      ret[29] = _t[10];
      ret[30] = _t[0];
      ret[31] = _t[1];
      ret[32] = _t[3];
      ret[33] = _t[6];
      ret[34] = _t[10];
      ret[35] = 2*_t[15];
    }
    else if (*_tn == 17){
      ret[11] = _t[2];
      ret[17] = _t[4];
      ret[23] = _t[7];
      ret[29] = _t[11];
      ret[31] = _t[2];
      ret[32] = _t[4];
      ret[33] = _t[7];
      ret[34] = _t[11];
      ret[35] = 2*_t[16];
    }
    else if (*_tn == 18){
      ret[17] = _t[5];
      ret[23] = _t[8];
      ret[29] = _t[12];
      ret[32] = _t[5];
      ret[33] = _t[8];
      ret[34] = _t[12];
      ret[35] = 2*_t[17];
    }
    else if (*_tn == 19){
      ret[23] = _t[9];
      ret[29] = _t[13];
      ret[33] = _t[9];
      ret[34] = _t[13];
      ret[35] = 2*_t[18];
    }
    else if (*_tn == 20){
      ret[29] = _t[14];
      ret[34] = _t[14];
      ret[35] = 2*_t[19];
    }
    else if (*_tn == 21){
      ret[35] = 2*_t[20];
    }
    return;
  }
else if (*dm == 7) {
  if (*_tn== NA_INTEGER){
    ret[0]=4;
    ret[1]=5;
    ret[2]=4;
    ret[3]=5;
    ret[4]=5;
    ret[5]=4;
    ret[6]=5;
    ret[7]=5;
    ret[8]=5;
    ret[9]=4;
    ret[10]=5;
    ret[11]=5;
    ret[12]=5;
    ret[13]=5;
    ret[14]=4;
    ret[15]=5;
    ret[16]=5;
    ret[17]=5;
    ret[18]=5;
    ret[19]=5;
    ret[20]=4;
    ret[21]=5;
    ret[22]=5;
    ret[23]=5;
    ret[24]=5;
    ret[25]=5;
    ret[26]=5;
    ret[27]=4;
    return;  
}

if (*_tn == -2){
    ret[0] = 28;
    return;
  }
  else if (*_tn < -30 || *_tn > 28){
    ret[0] = NA_REAL;ret[1] = -1; //error("d(Omega^-1) derivative outside bounds");
  }
  else if (*length_theta != 28){
    ret[0] = NA_REAL;ret[1] = *length_theta;//error("requires vector with 28 arguments");
  }
    if (*_tn == 0){
      ret[0] = (_t[0]);
      ret[7] = _t[1];
      ret[8] = (_t[2]);
      ret[14] = _t[3];
      ret[15] = _t[4];
      ret[16] = (_t[5]);
      ret[21] = _t[6];
      ret[22] = _t[7];
      ret[23] = _t[8];
      ret[24] = (_t[9]);
      ret[28] = _t[10];
      ret[29] = _t[11];
      ret[30] = _t[12];
      ret[31] = _t[13];
      ret[32] = (_t[14]);
      ret[35] = _t[15];
      ret[36] = _t[16];
      ret[37] = _t[17];
      ret[38] = _t[18];
      ret[39] = _t[19];
      ret[40] = (_t[20]);
      ret[42] = _t[21];
      ret[43] = _t[22];
      ret[44] = _t[23];
      ret[45] = _t[24];
      ret[46] = _t[25];
      ret[47] = _t[26];
      ret[48] = (_t[27]);
    }
    else if (*_tn == -1){
      ret[0] = pow(_t[0], 2);
      ret[1] = _t[1]*_t[0];
      ret[2] = _t[0]*_t[3];
      ret[3] = _t[6]*_t[0];
      ret[4] = _t[0]*_t[10];
      ret[5] = _t[0]*_t[15];
      ret[6] = _t[0]*_t[21];
      ret[7] = _t[1]*_t[0];
      ret[8] = pow(_t[1], 2)+pow(_t[2], 2);
      ret[9] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[10] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[11] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[12] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[13] = _t[1]*_t[21]+_t[2]*_t[22];
      ret[14] = _t[0]*_t[3];
      ret[15] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[16] = pow(_t[3], 2)+pow(_t[4], 2)+pow(_t[5], 2);
      ret[17] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[18] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[19] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[20] = _t[3]*_t[21]+_t[4]*_t[22]+_t[5]*_t[23];
      ret[21] = _t[6]*_t[0];
      ret[22] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[23] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[24] = pow(_t[6], 2)+pow(_t[7], 2)+pow(_t[8], 2)+pow(_t[9], 2);
      ret[25] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[26] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[27] = _t[6]*_t[21]+_t[7]*_t[22]+_t[8]*_t[23]+_t[9]*_t[24];
      ret[28] = _t[0]*_t[10];
      ret[29] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[30] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[31] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[32] = pow(_t[10], 2)+pow(_t[11], 2)+pow(_t[12], 2)+pow(_t[13], 2)+pow(_t[14], 2);
      ret[33] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[34] = _t[21]*_t[10]+_t[22]*_t[11]+_t[23]*_t[12]+_t[24]*_t[13]+_t[25]*_t[14];
      ret[35] = _t[0]*_t[15];
      ret[36] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[37] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[38] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[39] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[40] = pow(_t[15], 2)+pow(_t[16], 2)+pow(_t[17], 2)+pow(_t[18], 2)+pow(_t[19], 2)+pow(_t[20], 2);
      ret[41] = _t[21]*_t[15]+_t[22]*_t[16]+_t[23]*_t[17]+_t[24]*_t[18]+_t[25]*_t[19]+_t[26]*_t[20];
      ret[42] = _t[0]*_t[21];
      ret[43] = _t[1]*_t[21]+_t[2]*_t[22];
      ret[44] = _t[3]*_t[21]+_t[4]*_t[22]+_t[5]*_t[23];
      ret[45] = _t[6]*_t[21]+_t[7]*_t[22]+_t[8]*_t[23]+_t[9]*_t[24];
      ret[46] = _t[21]*_t[10]+_t[22]*_t[11]+_t[23]*_t[12]+_t[24]*_t[13]+_t[25]*_t[14];
      ret[47] = _t[21]*_t[15]+_t[22]*_t[16]+_t[23]*_t[17]+_t[24]*_t[18]+_t[25]*_t[19]+_t[26]*_t[20];
      ret[48] = pow(_t[21], 2)+pow(_t[22], 2)+pow(_t[23], 2)+pow(_t[24], 2)+pow(_t[25], 2)+pow(_t[26], 2)+pow(_t[27], 2);
    }
    else if (*_tn == 1){
      ret[0] = 2*_t[0];
      ret[1] = _t[1];
      ret[2] = _t[3];
      ret[3] = _t[6];
      ret[4] = _t[10];
      ret[5] = _t[15];
      ret[6] = _t[21];
      ret[7] = _t[1];
      ret[14] = _t[3];
      ret[21] = _t[6];
      ret[28] = _t[10];
      ret[35] = _t[15];
      ret[42] = _t[21];
    }
    else if (*_tn == 2){
      ret[1] = _t[0];
      ret[7] = _t[0];
      ret[8] = 2*_t[1];
      ret[9] = _t[3];
      ret[10] = _t[6];
      ret[11] = _t[10];
      ret[12] = _t[15];
      ret[13] = _t[21];
      ret[15] = _t[3];
      ret[22] = _t[6];
      ret[29] = _t[10];
      ret[36] = _t[15];
      ret[43] = _t[21];
    }
    else if (*_tn == 3){
      ret[8] = 2*_t[2];
      ret[9] = _t[4];
      ret[10] = _t[7];
      ret[11] = _t[11];
      ret[12] = _t[16];
      ret[13] = _t[22];
      ret[15] = _t[4];
      ret[22] = _t[7];
      ret[29] = _t[11];
      ret[36] = _t[16];
      ret[43] = _t[22];
    }
    else if (*_tn == 4){
      ret[2] = _t[0];
      ret[9] = _t[1];
      ret[14] = _t[0];
      ret[15] = _t[1];
      ret[16] = 2*_t[3];
      ret[17] = _t[6];
      ret[18] = _t[10];
      ret[19] = _t[15];
      ret[20] = _t[21];
      ret[23] = _t[6];
      ret[30] = _t[10];
      ret[37] = _t[15];
      ret[44] = _t[21];
    }
    else if (*_tn == 5){
      ret[9] = _t[2];
      ret[15] = _t[2];
      ret[16] = 2*_t[4];
      ret[17] = _t[7];
      ret[18] = _t[11];
      ret[19] = _t[16];
      ret[20] = _t[22];
      ret[23] = _t[7];
      ret[30] = _t[11];
      ret[37] = _t[16];
      ret[44] = _t[22];
    }
    else if (*_tn == 6){
      ret[16] = 2*_t[5];
      ret[17] = _t[8];
      ret[18] = _t[12];
      ret[19] = _t[17];
      ret[20] = _t[23];
      ret[23] = _t[8];
      ret[30] = _t[12];
      ret[37] = _t[17];
      ret[44] = _t[23];
    }
    else if (*_tn == 7){
      ret[3] = _t[0];
      ret[10] = _t[1];
      ret[17] = _t[3];
      ret[21] = _t[0];
      ret[22] = _t[1];
      ret[23] = _t[3];
      ret[24] = 2*_t[6];
      ret[25] = _t[10];
      ret[26] = _t[15];
      ret[27] = _t[21];
      ret[31] = _t[10];
      ret[38] = _t[15];
      ret[45] = _t[21];
    }
    else if (*_tn == 8){
      ret[10] = _t[2];
      ret[17] = _t[4];
      ret[22] = _t[2];
      ret[23] = _t[4];
      ret[24] = 2*_t[7];
      ret[25] = _t[11];
      ret[26] = _t[16];
      ret[27] = _t[22];
      ret[31] = _t[11];
      ret[38] = _t[16];
      ret[45] = _t[22];
    }
    else if (*_tn == 9){
      ret[17] = _t[5];
      ret[23] = _t[5];
      ret[24] = 2*_t[8];
      ret[25] = _t[12];
      ret[26] = _t[17];
      ret[27] = _t[23];
      ret[31] = _t[12];
      ret[38] = _t[17];
      ret[45] = _t[23];
    }
    else if (*_tn == 10){
      ret[24] = 2*_t[9];
      ret[25] = _t[13];
      ret[26] = _t[18];
      ret[27] = _t[24];
      ret[31] = _t[13];
      ret[38] = _t[18];
      ret[45] = _t[24];
    }
    else if (*_tn == 11){
      ret[4] = _t[0];
      ret[11] = _t[1];
      ret[18] = _t[3];
      ret[25] = _t[6];
      ret[28] = _t[0];
      ret[29] = _t[1];
      ret[30] = _t[3];
      ret[31] = _t[6];
      ret[32] = 2*_t[10];
      ret[33] = _t[15];
      ret[34] = _t[21];
      ret[39] = _t[15];
      ret[46] = _t[21];
    }
    else if (*_tn == 12){
      ret[11] = _t[2];
      ret[18] = _t[4];
      ret[25] = _t[7];
      ret[29] = _t[2];
      ret[30] = _t[4];
      ret[31] = _t[7];
      ret[32] = 2*_t[11];
      ret[33] = _t[16];
      ret[34] = _t[22];
      ret[39] = _t[16];
      ret[46] = _t[22];
    }
    else if (*_tn == 13){
      ret[18] = _t[5];
      ret[25] = _t[8];
      ret[30] = _t[5];
      ret[31] = _t[8];
      ret[32] = 2*_t[12];
      ret[33] = _t[17];
      ret[34] = _t[23];
      ret[39] = _t[17];
      ret[46] = _t[23];
    }
    else if (*_tn == 14){
      ret[25] = _t[9];
      ret[31] = _t[9];
      ret[32] = 2*_t[13];
      ret[33] = _t[18];
      ret[34] = _t[24];
      ret[39] = _t[18];
      ret[46] = _t[24];
    }
    else if (*_tn == 15){
      ret[32] = 2*_t[14];
      ret[33] = _t[19];
      ret[34] = _t[25];
      ret[39] = _t[19];
      ret[46] = _t[25];
    }
    else if (*_tn == 16){
      ret[5] = _t[0];
      ret[12] = _t[1];
      ret[19] = _t[3];
      ret[26] = _t[6];
      ret[33] = _t[10];
      ret[35] = _t[0];
      ret[36] = _t[1];
      ret[37] = _t[3];
      ret[38] = _t[6];
      ret[39] = _t[10];
      ret[40] = 2*_t[15];
      ret[41] = _t[21];
      ret[47] = _t[21];
    }
    else if (*_tn == 17){
      ret[12] = _t[2];
      ret[19] = _t[4];
      ret[26] = _t[7];
      ret[33] = _t[11];
      ret[36] = _t[2];
      ret[37] = _t[4];
      ret[38] = _t[7];
      ret[39] = _t[11];
      ret[40] = 2*_t[16];
      ret[41] = _t[22];
      ret[47] = _t[22];
    }
    else if (*_tn == 18){
      ret[19] = _t[5];
      ret[26] = _t[8];
      ret[33] = _t[12];
      ret[37] = _t[5];
      ret[38] = _t[8];
      ret[39] = _t[12];
      ret[40] = 2*_t[17];
      ret[41] = _t[23];
      ret[47] = _t[23];
    }
    else if (*_tn == 19){
      ret[26] = _t[9];
      ret[33] = _t[13];
      ret[38] = _t[9];
      ret[39] = _t[13];
      ret[40] = 2*_t[18];
      ret[41] = _t[24];
      ret[47] = _t[24];
    }
    else if (*_tn == 20){
      ret[33] = _t[14];
      ret[39] = _t[14];
      ret[40] = 2*_t[19];
      ret[41] = _t[25];
      ret[47] = _t[25];
    }
    else if (*_tn == 21){
      ret[40] = 2*_t[20];
      ret[41] = _t[26];
      ret[47] = _t[26];
    }
    else if (*_tn == 22){
      ret[6] = _t[0];
      ret[13] = _t[1];
      ret[20] = _t[3];
      ret[27] = _t[6];
      ret[34] = _t[10];
      ret[41] = _t[15];
      ret[42] = _t[0];
      ret[43] = _t[1];
      ret[44] = _t[3];
      ret[45] = _t[6];
      ret[46] = _t[10];
      ret[47] = _t[15];
      ret[48] = 2*_t[21];
    }
    else if (*_tn == 23){
      ret[13] = _t[2];
      ret[20] = _t[4];
      ret[27] = _t[7];
      ret[34] = _t[11];
      ret[41] = _t[16];
      ret[43] = _t[2];
      ret[44] = _t[4];
      ret[45] = _t[7];
      ret[46] = _t[11];
      ret[47] = _t[16];
      ret[48] = 2*_t[22];
    }
    else if (*_tn == 24){
      ret[20] = _t[5];
      ret[27] = _t[8];
      ret[34] = _t[12];
      ret[41] = _t[17];
      ret[44] = _t[5];
      ret[45] = _t[8];
      ret[46] = _t[12];
      ret[47] = _t[17];
      ret[48] = 2*_t[23];
    }
    else if (*_tn == 25){
      ret[27] = _t[9];
      ret[34] = _t[13];
      ret[41] = _t[18];
      ret[45] = _t[9];
      ret[46] = _t[13];
      ret[47] = _t[18];
      ret[48] = 2*_t[24];
    }
    else if (*_tn == 26){
      ret[34] = _t[14];
      ret[41] = _t[19];
      ret[46] = _t[14];
      ret[47] = _t[19];
      ret[48] = 2*_t[25];
    }
    else if (*_tn == 27){
      ret[41] = _t[20];
      ret[47] = _t[20];
      ret[48] = 2*_t[26];
    }
    else if (*_tn == 28){
      ret[48] = 2*_t[27];
    }
    return;
  }
else if (*dm == 8) {
  if (*_tn== NA_INTEGER){
    ret[0]=4;
    ret[1]=5;
    ret[2]=4;
    ret[3]=5;
    ret[4]=5;
    ret[5]=4;
    ret[6]=5;
    ret[7]=5;
    ret[8]=5;
    ret[9]=4;
    ret[10]=5;
    ret[11]=5;
    ret[12]=5;
    ret[13]=5;
    ret[14]=4;
    ret[15]=5;
    ret[16]=5;
    ret[17]=5;
    ret[18]=5;
    ret[19]=5;
    ret[20]=4;
    ret[21]=5;
    ret[22]=5;
    ret[23]=5;
    ret[24]=5;
    ret[25]=5;
    ret[26]=5;
    ret[27]=4;
    ret[28]=5;
    ret[29]=5;
    ret[30]=5;
    ret[31]=5;
    ret[32]=5;
    ret[33]=5;
    ret[34]=5;
    ret[35]=4;
    return;  
}

if (*_tn == -2){
    ret[0] = 36;
    return;
  }
  else if (*_tn < -38 || *_tn > 36){
    ret[0] = NA_REAL;ret[1] = -1; //error("d(Omega^-1) derivative outside bounds");
  }
  else if (*length_theta != 36){
    ret[0] = NA_REAL;ret[1] = *length_theta;//error("requires vector with 36 arguments");
  }
    if (*_tn == 0){
      ret[0] = (_t[0]);
      ret[8] = _t[1];
      ret[9] = (_t[2]);
      ret[16] = _t[3];
      ret[17] = _t[4];
      ret[18] = (_t[5]);
      ret[24] = _t[6];
      ret[25] = _t[7];
      ret[26] = _t[8];
      ret[27] = (_t[9]);
      ret[32] = _t[10];
      ret[33] = _t[11];
      ret[34] = _t[12];
      ret[35] = _t[13];
      ret[36] = (_t[14]);
      ret[40] = _t[15];
      ret[41] = _t[16];
      ret[42] = _t[17];
      ret[43] = _t[18];
      ret[44] = _t[19];
      ret[45] = (_t[20]);
      ret[48] = _t[21];
      ret[49] = _t[22];
      ret[50] = _t[23];
      ret[51] = _t[24];
      ret[52] = _t[25];
      ret[53] = _t[26];
      ret[54] = (_t[27]);
      ret[56] = _t[28];
      ret[57] = _t[29];
      ret[58] = _t[30];
      ret[59] = _t[31];
      ret[60] = _t[32];
      ret[61] = _t[33];
      ret[62] = _t[34];
      ret[63] = (_t[35]);
    }
    else if (*_tn == -1){
      ret[0] = pow(_t[0], 2);
      ret[1] = _t[1]*_t[0];
      ret[2] = _t[0]*_t[3];
      ret[3] = _t[6]*_t[0];
      ret[4] = _t[0]*_t[10];
      ret[5] = _t[0]*_t[15];
      ret[6] = _t[0]*_t[21];
      ret[7] = _t[0]*_t[28];
      ret[8] = _t[1]*_t[0];
      ret[9] = pow(_t[1], 2)+pow(_t[2], 2);
      ret[10] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[11] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[12] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[13] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[14] = _t[1]*_t[21]+_t[2]*_t[22];
      ret[15] = _t[1]*_t[28]+_t[2]*_t[29];
      ret[16] = _t[0]*_t[3];
      ret[17] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[18] = pow(_t[3], 2)+pow(_t[4], 2)+pow(_t[5], 2);
      ret[19] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[20] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[21] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[22] = _t[3]*_t[21]+_t[4]*_t[22]+_t[5]*_t[23];
      ret[23] = _t[3]*_t[28]+_t[4]*_t[29]+_t[5]*_t[30];
      ret[24] = _t[6]*_t[0];
      ret[25] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[26] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[27] = pow(_t[6], 2)+pow(_t[7], 2)+pow(_t[8], 2)+pow(_t[9], 2);
      ret[28] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[29] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[30] = _t[6]*_t[21]+_t[7]*_t[22]+_t[8]*_t[23]+_t[9]*_t[24];
      ret[31] = _t[6]*_t[28]+_t[7]*_t[29]+_t[8]*_t[30]+_t[9]*_t[31];
      ret[32] = _t[0]*_t[10];
      ret[33] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[34] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[35] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[36] = pow(_t[10], 2)+pow(_t[11], 2)+pow(_t[12], 2)+pow(_t[13], 2)+pow(_t[14], 2);
      ret[37] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[38] = _t[21]*_t[10]+_t[22]*_t[11]+_t[23]*_t[12]+_t[24]*_t[13]+_t[25]*_t[14];
      ret[39] = _t[28]*_t[10]+_t[29]*_t[11]+_t[30]*_t[12]+_t[31]*_t[13]+_t[32]*_t[14];
      ret[40] = _t[0]*_t[15];
      ret[41] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[42] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[43] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[44] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[45] = pow(_t[15], 2)+pow(_t[16], 2)+pow(_t[17], 2)+pow(_t[18], 2)+pow(_t[19], 2)+pow(_t[20], 2);
      ret[46] = _t[21]*_t[15]+_t[22]*_t[16]+_t[23]*_t[17]+_t[24]*_t[18]+_t[25]*_t[19]+_t[26]*_t[20];
      ret[47] = _t[20]*_t[33]+_t[28]*_t[15]+_t[29]*_t[16]+_t[30]*_t[17]+_t[31]*_t[18]+_t[32]*_t[19];
      ret[48] = _t[0]*_t[21];
      ret[49] = _t[1]*_t[21]+_t[2]*_t[22];
      ret[50] = _t[3]*_t[21]+_t[4]*_t[22]+_t[5]*_t[23];
      ret[51] = _t[6]*_t[21]+_t[7]*_t[22]+_t[8]*_t[23]+_t[9]*_t[24];
      ret[52] = _t[21]*_t[10]+_t[22]*_t[11]+_t[23]*_t[12]+_t[24]*_t[13]+_t[25]*_t[14];
      ret[53] = _t[21]*_t[15]+_t[22]*_t[16]+_t[23]*_t[17]+_t[24]*_t[18]+_t[25]*_t[19]+_t[26]*_t[20];
      ret[54] = pow(_t[21], 2)+pow(_t[22], 2)+pow(_t[23], 2)+pow(_t[24], 2)+pow(_t[25], 2)+pow(_t[26], 2)+pow(_t[27], 2);
      ret[55] = _t[21]*_t[28]+_t[23]*_t[30]+_t[24]*_t[31]+_t[25]*_t[32]+_t[26]*_t[33]+_t[27]*_t[34]+_t[29]*_t[22];
      ret[56] = _t[0]*_t[28];
      ret[57] = _t[1]*_t[28]+_t[2]*_t[29];
      ret[58] = _t[3]*_t[28]+_t[4]*_t[29]+_t[5]*_t[30];
      ret[59] = _t[6]*_t[28]+_t[7]*_t[29]+_t[8]*_t[30]+_t[9]*_t[31];
      ret[60] = _t[28]*_t[10]+_t[29]*_t[11]+_t[30]*_t[12]+_t[31]*_t[13]+_t[32]*_t[14];
      ret[61] = _t[20]*_t[33]+_t[28]*_t[15]+_t[29]*_t[16]+_t[30]*_t[17]+_t[31]*_t[18]+_t[32]*_t[19];
      ret[62] = _t[21]*_t[28]+_t[23]*_t[30]+_t[24]*_t[31]+_t[25]*_t[32]+_t[26]*_t[33]+_t[27]*_t[34]+_t[29]*_t[22];
      ret[63] = pow(_t[28], 2)+pow(_t[29], 2)+pow(_t[30], 2)+pow(_t[31], 2)+pow(_t[32], 2)+pow(_t[33], 2)+pow(_t[34], 2)+pow(_t[35], 2);
    }
    else if (*_tn == 1){
      ret[0] = 2*_t[0];
      ret[1] = _t[1];
      ret[2] = _t[3];
      ret[3] = _t[6];
      ret[4] = _t[10];
      ret[5] = _t[15];
      ret[6] = _t[21];
      ret[7] = _t[28];
      ret[8] = _t[1];
      ret[16] = _t[3];
      ret[24] = _t[6];
      ret[32] = _t[10];
      ret[40] = _t[15];
      ret[48] = _t[21];
      ret[56] = _t[28];
    }
    else if (*_tn == 2){
      ret[1] = _t[0];
      ret[8] = _t[0];
      ret[9] = 2*_t[1];
      ret[10] = _t[3];
      ret[11] = _t[6];
      ret[12] = _t[10];
      ret[13] = _t[15];
      ret[14] = _t[21];
      ret[15] = _t[28];
      ret[17] = _t[3];
      ret[25] = _t[6];
      ret[33] = _t[10];
      ret[41] = _t[15];
      ret[49] = _t[21];
      ret[57] = _t[28];
    }
    else if (*_tn == 3){
      ret[9] = 2*_t[2];
      ret[10] = _t[4];
      ret[11] = _t[7];
      ret[12] = _t[11];
      ret[13] = _t[16];
      ret[14] = _t[22];
      ret[15] = _t[29];
      ret[17] = _t[4];
      ret[25] = _t[7];
      ret[33] = _t[11];
      ret[41] = _t[16];
      ret[49] = _t[22];
      ret[57] = _t[29];
    }
    else if (*_tn == 4){
      ret[2] = _t[0];
      ret[10] = _t[1];
      ret[16] = _t[0];
      ret[17] = _t[1];
      ret[18] = 2*_t[3];
      ret[19] = _t[6];
      ret[20] = _t[10];
      ret[21] = _t[15];
      ret[22] = _t[21];
      ret[23] = _t[28];
      ret[26] = _t[6];
      ret[34] = _t[10];
      ret[42] = _t[15];
      ret[50] = _t[21];
      ret[58] = _t[28];
    }
    else if (*_tn == 5){
      ret[10] = _t[2];
      ret[17] = _t[2];
      ret[18] = 2*_t[4];
      ret[19] = _t[7];
      ret[20] = _t[11];
      ret[21] = _t[16];
      ret[22] = _t[22];
      ret[23] = _t[29];
      ret[26] = _t[7];
      ret[34] = _t[11];
      ret[42] = _t[16];
      ret[50] = _t[22];
      ret[58] = _t[29];
    }
    else if (*_tn == 6){
      ret[18] = 2*_t[5];
      ret[19] = _t[8];
      ret[20] = _t[12];
      ret[21] = _t[17];
      ret[22] = _t[23];
      ret[23] = _t[30];
      ret[26] = _t[8];
      ret[34] = _t[12];
      ret[42] = _t[17];
      ret[50] = _t[23];
      ret[58] = _t[30];
    }
    else if (*_tn == 7){
      ret[3] = _t[0];
      ret[11] = _t[1];
      ret[19] = _t[3];
      ret[24] = _t[0];
      ret[25] = _t[1];
      ret[26] = _t[3];
      ret[27] = 2*_t[6];
      ret[28] = _t[10];
      ret[29] = _t[15];
      ret[30] = _t[21];
      ret[31] = _t[28];
      ret[35] = _t[10];
      ret[43] = _t[15];
      ret[51] = _t[21];
      ret[59] = _t[28];
    }
    else if (*_tn == 8){
      ret[11] = _t[2];
      ret[19] = _t[4];
      ret[25] = _t[2];
      ret[26] = _t[4];
      ret[27] = 2*_t[7];
      ret[28] = _t[11];
      ret[29] = _t[16];
      ret[30] = _t[22];
      ret[31] = _t[29];
      ret[35] = _t[11];
      ret[43] = _t[16];
      ret[51] = _t[22];
      ret[59] = _t[29];
    }
    else if (*_tn == 9){
      ret[19] = _t[5];
      ret[26] = _t[5];
      ret[27] = 2*_t[8];
      ret[28] = _t[12];
      ret[29] = _t[17];
      ret[30] = _t[23];
      ret[31] = _t[30];
      ret[35] = _t[12];
      ret[43] = _t[17];
      ret[51] = _t[23];
      ret[59] = _t[30];
    }
    else if (*_tn == 10){
      ret[27] = 2*_t[9];
      ret[28] = _t[13];
      ret[29] = _t[18];
      ret[30] = _t[24];
      ret[31] = _t[31];
      ret[35] = _t[13];
      ret[43] = _t[18];
      ret[51] = _t[24];
      ret[59] = _t[31];
    }
    else if (*_tn == 11){
      ret[4] = _t[0];
      ret[12] = _t[1];
      ret[20] = _t[3];
      ret[28] = _t[6];
      ret[32] = _t[0];
      ret[33] = _t[1];
      ret[34] = _t[3];
      ret[35] = _t[6];
      ret[36] = 2*_t[10];
      ret[37] = _t[15];
      ret[38] = _t[21];
      ret[39] = _t[28];
      ret[44] = _t[15];
      ret[52] = _t[21];
      ret[60] = _t[28];
    }
    else if (*_tn == 12){
      ret[12] = _t[2];
      ret[20] = _t[4];
      ret[28] = _t[7];
      ret[33] = _t[2];
      ret[34] = _t[4];
      ret[35] = _t[7];
      ret[36] = 2*_t[11];
      ret[37] = _t[16];
      ret[38] = _t[22];
      ret[39] = _t[29];
      ret[44] = _t[16];
      ret[52] = _t[22];
      ret[60] = _t[29];
    }
    else if (*_tn == 13){
      ret[20] = _t[5];
      ret[28] = _t[8];
      ret[34] = _t[5];
      ret[35] = _t[8];
      ret[36] = 2*_t[12];
      ret[37] = _t[17];
      ret[38] = _t[23];
      ret[39] = _t[30];
      ret[44] = _t[17];
      ret[52] = _t[23];
      ret[60] = _t[30];
    }
    else if (*_tn == 14){
      ret[28] = _t[9];
      ret[35] = _t[9];
      ret[36] = 2*_t[13];
      ret[37] = _t[18];
      ret[38] = _t[24];
      ret[39] = _t[31];
      ret[44] = _t[18];
      ret[52] = _t[24];
      ret[60] = _t[31];
    }
    else if (*_tn == 15){
      ret[36] = 2*_t[14];
      ret[37] = _t[19];
      ret[38] = _t[25];
      ret[39] = _t[32];
      ret[44] = _t[19];
      ret[52] = _t[25];
      ret[60] = _t[32];
    }
    else if (*_tn == 16){
      ret[5] = _t[0];
      ret[13] = _t[1];
      ret[21] = _t[3];
      ret[29] = _t[6];
      ret[37] = _t[10];
      ret[40] = _t[0];
      ret[41] = _t[1];
      ret[42] = _t[3];
      ret[43] = _t[6];
      ret[44] = _t[10];
      ret[45] = 2*_t[15];
      ret[46] = _t[21];
      ret[47] = _t[28];
      ret[53] = _t[21];
      ret[61] = _t[28];
    }
    else if (*_tn == 17){
      ret[13] = _t[2];
      ret[21] = _t[4];
      ret[29] = _t[7];
      ret[37] = _t[11];
      ret[41] = _t[2];
      ret[42] = _t[4];
      ret[43] = _t[7];
      ret[44] = _t[11];
      ret[45] = 2*_t[16];
      ret[46] = _t[22];
      ret[47] = _t[29];
      ret[53] = _t[22];
      ret[61] = _t[29];
    }
    else if (*_tn == 18){
      ret[21] = _t[5];
      ret[29] = _t[8];
      ret[37] = _t[12];
      ret[42] = _t[5];
      ret[43] = _t[8];
      ret[44] = _t[12];
      ret[45] = 2*_t[17];
      ret[46] = _t[23];
      ret[47] = _t[30];
      ret[53] = _t[23];
      ret[61] = _t[30];
    }
    else if (*_tn == 19){
      ret[29] = _t[9];
      ret[37] = _t[13];
      ret[43] = _t[9];
      ret[44] = _t[13];
      ret[45] = 2*_t[18];
      ret[46] = _t[24];
      ret[47] = _t[31];
      ret[53] = _t[24];
      ret[61] = _t[31];
    }
    else if (*_tn == 20){
      ret[37] = _t[14];
      ret[44] = _t[14];
      ret[45] = 2*_t[19];
      ret[46] = _t[25];
      ret[47] = _t[32];
      ret[53] = _t[25];
      ret[61] = _t[32];
    }
    else if (*_tn == 21){
      ret[45] = 2*_t[20];
      ret[46] = _t[26];
      ret[47] = _t[33];
      ret[53] = _t[26];
      ret[61] = _t[33];
    }
    else if (*_tn == 22){
      ret[6] = _t[0];
      ret[14] = _t[1];
      ret[22] = _t[3];
      ret[30] = _t[6];
      ret[38] = _t[10];
      ret[46] = _t[15];
      ret[48] = _t[0];
      ret[49] = _t[1];
      ret[50] = _t[3];
      ret[51] = _t[6];
      ret[52] = _t[10];
      ret[53] = _t[15];
      ret[54] = 2*_t[21];
      ret[55] = _t[28];
      ret[62] = _t[28];
    }
    else if (*_tn == 23){
      ret[14] = _t[2];
      ret[22] = _t[4];
      ret[30] = _t[7];
      ret[38] = _t[11];
      ret[46] = _t[16];
      ret[49] = _t[2];
      ret[50] = _t[4];
      ret[51] = _t[7];
      ret[52] = _t[11];
      ret[53] = _t[16];
      ret[54] = 2*_t[22];
      ret[55] = _t[29];
      ret[62] = _t[29];
    }
    else if (*_tn == 24){
      ret[22] = _t[5];
      ret[30] = _t[8];
      ret[38] = _t[12];
      ret[46] = _t[17];
      ret[50] = _t[5];
      ret[51] = _t[8];
      ret[52] = _t[12];
      ret[53] = _t[17];
      ret[54] = 2*_t[23];
      ret[55] = _t[30];
      ret[62] = _t[30];
    }
    else if (*_tn == 25){
      ret[30] = _t[9];
      ret[38] = _t[13];
      ret[46] = _t[18];
      ret[51] = _t[9];
      ret[52] = _t[13];
      ret[53] = _t[18];
      ret[54] = 2*_t[24];
      ret[55] = _t[31];
      ret[62] = _t[31];
    }
    else if (*_tn == 26){
      ret[38] = _t[14];
      ret[46] = _t[19];
      ret[52] = _t[14];
      ret[53] = _t[19];
      ret[54] = 2*_t[25];
      ret[55] = _t[32];
      ret[62] = _t[32];
    }
    else if (*_tn == 27){
      ret[46] = _t[20];
      ret[53] = _t[20];
      ret[54] = 2*_t[26];
      ret[55] = _t[33];
      ret[62] = _t[33];
    }
    else if (*_tn == 28){
      ret[54] = 2*_t[27];
      ret[55] = _t[34];
      ret[62] = _t[34];
    }
    else if (*_tn == 29){
      ret[7] = _t[0];
      ret[15] = _t[1];
      ret[23] = _t[3];
      ret[31] = _t[6];
      ret[39] = _t[10];
      ret[47] = _t[15];
      ret[55] = _t[21];
      ret[56] = _t[0];
      ret[57] = _t[1];
      ret[58] = _t[3];
      ret[59] = _t[6];
      ret[60] = _t[10];
      ret[61] = _t[15];
      ret[62] = _t[21];
      ret[63] = 2*_t[28];
    }
    else if (*_tn == 30){
      ret[15] = _t[2];
      ret[23] = _t[4];
      ret[31] = _t[7];
      ret[39] = _t[11];
      ret[47] = _t[16];
      ret[55] = _t[22];
      ret[57] = _t[2];
      ret[58] = _t[4];
      ret[59] = _t[7];
      ret[60] = _t[11];
      ret[61] = _t[16];
      ret[62] = _t[22];
      ret[63] = 2*_t[29];
    }
    else if (*_tn == 31){
      ret[23] = _t[5];
      ret[31] = _t[8];
      ret[39] = _t[12];
      ret[47] = _t[17];
      ret[55] = _t[23];
      ret[58] = _t[5];
      ret[59] = _t[8];
      ret[60] = _t[12];
      ret[61] = _t[17];
      ret[62] = _t[23];
      ret[63] = 2*_t[30];
    }
    else if (*_tn == 32){
      ret[31] = _t[9];
      ret[39] = _t[13];
      ret[47] = _t[18];
      ret[55] = _t[24];
      ret[59] = _t[9];
      ret[60] = _t[13];
      ret[61] = _t[18];
      ret[62] = _t[24];
      ret[63] = 2*_t[31];
    }
    else if (*_tn == 33){
      ret[39] = _t[14];
      ret[47] = _t[19];
      ret[55] = _t[25];
      ret[60] = _t[14];
      ret[61] = _t[19];
      ret[62] = _t[25];
      ret[63] = 2*_t[32];
    }
    else if (*_tn == 34){
      ret[47] = _t[20];
      ret[55] = _t[26];
      ret[61] = _t[20];
      ret[62] = _t[26];
      ret[63] = 2*_t[33];
    }
    else if (*_tn == 35){
      ret[55] = _t[27];
      ret[62] = _t[27];
      ret[63] = 2*_t[34];
    }
    else if (*_tn == 36){
      ret[63] = 2*_t[35];
    }
    return;
  }
else if (*dm == 9) {
  if (*_tn== NA_INTEGER){
    ret[0]=4;
    ret[1]=5;
    ret[2]=4;
    ret[3]=5;
    ret[4]=5;
    ret[5]=4;
    ret[6]=5;
    ret[7]=5;
    ret[8]=5;
    ret[9]=4;
    ret[10]=5;
    ret[11]=5;
    ret[12]=5;
    ret[13]=5;
    ret[14]=4;
    ret[15]=5;
    ret[16]=5;
    ret[17]=5;
    ret[18]=5;
    ret[19]=5;
    ret[20]=4;
    ret[21]=5;
    ret[22]=5;
    ret[23]=5;
    ret[24]=5;
    ret[25]=5;
    ret[26]=5;
    ret[27]=4;
    ret[28]=5;
    ret[29]=5;
    ret[30]=5;
    ret[31]=5;
    ret[32]=5;
    ret[33]=5;
    ret[34]=5;
    ret[35]=4;
    ret[36]=5;
    ret[37]=5;
    ret[38]=5;
    ret[39]=5;
    ret[40]=5;
    ret[41]=5;
    ret[42]=5;
    ret[43]=5;
    ret[44]=4;
    return;  
}

if (*_tn == -2){
    ret[0] = 45;
    return;
  }
  else if (*_tn < -47 || *_tn > 45){
    ret[0] = NA_REAL;ret[1] = -1; //error("d(Omega^-1) derivative outside bounds");
  }
  else if (*length_theta != 45){
    ret[0] = NA_REAL;ret[1] = *length_theta;//error("requires vector with 45 arguments");
  }
    if (*_tn == 0){
      ret[0] = (_t[0]);
      ret[9] = _t[1];
      ret[10] = (_t[2]);
      ret[18] = _t[3];
      ret[19] = _t[4];
      ret[20] = (_t[5]);
      ret[27] = _t[6];
      ret[28] = _t[7];
      ret[29] = _t[8];
      ret[30] = (_t[9]);
      ret[36] = _t[10];
      ret[37] = _t[11];
      ret[38] = _t[12];
      ret[39] = _t[13];
      ret[40] = (_t[14]);
      ret[45] = _t[15];
      ret[46] = _t[16];
      ret[47] = _t[17];
      ret[48] = _t[18];
      ret[49] = _t[19];
      ret[50] = (_t[20]);
      ret[54] = _t[21];
      ret[55] = _t[22];
      ret[56] = _t[23];
      ret[57] = _t[24];
      ret[58] = _t[25];
      ret[59] = _t[26];
      ret[60] = (_t[27]);
      ret[63] = _t[28];
      ret[64] = _t[29];
      ret[65] = _t[30];
      ret[66] = _t[31];
      ret[67] = _t[32];
      ret[68] = _t[33];
      ret[69] = _t[34];
      ret[70] = (_t[35]);
      ret[72] = _t[36];
      ret[73] = _t[37];
      ret[74] = _t[38];
      ret[75] = _t[39];
      ret[76] = _t[40];
      ret[77] = _t[41];
      ret[78] = _t[42];
      ret[79] = _t[43];
      ret[80] = (_t[44]);
    }
    else if (*_tn == -1){
      ret[0] = pow(_t[0], 2);
      ret[1] = _t[1]*_t[0];
      ret[2] = _t[0]*_t[3];
      ret[3] = _t[6]*_t[0];
      ret[4] = _t[0]*_t[10];
      ret[5] = _t[0]*_t[15];
      ret[6] = _t[0]*_t[21];
      ret[7] = _t[0]*_t[28];
      ret[8] = _t[0]*_t[36];
      ret[9] = _t[1]*_t[0];
      ret[10] = pow(_t[1], 2)+pow(_t[2], 2);
      ret[11] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[12] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[13] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[14] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[15] = _t[1]*_t[21]+_t[2]*_t[22];
      ret[16] = _t[1]*_t[28]+_t[2]*_t[29];
      ret[17] = _t[1]*_t[36]+_t[2]*_t[37];
      ret[18] = _t[0]*_t[3];
      ret[19] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[20] = pow(_t[3], 2)+pow(_t[4], 2)+pow(_t[5], 2);
      ret[21] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[22] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[23] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[24] = _t[3]*_t[21]+_t[4]*_t[22]+_t[5]*_t[23];
      ret[25] = _t[3]*_t[28]+_t[4]*_t[29]+_t[5]*_t[30];
      ret[26] = _t[3]*_t[36]+_t[4]*_t[37]+_t[5]*_t[38];
      ret[27] = _t[6]*_t[0];
      ret[28] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[29] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[30] = pow(_t[6], 2)+pow(_t[7], 2)+pow(_t[8], 2)+pow(_t[9], 2);
      ret[31] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[32] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[33] = _t[6]*_t[21]+_t[7]*_t[22]+_t[8]*_t[23]+_t[9]*_t[24];
      ret[34] = _t[6]*_t[28]+_t[7]*_t[29]+_t[8]*_t[30]+_t[9]*_t[31];
      ret[35] = _t[6]*_t[36]+_t[7]*_t[37]+_t[8]*_t[38]+_t[9]*_t[39];
      ret[36] = _t[0]*_t[10];
      ret[37] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[38] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[39] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[40] = pow(_t[10], 2)+pow(_t[11], 2)+pow(_t[12], 2)+pow(_t[13], 2)+pow(_t[14], 2);
      ret[41] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[42] = _t[21]*_t[10]+_t[22]*_t[11]+_t[23]*_t[12]+_t[24]*_t[13]+_t[25]*_t[14];
      ret[43] = _t[28]*_t[10]+_t[29]*_t[11]+_t[30]*_t[12]+_t[31]*_t[13]+_t[32]*_t[14];
      ret[44] = _t[36]*_t[10]+_t[37]*_t[11]+_t[38]*_t[12]+_t[39]*_t[13]+_t[40]*_t[14];
      ret[45] = _t[0]*_t[15];
      ret[46] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[47] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[48] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[49] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[50] = pow(_t[15], 2)+pow(_t[16], 2)+pow(_t[17], 2)+pow(_t[18], 2)+pow(_t[19], 2)+pow(_t[20], 2);
      ret[51] = _t[21]*_t[15]+_t[22]*_t[16]+_t[23]*_t[17]+_t[24]*_t[18]+_t[25]*_t[19]+_t[26]*_t[20];
      ret[52] = _t[20]*_t[33]+_t[28]*_t[15]+_t[29]*_t[16]+_t[30]*_t[17]+_t[31]*_t[18]+_t[32]*_t[19];
      ret[53] = _t[36]*_t[15]+_t[37]*_t[16]+_t[38]*_t[17]+_t[39]*_t[18]+_t[40]*_t[19]+_t[41]*_t[20];
      ret[54] = _t[0]*_t[21];
      ret[55] = _t[1]*_t[21]+_t[2]*_t[22];
      ret[56] = _t[3]*_t[21]+_t[4]*_t[22]+_t[5]*_t[23];
      ret[57] = _t[6]*_t[21]+_t[7]*_t[22]+_t[8]*_t[23]+_t[9]*_t[24];
      ret[58] = _t[21]*_t[10]+_t[22]*_t[11]+_t[23]*_t[12]+_t[24]*_t[13]+_t[25]*_t[14];
      ret[59] = _t[21]*_t[15]+_t[22]*_t[16]+_t[23]*_t[17]+_t[24]*_t[18]+_t[25]*_t[19]+_t[26]*_t[20];
      ret[60] = pow(_t[21], 2)+pow(_t[22], 2)+pow(_t[23], 2)+pow(_t[24], 2)+pow(_t[25], 2)+pow(_t[26], 2)+pow(_t[27], 2);
      ret[61] = _t[21]*_t[28]+_t[23]*_t[30]+_t[24]*_t[31]+_t[25]*_t[32]+_t[26]*_t[33]+_t[27]*_t[34]+_t[29]*_t[22];
      ret[62] = _t[21]*_t[36]+_t[22]*_t[37]+_t[23]*_t[38]+_t[24]*_t[39]+_t[40]*_t[25]+_t[41]*_t[26]+_t[42]*_t[27];
      ret[63] = _t[0]*_t[28];
      ret[64] = _t[1]*_t[28]+_t[2]*_t[29];
      ret[65] = _t[3]*_t[28]+_t[4]*_t[29]+_t[5]*_t[30];
      ret[66] = _t[6]*_t[28]+_t[7]*_t[29]+_t[8]*_t[30]+_t[9]*_t[31];
      ret[67] = _t[28]*_t[10]+_t[29]*_t[11]+_t[30]*_t[12]+_t[31]*_t[13]+_t[32]*_t[14];
      ret[68] = _t[20]*_t[33]+_t[28]*_t[15]+_t[29]*_t[16]+_t[30]*_t[17]+_t[31]*_t[18]+_t[32]*_t[19];
      ret[69] = _t[21]*_t[28]+_t[23]*_t[30]+_t[24]*_t[31]+_t[25]*_t[32]+_t[26]*_t[33]+_t[27]*_t[34]+_t[29]*_t[22];
      ret[70] = pow(_t[28], 2)+pow(_t[29], 2)+pow(_t[30], 2)+pow(_t[31], 2)+pow(_t[32], 2)+pow(_t[33], 2)+pow(_t[34], 2)+pow(_t[35], 2);
      ret[71] = _t[28]*_t[36]+_t[29]*_t[37]+_t[38]*_t[30]+_t[39]*_t[31]+_t[40]*_t[32]+_t[41]*_t[33]+_t[42]*_t[34]+_t[43]*_t[35];
      ret[72] = _t[0]*_t[36];
      ret[73] = _t[1]*_t[36]+_t[2]*_t[37];
      ret[74] = _t[3]*_t[36]+_t[4]*_t[37]+_t[5]*_t[38];
      ret[75] = _t[6]*_t[36]+_t[7]*_t[37]+_t[8]*_t[38]+_t[9]*_t[39];
      ret[76] = _t[36]*_t[10]+_t[37]*_t[11]+_t[38]*_t[12]+_t[39]*_t[13]+_t[40]*_t[14];
      ret[77] = _t[36]*_t[15]+_t[37]*_t[16]+_t[38]*_t[17]+_t[39]*_t[18]+_t[40]*_t[19]+_t[41]*_t[20];
      ret[78] = _t[21]*_t[36]+_t[22]*_t[37]+_t[23]*_t[38]+_t[24]*_t[39]+_t[40]*_t[25]+_t[41]*_t[26]+_t[42]*_t[27];
      ret[79] = _t[28]*_t[36]+_t[29]*_t[37]+_t[38]*_t[30]+_t[39]*_t[31]+_t[40]*_t[32]+_t[41]*_t[33]+_t[42]*_t[34]+_t[43]*_t[35];
      ret[80] = pow(_t[36], 2)+pow(_t[37], 2)+pow(_t[38], 2)+pow(_t[39], 2)+pow(_t[40], 2)+pow(_t[41], 2)+pow(_t[42], 2)+pow(_t[43], 2)+pow(_t[44], 2);
    }
    else if (*_tn == 1){
      ret[0] = 2*_t[0];
      ret[1] = _t[1];
      ret[2] = _t[3];
      ret[3] = _t[6];
      ret[4] = _t[10];
      ret[5] = _t[15];
      ret[6] = _t[21];
      ret[7] = _t[28];
      ret[8] = _t[36];
      ret[9] = _t[1];
      ret[18] = _t[3];
      ret[27] = _t[6];
      ret[36] = _t[10];
      ret[45] = _t[15];
      ret[54] = _t[21];
      ret[63] = _t[28];
      ret[72] = _t[36];
    }
    else if (*_tn == 2){
      ret[1] = _t[0];
      ret[9] = _t[0];
      ret[10] = 2*_t[1];
      ret[11] = _t[3];
      ret[12] = _t[6];
      ret[13] = _t[10];
      ret[14] = _t[15];
      ret[15] = _t[21];
      ret[16] = _t[28];
      ret[17] = _t[36];
      ret[19] = _t[3];
      ret[28] = _t[6];
      ret[37] = _t[10];
      ret[46] = _t[15];
      ret[55] = _t[21];
      ret[64] = _t[28];
      ret[73] = _t[36];
    }
    else if (*_tn == 3){
      ret[10] = 2*_t[2];
      ret[11] = _t[4];
      ret[12] = _t[7];
      ret[13] = _t[11];
      ret[14] = _t[16];
      ret[15] = _t[22];
      ret[16] = _t[29];
      ret[17] = _t[37];
      ret[19] = _t[4];
      ret[28] = _t[7];
      ret[37] = _t[11];
      ret[46] = _t[16];
      ret[55] = _t[22];
      ret[64] = _t[29];
      ret[73] = _t[37];
    }
    else if (*_tn == 4){
      ret[2] = _t[0];
      ret[11] = _t[1];
      ret[18] = _t[0];
      ret[19] = _t[1];
      ret[20] = 2*_t[3];
      ret[21] = _t[6];
      ret[22] = _t[10];
      ret[23] = _t[15];
      ret[24] = _t[21];
      ret[25] = _t[28];
      ret[26] = _t[36];
      ret[29] = _t[6];
      ret[38] = _t[10];
      ret[47] = _t[15];
      ret[56] = _t[21];
      ret[65] = _t[28];
      ret[74] = _t[36];
    }
    else if (*_tn == 5){
      ret[11] = _t[2];
      ret[19] = _t[2];
      ret[20] = 2*_t[4];
      ret[21] = _t[7];
      ret[22] = _t[11];
      ret[23] = _t[16];
      ret[24] = _t[22];
      ret[25] = _t[29];
      ret[26] = _t[37];
      ret[29] = _t[7];
      ret[38] = _t[11];
      ret[47] = _t[16];
      ret[56] = _t[22];
      ret[65] = _t[29];
      ret[74] = _t[37];
    }
    else if (*_tn == 6){
      ret[20] = 2*_t[5];
      ret[21] = _t[8];
      ret[22] = _t[12];
      ret[23] = _t[17];
      ret[24] = _t[23];
      ret[25] = _t[30];
      ret[26] = _t[38];
      ret[29] = _t[8];
      ret[38] = _t[12];
      ret[47] = _t[17];
      ret[56] = _t[23];
      ret[65] = _t[30];
      ret[74] = _t[38];
    }
    else if (*_tn == 7){
      ret[3] = _t[0];
      ret[12] = _t[1];
      ret[21] = _t[3];
      ret[27] = _t[0];
      ret[28] = _t[1];
      ret[29] = _t[3];
      ret[30] = 2*_t[6];
      ret[31] = _t[10];
      ret[32] = _t[15];
      ret[33] = _t[21];
      ret[34] = _t[28];
      ret[35] = _t[36];
      ret[39] = _t[10];
      ret[48] = _t[15];
      ret[57] = _t[21];
      ret[66] = _t[28];
      ret[75] = _t[36];
    }
    else if (*_tn == 8){
      ret[12] = _t[2];
      ret[21] = _t[4];
      ret[28] = _t[2];
      ret[29] = _t[4];
      ret[30] = 2*_t[7];
      ret[31] = _t[11];
      ret[32] = _t[16];
      ret[33] = _t[22];
      ret[34] = _t[29];
      ret[35] = _t[37];
      ret[39] = _t[11];
      ret[48] = _t[16];
      ret[57] = _t[22];
      ret[66] = _t[29];
      ret[75] = _t[37];
    }
    else if (*_tn == 9){
      ret[21] = _t[5];
      ret[29] = _t[5];
      ret[30] = 2*_t[8];
      ret[31] = _t[12];
      ret[32] = _t[17];
      ret[33] = _t[23];
      ret[34] = _t[30];
      ret[35] = _t[38];
      ret[39] = _t[12];
      ret[48] = _t[17];
      ret[57] = _t[23];
      ret[66] = _t[30];
      ret[75] = _t[38];
    }
    else if (*_tn == 10){
      ret[30] = 2*_t[9];
      ret[31] = _t[13];
      ret[32] = _t[18];
      ret[33] = _t[24];
      ret[34] = _t[31];
      ret[35] = _t[39];
      ret[39] = _t[13];
      ret[48] = _t[18];
      ret[57] = _t[24];
      ret[66] = _t[31];
      ret[75] = _t[39];
    }
    else if (*_tn == 11){
      ret[4] = _t[0];
      ret[13] = _t[1];
      ret[22] = _t[3];
      ret[31] = _t[6];
      ret[36] = _t[0];
      ret[37] = _t[1];
      ret[38] = _t[3];
      ret[39] = _t[6];
      ret[40] = 2*_t[10];
      ret[41] = _t[15];
      ret[42] = _t[21];
      ret[43] = _t[28];
      ret[44] = _t[36];
      ret[49] = _t[15];
      ret[58] = _t[21];
      ret[67] = _t[28];
      ret[76] = _t[36];
    }
    else if (*_tn == 12){
      ret[13] = _t[2];
      ret[22] = _t[4];
      ret[31] = _t[7];
      ret[37] = _t[2];
      ret[38] = _t[4];
      ret[39] = _t[7];
      ret[40] = 2*_t[11];
      ret[41] = _t[16];
      ret[42] = _t[22];
      ret[43] = _t[29];
      ret[44] = _t[37];
      ret[49] = _t[16];
      ret[58] = _t[22];
      ret[67] = _t[29];
      ret[76] = _t[37];
    }
    else if (*_tn == 13){
      ret[22] = _t[5];
      ret[31] = _t[8];
      ret[38] = _t[5];
      ret[39] = _t[8];
      ret[40] = 2*_t[12];
      ret[41] = _t[17];
      ret[42] = _t[23];
      ret[43] = _t[30];
      ret[44] = _t[38];
      ret[49] = _t[17];
      ret[58] = _t[23];
      ret[67] = _t[30];
      ret[76] = _t[38];
    }
    else if (*_tn == 14){
      ret[31] = _t[9];
      ret[39] = _t[9];
      ret[40] = 2*_t[13];
      ret[41] = _t[18];
      ret[42] = _t[24];
      ret[43] = _t[31];
      ret[44] = _t[39];
      ret[49] = _t[18];
      ret[58] = _t[24];
      ret[67] = _t[31];
      ret[76] = _t[39];
    }
    else if (*_tn == 15){
      ret[40] = 2*_t[14];
      ret[41] = _t[19];
      ret[42] = _t[25];
      ret[43] = _t[32];
      ret[44] = _t[40];
      ret[49] = _t[19];
      ret[58] = _t[25];
      ret[67] = _t[32];
      ret[76] = _t[40];
    }
    else if (*_tn == 16){
      ret[5] = _t[0];
      ret[14] = _t[1];
      ret[23] = _t[3];
      ret[32] = _t[6];
      ret[41] = _t[10];
      ret[45] = _t[0];
      ret[46] = _t[1];
      ret[47] = _t[3];
      ret[48] = _t[6];
      ret[49] = _t[10];
      ret[50] = 2*_t[15];
      ret[51] = _t[21];
      ret[52] = _t[28];
      ret[53] = _t[36];
      ret[59] = _t[21];
      ret[68] = _t[28];
      ret[77] = _t[36];
    }
    else if (*_tn == 17){
      ret[14] = _t[2];
      ret[23] = _t[4];
      ret[32] = _t[7];
      ret[41] = _t[11];
      ret[46] = _t[2];
      ret[47] = _t[4];
      ret[48] = _t[7];
      ret[49] = _t[11];
      ret[50] = 2*_t[16];
      ret[51] = _t[22];
      ret[52] = _t[29];
      ret[53] = _t[37];
      ret[59] = _t[22];
      ret[68] = _t[29];
      ret[77] = _t[37];
    }
    else if (*_tn == 18){
      ret[23] = _t[5];
      ret[32] = _t[8];
      ret[41] = _t[12];
      ret[47] = _t[5];
      ret[48] = _t[8];
      ret[49] = _t[12];
      ret[50] = 2*_t[17];
      ret[51] = _t[23];
      ret[52] = _t[30];
      ret[53] = _t[38];
      ret[59] = _t[23];
      ret[68] = _t[30];
      ret[77] = _t[38];
    }
    else if (*_tn == 19){
      ret[32] = _t[9];
      ret[41] = _t[13];
      ret[48] = _t[9];
      ret[49] = _t[13];
      ret[50] = 2*_t[18];
      ret[51] = _t[24];
      ret[52] = _t[31];
      ret[53] = _t[39];
      ret[59] = _t[24];
      ret[68] = _t[31];
      ret[77] = _t[39];
    }
    else if (*_tn == 20){
      ret[41] = _t[14];
      ret[49] = _t[14];
      ret[50] = 2*_t[19];
      ret[51] = _t[25];
      ret[52] = _t[32];
      ret[53] = _t[40];
      ret[59] = _t[25];
      ret[68] = _t[32];
      ret[77] = _t[40];
    }
    else if (*_tn == 21){
      ret[50] = 2*_t[20];
      ret[51] = _t[26];
      ret[52] = _t[33];
      ret[53] = _t[41];
      ret[59] = _t[26];
      ret[68] = _t[33];
      ret[77] = _t[41];
    }
    else if (*_tn == 22){
      ret[6] = _t[0];
      ret[15] = _t[1];
      ret[24] = _t[3];
      ret[33] = _t[6];
      ret[42] = _t[10];
      ret[51] = _t[15];
      ret[54] = _t[0];
      ret[55] = _t[1];
      ret[56] = _t[3];
      ret[57] = _t[6];
      ret[58] = _t[10];
      ret[59] = _t[15];
      ret[60] = 2*_t[21];
      ret[61] = _t[28];
      ret[62] = _t[36];
      ret[69] = _t[28];
      ret[78] = _t[36];
    }
    else if (*_tn == 23){
      ret[15] = _t[2];
      ret[24] = _t[4];
      ret[33] = _t[7];
      ret[42] = _t[11];
      ret[51] = _t[16];
      ret[55] = _t[2];
      ret[56] = _t[4];
      ret[57] = _t[7];
      ret[58] = _t[11];
      ret[59] = _t[16];
      ret[60] = 2*_t[22];
      ret[61] = _t[29];
      ret[62] = _t[37];
      ret[69] = _t[29];
      ret[78] = _t[37];
    }
    else if (*_tn == 24){
      ret[24] = _t[5];
      ret[33] = _t[8];
      ret[42] = _t[12];
      ret[51] = _t[17];
      ret[56] = _t[5];
      ret[57] = _t[8];
      ret[58] = _t[12];
      ret[59] = _t[17];
      ret[60] = 2*_t[23];
      ret[61] = _t[30];
      ret[62] = _t[38];
      ret[69] = _t[30];
      ret[78] = _t[38];
    }
    else if (*_tn == 25){
      ret[33] = _t[9];
      ret[42] = _t[13];
      ret[51] = _t[18];
      ret[57] = _t[9];
      ret[58] = _t[13];
      ret[59] = _t[18];
      ret[60] = 2*_t[24];
      ret[61] = _t[31];
      ret[62] = _t[39];
      ret[69] = _t[31];
      ret[78] = _t[39];
    }
    else if (*_tn == 26){
      ret[42] = _t[14];
      ret[51] = _t[19];
      ret[58] = _t[14];
      ret[59] = _t[19];
      ret[60] = 2*_t[25];
      ret[61] = _t[32];
      ret[62] = _t[40];
      ret[69] = _t[32];
      ret[78] = _t[40];
    }
    else if (*_tn == 27){
      ret[51] = _t[20];
      ret[59] = _t[20];
      ret[60] = 2*_t[26];
      ret[61] = _t[33];
      ret[62] = _t[41];
      ret[69] = _t[33];
      ret[78] = _t[41];
    }
    else if (*_tn == 28){
      ret[60] = 2*_t[27];
      ret[61] = _t[34];
      ret[62] = _t[42];
      ret[69] = _t[34];
      ret[78] = _t[42];
    }
    else if (*_tn == 29){
      ret[7] = _t[0];
      ret[16] = _t[1];
      ret[25] = _t[3];
      ret[34] = _t[6];
      ret[43] = _t[10];
      ret[52] = _t[15];
      ret[61] = _t[21];
      ret[63] = _t[0];
      ret[64] = _t[1];
      ret[65] = _t[3];
      ret[66] = _t[6];
      ret[67] = _t[10];
      ret[68] = _t[15];
      ret[69] = _t[21];
      ret[70] = 2*_t[28];
      ret[71] = _t[36];
      ret[79] = _t[36];
    }
    else if (*_tn == 30){
      ret[16] = _t[2];
      ret[25] = _t[4];
      ret[34] = _t[7];
      ret[43] = _t[11];
      ret[52] = _t[16];
      ret[61] = _t[22];
      ret[64] = _t[2];
      ret[65] = _t[4];
      ret[66] = _t[7];
      ret[67] = _t[11];
      ret[68] = _t[16];
      ret[69] = _t[22];
      ret[70] = 2*_t[29];
      ret[71] = _t[37];
      ret[79] = _t[37];
    }
    else if (*_tn == 31){
      ret[25] = _t[5];
      ret[34] = _t[8];
      ret[43] = _t[12];
      ret[52] = _t[17];
      ret[61] = _t[23];
      ret[65] = _t[5];
      ret[66] = _t[8];
      ret[67] = _t[12];
      ret[68] = _t[17];
      ret[69] = _t[23];
      ret[70] = 2*_t[30];
      ret[71] = _t[38];
      ret[79] = _t[38];
    }
    else if (*_tn == 32){
      ret[34] = _t[9];
      ret[43] = _t[13];
      ret[52] = _t[18];
      ret[61] = _t[24];
      ret[66] = _t[9];
      ret[67] = _t[13];
      ret[68] = _t[18];
      ret[69] = _t[24];
      ret[70] = 2*_t[31];
      ret[71] = _t[39];
      ret[79] = _t[39];
    }
    else if (*_tn == 33){
      ret[43] = _t[14];
      ret[52] = _t[19];
      ret[61] = _t[25];
      ret[67] = _t[14];
      ret[68] = _t[19];
      ret[69] = _t[25];
      ret[70] = 2*_t[32];
      ret[71] = _t[40];
      ret[79] = _t[40];
    }
    else if (*_tn == 34){
      ret[52] = _t[20];
      ret[61] = _t[26];
      ret[68] = _t[20];
      ret[69] = _t[26];
      ret[70] = 2*_t[33];
      ret[71] = _t[41];
      ret[79] = _t[41];
    }
    else if (*_tn == 35){
      ret[61] = _t[27];
      ret[69] = _t[27];
      ret[70] = 2*_t[34];
      ret[71] = _t[42];
      ret[79] = _t[42];
    }
    else if (*_tn == 36){
      ret[70] = 2*_t[35];
      ret[71] = _t[43];
      ret[79] = _t[43];
    }
    else if (*_tn == 37){
      ret[8] = _t[0];
      ret[17] = _t[1];
      ret[26] = _t[3];
      ret[35] = _t[6];
      ret[44] = _t[10];
      ret[53] = _t[15];
      ret[62] = _t[21];
      ret[71] = _t[28];
      ret[72] = _t[0];
      ret[73] = _t[1];
      ret[74] = _t[3];
      ret[75] = _t[6];
      ret[76] = _t[10];
      ret[77] = _t[15];
      ret[78] = _t[21];
      ret[79] = _t[28];
      ret[80] = 2*_t[36];
    }
    else if (*_tn == 38){
      ret[17] = _t[2];
      ret[26] = _t[4];
      ret[35] = _t[7];
      ret[44] = _t[11];
      ret[53] = _t[16];
      ret[62] = _t[22];
      ret[71] = _t[29];
      ret[73] = _t[2];
      ret[74] = _t[4];
      ret[75] = _t[7];
      ret[76] = _t[11];
      ret[77] = _t[16];
      ret[78] = _t[22];
      ret[79] = _t[29];
      ret[80] = 2*_t[37];
    }
    else if (*_tn == 39){
      ret[26] = _t[5];
      ret[35] = _t[8];
      ret[44] = _t[12];
      ret[53] = _t[17];
      ret[62] = _t[23];
      ret[71] = _t[30];
      ret[74] = _t[5];
      ret[75] = _t[8];
      ret[76] = _t[12];
      ret[77] = _t[17];
      ret[78] = _t[23];
      ret[79] = _t[30];
      ret[80] = 2*_t[38];
    }
    else if (*_tn == 40){
      ret[35] = _t[9];
      ret[44] = _t[13];
      ret[53] = _t[18];
      ret[62] = _t[24];
      ret[71] = _t[31];
      ret[75] = _t[9];
      ret[76] = _t[13];
      ret[77] = _t[18];
      ret[78] = _t[24];
      ret[79] = _t[31];
      ret[80] = 2*_t[39];
    }
    else if (*_tn == 41){
      ret[44] = _t[14];
      ret[53] = _t[19];
      ret[62] = _t[25];
      ret[71] = _t[32];
      ret[76] = _t[14];
      ret[77] = _t[19];
      ret[78] = _t[25];
      ret[79] = _t[32];
      ret[80] = 2*_t[40];
    }
    else if (*_tn == 42){
      ret[53] = _t[20];
      ret[62] = _t[26];
      ret[71] = _t[33];
      ret[77] = _t[20];
      ret[78] = _t[26];
      ret[79] = _t[33];
      ret[80] = 2*_t[41];
    }
    else if (*_tn == 43){
      ret[62] = _t[27];
      ret[71] = _t[34];
      ret[78] = _t[27];
      ret[79] = _t[34];
      ret[80] = 2*_t[42];
    }
    else if (*_tn == 44){
      ret[71] = _t[35];
      ret[79] = _t[35];
      ret[80] = 2*_t[43];
    }
    else if (*_tn == 45){
      ret[80] = 2*_t[44];
    }
    return;
  }
else if (*dm == 10) {
  if (*_tn== NA_INTEGER){
    ret[0]=4;
    ret[1]=5;
    ret[2]=4;
    ret[3]=5;
    ret[4]=5;
    ret[5]=4;
    ret[6]=5;
    ret[7]=5;
    ret[8]=5;
    ret[9]=4;
    ret[10]=5;
    ret[11]=5;
    ret[12]=5;
    ret[13]=5;
    ret[14]=4;
    ret[15]=5;
    ret[16]=5;
    ret[17]=5;
    ret[18]=5;
    ret[19]=5;
    ret[20]=4;
    ret[21]=5;
    ret[22]=5;
    ret[23]=5;
    ret[24]=5;
    ret[25]=5;
    ret[26]=5;
    ret[27]=4;
    ret[28]=5;
    ret[29]=5;
    ret[30]=5;
    ret[31]=5;
    ret[32]=5;
    ret[33]=5;
    ret[34]=5;
    ret[35]=4;
    ret[36]=5;
    ret[37]=5;
    ret[38]=5;
    ret[39]=5;
    ret[40]=5;
    ret[41]=5;
    ret[42]=5;
    ret[43]=5;
    ret[44]=4;
    ret[45]=5;
    ret[46]=5;
    ret[47]=5;
    ret[48]=5;
    ret[49]=5;
    ret[50]=5;
    ret[51]=5;
    ret[52]=5;
    ret[53]=5;
    ret[54]=4;
    return;  
}

if (*_tn == -2){
    ret[0] = 55;
    return;
  }
  else if (*_tn < -57 || *_tn > 55){
    ret[0] = NA_REAL;ret[1] = -1; //error("d(Omega^-1) derivative outside bounds");
  }
  else if (*length_theta != 55){
    ret[0] = NA_REAL;ret[1] = *length_theta;//error("requires vector with 55 arguments");
  }
    if (*_tn == 0){
      ret[0] = (_t[0]);
      ret[10] = _t[1];
      ret[11] = (_t[2]);
      ret[20] = _t[3];
      ret[21] = _t[4];
      ret[22] = (_t[5]);
      ret[30] = _t[6];
      ret[31] = _t[7];
      ret[32] = _t[8];
      ret[33] = (_t[9]);
      ret[40] = _t[10];
      ret[41] = _t[11];
      ret[42] = _t[12];
      ret[43] = _t[13];
      ret[44] = (_t[14]);
      ret[50] = _t[15];
      ret[51] = _t[16];
      ret[52] = _t[17];
      ret[53] = _t[18];
      ret[54] = _t[19];
      ret[55] = (_t[20]);
      ret[60] = _t[21];
      ret[61] = _t[22];
      ret[62] = _t[23];
      ret[63] = _t[24];
      ret[64] = _t[25];
      ret[65] = _t[26];
      ret[66] = (_t[27]);
      ret[70] = _t[28];
      ret[71] = _t[29];
      ret[72] = _t[30];
      ret[73] = _t[31];
      ret[74] = _t[32];
      ret[75] = _t[33];
      ret[76] = _t[34];
      ret[77] = (_t[35]);
      ret[80] = _t[36];
      ret[81] = _t[37];
      ret[82] = _t[38];
      ret[83] = _t[39];
      ret[84] = _t[40];
      ret[85] = _t[41];
      ret[86] = _t[42];
      ret[87] = _t[43];
      ret[88] = (_t[44]);
      ret[90] = _t[45];
      ret[91] = _t[46];
      ret[92] = _t[47];
      ret[93] = _t[48];
      ret[94] = _t[49];
      ret[95] = _t[50];
      ret[96] = _t[51];
      ret[97] = _t[52];
      ret[98] = _t[53];
      ret[99] = (_t[54]);
    }
    else if (*_tn == -1){
      ret[0] = pow(_t[0], 2);
      ret[1] = _t[1]*_t[0];
      ret[2] = _t[0]*_t[3];
      ret[3] = _t[6]*_t[0];
      ret[4] = _t[0]*_t[10];
      ret[5] = _t[0]*_t[15];
      ret[6] = _t[0]*_t[21];
      ret[7] = _t[0]*_t[28];
      ret[8] = _t[0]*_t[36];
      ret[9] = _t[0]*_t[45];
      ret[10] = _t[1]*_t[0];
      ret[11] = pow(_t[1], 2)+pow(_t[2], 2);
      ret[12] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[13] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[14] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[15] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[16] = _t[1]*_t[21]+_t[2]*_t[22];
      ret[17] = _t[1]*_t[28]+_t[2]*_t[29];
      ret[18] = _t[1]*_t[36]+_t[2]*_t[37];
      ret[19] = _t[1]*_t[45]+_t[2]*_t[46];
      ret[20] = _t[0]*_t[3];
      ret[21] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[22] = pow(_t[3], 2)+pow(_t[4], 2)+pow(_t[5], 2);
      ret[23] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[24] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[25] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[26] = _t[3]*_t[21]+_t[4]*_t[22]+_t[5]*_t[23];
      ret[27] = _t[3]*_t[28]+_t[4]*_t[29]+_t[5]*_t[30];
      ret[28] = _t[3]*_t[36]+_t[4]*_t[37]+_t[5]*_t[38];
      ret[29] = _t[3]*_t[45]+_t[4]*_t[46]+_t[5]*_t[47];
      ret[30] = _t[6]*_t[0];
      ret[31] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[32] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[33] = pow(_t[6], 2)+pow(_t[7], 2)+pow(_t[8], 2)+pow(_t[9], 2);
      ret[34] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[35] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[36] = _t[6]*_t[21]+_t[7]*_t[22]+_t[8]*_t[23]+_t[9]*_t[24];
      ret[37] = _t[6]*_t[28]+_t[7]*_t[29]+_t[8]*_t[30]+_t[9]*_t[31];
      ret[38] = _t[6]*_t[36]+_t[7]*_t[37]+_t[8]*_t[38]+_t[9]*_t[39];
      ret[39] = _t[6]*_t[45]+_t[7]*_t[46]+_t[8]*_t[47]+_t[9]*_t[48];
      ret[40] = _t[0]*_t[10];
      ret[41] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[42] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[43] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[44] = pow(_t[10], 2)+pow(_t[11], 2)+pow(_t[12], 2)+pow(_t[13], 2)+pow(_t[14], 2);
      ret[45] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[46] = _t[21]*_t[10]+_t[22]*_t[11]+_t[23]*_t[12]+_t[24]*_t[13]+_t[25]*_t[14];
      ret[47] = _t[28]*_t[10]+_t[29]*_t[11]+_t[30]*_t[12]+_t[31]*_t[13]+_t[32]*_t[14];
      ret[48] = _t[36]*_t[10]+_t[37]*_t[11]+_t[38]*_t[12]+_t[39]*_t[13]+_t[40]*_t[14];
      ret[49] = _t[45]*_t[10]+_t[46]*_t[11]+_t[47]*_t[12]+_t[48]*_t[13]+_t[49]*_t[14];
      ret[50] = _t[0]*_t[15];
      ret[51] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[52] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[53] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[54] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[55] = pow(_t[15], 2)+pow(_t[16], 2)+pow(_t[17], 2)+pow(_t[18], 2)+pow(_t[19], 2)+pow(_t[20], 2);
      ret[56] = _t[21]*_t[15]+_t[22]*_t[16]+_t[23]*_t[17]+_t[24]*_t[18]+_t[25]*_t[19]+_t[26]*_t[20];
      ret[57] = _t[20]*_t[33]+_t[28]*_t[15]+_t[29]*_t[16]+_t[30]*_t[17]+_t[31]*_t[18]+_t[32]*_t[19];
      ret[58] = _t[36]*_t[15]+_t[37]*_t[16]+_t[38]*_t[17]+_t[39]*_t[18]+_t[40]*_t[19]+_t[41]*_t[20];
      ret[59] = _t[45]*_t[15]+_t[46]*_t[16]+_t[47]*_t[17]+_t[48]*_t[18]+_t[49]*_t[19]+_t[50]*_t[20];
      ret[60] = _t[0]*_t[21];
      ret[61] = _t[1]*_t[21]+_t[2]*_t[22];
      ret[62] = _t[3]*_t[21]+_t[4]*_t[22]+_t[5]*_t[23];
      ret[63] = _t[6]*_t[21]+_t[7]*_t[22]+_t[8]*_t[23]+_t[9]*_t[24];
      ret[64] = _t[21]*_t[10]+_t[22]*_t[11]+_t[23]*_t[12]+_t[24]*_t[13]+_t[25]*_t[14];
      ret[65] = _t[21]*_t[15]+_t[22]*_t[16]+_t[23]*_t[17]+_t[24]*_t[18]+_t[25]*_t[19]+_t[26]*_t[20];
      ret[66] = pow(_t[21], 2)+pow(_t[22], 2)+pow(_t[23], 2)+pow(_t[24], 2)+pow(_t[25], 2)+pow(_t[26], 2)+pow(_t[27], 2);
      ret[67] = _t[21]*_t[28]+_t[23]*_t[30]+_t[24]*_t[31]+_t[25]*_t[32]+_t[26]*_t[33]+_t[27]*_t[34]+_t[29]*_t[22];
      ret[68] = _t[21]*_t[36]+_t[22]*_t[37]+_t[23]*_t[38]+_t[24]*_t[39]+_t[40]*_t[25]+_t[41]*_t[26]+_t[42]*_t[27];
      ret[69] = _t[45]*_t[21]+_t[46]*_t[22]+_t[47]*_t[23]+_t[48]*_t[24]+_t[49]*_t[25]+_t[50]*_t[26]+_t[51]*_t[27];
      ret[70] = _t[0]*_t[28];
      ret[71] = _t[1]*_t[28]+_t[2]*_t[29];
      ret[72] = _t[3]*_t[28]+_t[4]*_t[29]+_t[5]*_t[30];
      ret[73] = _t[6]*_t[28]+_t[7]*_t[29]+_t[8]*_t[30]+_t[9]*_t[31];
      ret[74] = _t[28]*_t[10]+_t[29]*_t[11]+_t[30]*_t[12]+_t[31]*_t[13]+_t[32]*_t[14];
      ret[75] = _t[20]*_t[33]+_t[28]*_t[15]+_t[29]*_t[16]+_t[30]*_t[17]+_t[31]*_t[18]+_t[32]*_t[19];
      ret[76] = _t[21]*_t[28]+_t[23]*_t[30]+_t[24]*_t[31]+_t[25]*_t[32]+_t[26]*_t[33]+_t[27]*_t[34]+_t[29]*_t[22];
      ret[77] = pow(_t[28], 2)+pow(_t[29], 2)+pow(_t[30], 2)+pow(_t[31], 2)+pow(_t[32], 2)+pow(_t[33], 2)+pow(_t[34], 2)+pow(_t[35], 2);
      ret[78] = _t[28]*_t[36]+_t[29]*_t[37]+_t[38]*_t[30]+_t[39]*_t[31]+_t[40]*_t[32]+_t[41]*_t[33]+_t[42]*_t[34]+_t[43]*_t[35];
      ret[79] = _t[45]*_t[28]+_t[46]*_t[29]+_t[47]*_t[30]+_t[48]*_t[31]+_t[49]*_t[32]+_t[50]*_t[33]+_t[51]*_t[34]+_t[52]*_t[35];
      ret[80] = _t[0]*_t[36];
      ret[81] = _t[1]*_t[36]+_t[2]*_t[37];
      ret[82] = _t[3]*_t[36]+_t[4]*_t[37]+_t[5]*_t[38];
      ret[83] = _t[6]*_t[36]+_t[7]*_t[37]+_t[8]*_t[38]+_t[9]*_t[39];
      ret[84] = _t[36]*_t[10]+_t[37]*_t[11]+_t[38]*_t[12]+_t[39]*_t[13]+_t[40]*_t[14];
      ret[85] = _t[36]*_t[15]+_t[37]*_t[16]+_t[38]*_t[17]+_t[39]*_t[18]+_t[40]*_t[19]+_t[41]*_t[20];
      ret[86] = _t[21]*_t[36]+_t[22]*_t[37]+_t[23]*_t[38]+_t[24]*_t[39]+_t[40]*_t[25]+_t[41]*_t[26]+_t[42]*_t[27];
      ret[87] = _t[28]*_t[36]+_t[29]*_t[37]+_t[38]*_t[30]+_t[39]*_t[31]+_t[40]*_t[32]+_t[41]*_t[33]+_t[42]*_t[34]+_t[43]*_t[35];
      ret[88] = pow(_t[36], 2)+pow(_t[37], 2)+pow(_t[38], 2)+pow(_t[39], 2)+pow(_t[40], 2)+pow(_t[41], 2)+pow(_t[42], 2)+pow(_t[43], 2)+pow(_t[44], 2);
      ret[89] = _t[41]*_t[50]+_t[42]*_t[51]+_t[43]*_t[52]+_t[44]*_t[53]+_t[45]*_t[36]+_t[46]*_t[37]+_t[47]*_t[38]+_t[48]*_t[39]+_t[49]*_t[40];
      ret[90] = _t[0]*_t[45];
      ret[91] = _t[1]*_t[45]+_t[2]*_t[46];
      ret[92] = _t[3]*_t[45]+_t[4]*_t[46]+_t[5]*_t[47];
      ret[93] = _t[6]*_t[45]+_t[7]*_t[46]+_t[8]*_t[47]+_t[9]*_t[48];
      ret[94] = _t[45]*_t[10]+_t[46]*_t[11]+_t[47]*_t[12]+_t[48]*_t[13]+_t[49]*_t[14];
      ret[95] = _t[45]*_t[15]+_t[46]*_t[16]+_t[47]*_t[17]+_t[48]*_t[18]+_t[49]*_t[19]+_t[50]*_t[20];
      ret[96] = _t[45]*_t[21]+_t[46]*_t[22]+_t[47]*_t[23]+_t[48]*_t[24]+_t[49]*_t[25]+_t[50]*_t[26]+_t[51]*_t[27];
      ret[97] = _t[45]*_t[28]+_t[46]*_t[29]+_t[47]*_t[30]+_t[48]*_t[31]+_t[49]*_t[32]+_t[50]*_t[33]+_t[51]*_t[34]+_t[52]*_t[35];
      ret[98] = _t[41]*_t[50]+_t[42]*_t[51]+_t[43]*_t[52]+_t[44]*_t[53]+_t[45]*_t[36]+_t[46]*_t[37]+_t[47]*_t[38]+_t[48]*_t[39]+_t[49]*_t[40];
      ret[99] = pow(_t[45], 2)+pow(_t[46], 2)+pow(_t[47], 2)+pow(_t[48], 2)+pow(_t[49], 2)+pow(_t[50], 2)+pow(_t[51], 2)+pow(_t[52], 2)+pow(_t[53], 2)+pow(_t[54], 2);
    }
    else if (*_tn == 1){
      ret[0] = 2*_t[0];
      ret[1] = _t[1];
      ret[2] = _t[3];
      ret[3] = _t[6];
      ret[4] = _t[10];
      ret[5] = _t[15];
      ret[6] = _t[21];
      ret[7] = _t[28];
      ret[8] = _t[36];
      ret[9] = _t[45];
      ret[10] = _t[1];
      ret[20] = _t[3];
      ret[30] = _t[6];
      ret[40] = _t[10];
      ret[50] = _t[15];
      ret[60] = _t[21];
      ret[70] = _t[28];
      ret[80] = _t[36];
      ret[90] = _t[45];
    }
    else if (*_tn == 2){
      ret[1] = _t[0];
      ret[10] = _t[0];
      ret[11] = 2*_t[1];
      ret[12] = _t[3];
      ret[13] = _t[6];
      ret[14] = _t[10];
      ret[15] = _t[15];
      ret[16] = _t[21];
      ret[17] = _t[28];
      ret[18] = _t[36];
      ret[19] = _t[45];
      ret[21] = _t[3];
      ret[31] = _t[6];
      ret[41] = _t[10];
      ret[51] = _t[15];
      ret[61] = _t[21];
      ret[71] = _t[28];
      ret[81] = _t[36];
      ret[91] = _t[45];
    }
    else if (*_tn == 3){
      ret[11] = 2*_t[2];
      ret[12] = _t[4];
      ret[13] = _t[7];
      ret[14] = _t[11];
      ret[15] = _t[16];
      ret[16] = _t[22];
      ret[17] = _t[29];
      ret[18] = _t[37];
      ret[19] = _t[46];
      ret[21] = _t[4];
      ret[31] = _t[7];
      ret[41] = _t[11];
      ret[51] = _t[16];
      ret[61] = _t[22];
      ret[71] = _t[29];
      ret[81] = _t[37];
      ret[91] = _t[46];
    }
    else if (*_tn == 4){
      ret[2] = _t[0];
      ret[12] = _t[1];
      ret[20] = _t[0];
      ret[21] = _t[1];
      ret[22] = 2*_t[3];
      ret[23] = _t[6];
      ret[24] = _t[10];
      ret[25] = _t[15];
      ret[26] = _t[21];
      ret[27] = _t[28];
      ret[28] = _t[36];
      ret[29] = _t[45];
      ret[32] = _t[6];
      ret[42] = _t[10];
      ret[52] = _t[15];
      ret[62] = _t[21];
      ret[72] = _t[28];
      ret[82] = _t[36];
      ret[92] = _t[45];
    }
    else if (*_tn == 5){
      ret[12] = _t[2];
      ret[21] = _t[2];
      ret[22] = 2*_t[4];
      ret[23] = _t[7];
      ret[24] = _t[11];
      ret[25] = _t[16];
      ret[26] = _t[22];
      ret[27] = _t[29];
      ret[28] = _t[37];
      ret[29] = _t[46];
      ret[32] = _t[7];
      ret[42] = _t[11];
      ret[52] = _t[16];
      ret[62] = _t[22];
      ret[72] = _t[29];
      ret[82] = _t[37];
      ret[92] = _t[46];
    }
    else if (*_tn == 6){
      ret[22] = 2*_t[5];
      ret[23] = _t[8];
      ret[24] = _t[12];
      ret[25] = _t[17];
      ret[26] = _t[23];
      ret[27] = _t[30];
      ret[28] = _t[38];
      ret[29] = _t[47];
      ret[32] = _t[8];
      ret[42] = _t[12];
      ret[52] = _t[17];
      ret[62] = _t[23];
      ret[72] = _t[30];
      ret[82] = _t[38];
      ret[92] = _t[47];
    }
    else if (*_tn == 7){
      ret[3] = _t[0];
      ret[13] = _t[1];
      ret[23] = _t[3];
      ret[30] = _t[0];
      ret[31] = _t[1];
      ret[32] = _t[3];
      ret[33] = 2*_t[6];
      ret[34] = _t[10];
      ret[35] = _t[15];
      ret[36] = _t[21];
      ret[37] = _t[28];
      ret[38] = _t[36];
      ret[39] = _t[45];
      ret[43] = _t[10];
      ret[53] = _t[15];
      ret[63] = _t[21];
      ret[73] = _t[28];
      ret[83] = _t[36];
      ret[93] = _t[45];
    }
    else if (*_tn == 8){
      ret[13] = _t[2];
      ret[23] = _t[4];
      ret[31] = _t[2];
      ret[32] = _t[4];
      ret[33] = 2*_t[7];
      ret[34] = _t[11];
      ret[35] = _t[16];
      ret[36] = _t[22];
      ret[37] = _t[29];
      ret[38] = _t[37];
      ret[39] = _t[46];
      ret[43] = _t[11];
      ret[53] = _t[16];
      ret[63] = _t[22];
      ret[73] = _t[29];
      ret[83] = _t[37];
      ret[93] = _t[46];
    }
    else if (*_tn == 9){
      ret[23] = _t[5];
      ret[32] = _t[5];
      ret[33] = 2*_t[8];
      ret[34] = _t[12];
      ret[35] = _t[17];
      ret[36] = _t[23];
      ret[37] = _t[30];
      ret[38] = _t[38];
      ret[39] = _t[47];
      ret[43] = _t[12];
      ret[53] = _t[17];
      ret[63] = _t[23];
      ret[73] = _t[30];
      ret[83] = _t[38];
      ret[93] = _t[47];
    }
    else if (*_tn == 10){
      ret[33] = 2*_t[9];
      ret[34] = _t[13];
      ret[35] = _t[18];
      ret[36] = _t[24];
      ret[37] = _t[31];
      ret[38] = _t[39];
      ret[39] = _t[48];
      ret[43] = _t[13];
      ret[53] = _t[18];
      ret[63] = _t[24];
      ret[73] = _t[31];
      ret[83] = _t[39];
      ret[93] = _t[48];
    }
    else if (*_tn == 11){
      ret[4] = _t[0];
      ret[14] = _t[1];
      ret[24] = _t[3];
      ret[34] = _t[6];
      ret[40] = _t[0];
      ret[41] = _t[1];
      ret[42] = _t[3];
      ret[43] = _t[6];
      ret[44] = 2*_t[10];
      ret[45] = _t[15];
      ret[46] = _t[21];
      ret[47] = _t[28];
      ret[48] = _t[36];
      ret[49] = _t[45];
      ret[54] = _t[15];
      ret[64] = _t[21];
      ret[74] = _t[28];
      ret[84] = _t[36];
      ret[94] = _t[45];
    }
    else if (*_tn == 12){
      ret[14] = _t[2];
      ret[24] = _t[4];
      ret[34] = _t[7];
      ret[41] = _t[2];
      ret[42] = _t[4];
      ret[43] = _t[7];
      ret[44] = 2*_t[11];
      ret[45] = _t[16];
      ret[46] = _t[22];
      ret[47] = _t[29];
      ret[48] = _t[37];
      ret[49] = _t[46];
      ret[54] = _t[16];
      ret[64] = _t[22];
      ret[74] = _t[29];
      ret[84] = _t[37];
      ret[94] = _t[46];
    }
    else if (*_tn == 13){
      ret[24] = _t[5];
      ret[34] = _t[8];
      ret[42] = _t[5];
      ret[43] = _t[8];
      ret[44] = 2*_t[12];
      ret[45] = _t[17];
      ret[46] = _t[23];
      ret[47] = _t[30];
      ret[48] = _t[38];
      ret[49] = _t[47];
      ret[54] = _t[17];
      ret[64] = _t[23];
      ret[74] = _t[30];
      ret[84] = _t[38];
      ret[94] = _t[47];
    }
    else if (*_tn == 14){
      ret[34] = _t[9];
      ret[43] = _t[9];
      ret[44] = 2*_t[13];
      ret[45] = _t[18];
      ret[46] = _t[24];
      ret[47] = _t[31];
      ret[48] = _t[39];
      ret[49] = _t[48];
      ret[54] = _t[18];
      ret[64] = _t[24];
      ret[74] = _t[31];
      ret[84] = _t[39];
      ret[94] = _t[48];
    }
    else if (*_tn == 15){
      ret[44] = 2*_t[14];
      ret[45] = _t[19];
      ret[46] = _t[25];
      ret[47] = _t[32];
      ret[48] = _t[40];
      ret[49] = _t[49];
      ret[54] = _t[19];
      ret[64] = _t[25];
      ret[74] = _t[32];
      ret[84] = _t[40];
      ret[94] = _t[49];
    }
    else if (*_tn == 16){
      ret[5] = _t[0];
      ret[15] = _t[1];
      ret[25] = _t[3];
      ret[35] = _t[6];
      ret[45] = _t[10];
      ret[50] = _t[0];
      ret[51] = _t[1];
      ret[52] = _t[3];
      ret[53] = _t[6];
      ret[54] = _t[10];
      ret[55] = 2*_t[15];
      ret[56] = _t[21];
      ret[57] = _t[28];
      ret[58] = _t[36];
      ret[59] = _t[45];
      ret[65] = _t[21];
      ret[75] = _t[28];
      ret[85] = _t[36];
      ret[95] = _t[45];
    }
    else if (*_tn == 17){
      ret[15] = _t[2];
      ret[25] = _t[4];
      ret[35] = _t[7];
      ret[45] = _t[11];
      ret[51] = _t[2];
      ret[52] = _t[4];
      ret[53] = _t[7];
      ret[54] = _t[11];
      ret[55] = 2*_t[16];
      ret[56] = _t[22];
      ret[57] = _t[29];
      ret[58] = _t[37];
      ret[59] = _t[46];
      ret[65] = _t[22];
      ret[75] = _t[29];
      ret[85] = _t[37];
      ret[95] = _t[46];
    }
    else if (*_tn == 18){
      ret[25] = _t[5];
      ret[35] = _t[8];
      ret[45] = _t[12];
      ret[52] = _t[5];
      ret[53] = _t[8];
      ret[54] = _t[12];
      ret[55] = 2*_t[17];
      ret[56] = _t[23];
      ret[57] = _t[30];
      ret[58] = _t[38];
      ret[59] = _t[47];
      ret[65] = _t[23];
      ret[75] = _t[30];
      ret[85] = _t[38];
      ret[95] = _t[47];
    }
    else if (*_tn == 19){
      ret[35] = _t[9];
      ret[45] = _t[13];
      ret[53] = _t[9];
      ret[54] = _t[13];
      ret[55] = 2*_t[18];
      ret[56] = _t[24];
      ret[57] = _t[31];
      ret[58] = _t[39];
      ret[59] = _t[48];
      ret[65] = _t[24];
      ret[75] = _t[31];
      ret[85] = _t[39];
      ret[95] = _t[48];
    }
    else if (*_tn == 20){
      ret[45] = _t[14];
      ret[54] = _t[14];
      ret[55] = 2*_t[19];
      ret[56] = _t[25];
      ret[57] = _t[32];
      ret[58] = _t[40];
      ret[59] = _t[49];
      ret[65] = _t[25];
      ret[75] = _t[32];
      ret[85] = _t[40];
      ret[95] = _t[49];
    }
    else if (*_tn == 21){
      ret[55] = 2*_t[20];
      ret[56] = _t[26];
      ret[57] = _t[33];
      ret[58] = _t[41];
      ret[59] = _t[50];
      ret[65] = _t[26];
      ret[75] = _t[33];
      ret[85] = _t[41];
      ret[95] = _t[50];
    }
    else if (*_tn == 22){
      ret[6] = _t[0];
      ret[16] = _t[1];
      ret[26] = _t[3];
      ret[36] = _t[6];
      ret[46] = _t[10];
      ret[56] = _t[15];
      ret[60] = _t[0];
      ret[61] = _t[1];
      ret[62] = _t[3];
      ret[63] = _t[6];
      ret[64] = _t[10];
      ret[65] = _t[15];
      ret[66] = 2*_t[21];
      ret[67] = _t[28];
      ret[68] = _t[36];
      ret[69] = _t[45];
      ret[76] = _t[28];
      ret[86] = _t[36];
      ret[96] = _t[45];
    }
    else if (*_tn == 23){
      ret[16] = _t[2];
      ret[26] = _t[4];
      ret[36] = _t[7];
      ret[46] = _t[11];
      ret[56] = _t[16];
      ret[61] = _t[2];
      ret[62] = _t[4];
      ret[63] = _t[7];
      ret[64] = _t[11];
      ret[65] = _t[16];
      ret[66] = 2*_t[22];
      ret[67] = _t[29];
      ret[68] = _t[37];
      ret[69] = _t[46];
      ret[76] = _t[29];
      ret[86] = _t[37];
      ret[96] = _t[46];
    }
    else if (*_tn == 24){
      ret[26] = _t[5];
      ret[36] = _t[8];
      ret[46] = _t[12];
      ret[56] = _t[17];
      ret[62] = _t[5];
      ret[63] = _t[8];
      ret[64] = _t[12];
      ret[65] = _t[17];
      ret[66] = 2*_t[23];
      ret[67] = _t[30];
      ret[68] = _t[38];
      ret[69] = _t[47];
      ret[76] = _t[30];
      ret[86] = _t[38];
      ret[96] = _t[47];
    }
    else if (*_tn == 25){
      ret[36] = _t[9];
      ret[46] = _t[13];
      ret[56] = _t[18];
      ret[63] = _t[9];
      ret[64] = _t[13];
      ret[65] = _t[18];
      ret[66] = 2*_t[24];
      ret[67] = _t[31];
      ret[68] = _t[39];
      ret[69] = _t[48];
      ret[76] = _t[31];
      ret[86] = _t[39];
      ret[96] = _t[48];
    }
    else if (*_tn == 26){
      ret[46] = _t[14];
      ret[56] = _t[19];
      ret[64] = _t[14];
      ret[65] = _t[19];
      ret[66] = 2*_t[25];
      ret[67] = _t[32];
      ret[68] = _t[40];
      ret[69] = _t[49];
      ret[76] = _t[32];
      ret[86] = _t[40];
      ret[96] = _t[49];
    }
    else if (*_tn == 27){
      ret[56] = _t[20];
      ret[65] = _t[20];
      ret[66] = 2*_t[26];
      ret[67] = _t[33];
      ret[68] = _t[41];
      ret[69] = _t[50];
      ret[76] = _t[33];
      ret[86] = _t[41];
      ret[96] = _t[50];
    }
    else if (*_tn == 28){
      ret[66] = 2*_t[27];
      ret[67] = _t[34];
      ret[68] = _t[42];
      ret[69] = _t[51];
      ret[76] = _t[34];
      ret[86] = _t[42];
      ret[96] = _t[51];
    }
    else if (*_tn == 29){
      ret[7] = _t[0];
      ret[17] = _t[1];
      ret[27] = _t[3];
      ret[37] = _t[6];
      ret[47] = _t[10];
      ret[57] = _t[15];
      ret[67] = _t[21];
      ret[70] = _t[0];
      ret[71] = _t[1];
      ret[72] = _t[3];
      ret[73] = _t[6];
      ret[74] = _t[10];
      ret[75] = _t[15];
      ret[76] = _t[21];
      ret[77] = 2*_t[28];
      ret[78] = _t[36];
      ret[79] = _t[45];
      ret[87] = _t[36];
      ret[97] = _t[45];
    }
    else if (*_tn == 30){
      ret[17] = _t[2];
      ret[27] = _t[4];
      ret[37] = _t[7];
      ret[47] = _t[11];
      ret[57] = _t[16];
      ret[67] = _t[22];
      ret[71] = _t[2];
      ret[72] = _t[4];
      ret[73] = _t[7];
      ret[74] = _t[11];
      ret[75] = _t[16];
      ret[76] = _t[22];
      ret[77] = 2*_t[29];
      ret[78] = _t[37];
      ret[79] = _t[46];
      ret[87] = _t[37];
      ret[97] = _t[46];
    }
    else if (*_tn == 31){
      ret[27] = _t[5];
      ret[37] = _t[8];
      ret[47] = _t[12];
      ret[57] = _t[17];
      ret[67] = _t[23];
      ret[72] = _t[5];
      ret[73] = _t[8];
      ret[74] = _t[12];
      ret[75] = _t[17];
      ret[76] = _t[23];
      ret[77] = 2*_t[30];
      ret[78] = _t[38];
      ret[79] = _t[47];
      ret[87] = _t[38];
      ret[97] = _t[47];
    }
    else if (*_tn == 32){
      ret[37] = _t[9];
      ret[47] = _t[13];
      ret[57] = _t[18];
      ret[67] = _t[24];
      ret[73] = _t[9];
      ret[74] = _t[13];
      ret[75] = _t[18];
      ret[76] = _t[24];
      ret[77] = 2*_t[31];
      ret[78] = _t[39];
      ret[79] = _t[48];
      ret[87] = _t[39];
      ret[97] = _t[48];
    }
    else if (*_tn == 33){
      ret[47] = _t[14];
      ret[57] = _t[19];
      ret[67] = _t[25];
      ret[74] = _t[14];
      ret[75] = _t[19];
      ret[76] = _t[25];
      ret[77] = 2*_t[32];
      ret[78] = _t[40];
      ret[79] = _t[49];
      ret[87] = _t[40];
      ret[97] = _t[49];
    }
    else if (*_tn == 34){
      ret[57] = _t[20];
      ret[67] = _t[26];
      ret[75] = _t[20];
      ret[76] = _t[26];
      ret[77] = 2*_t[33];
      ret[78] = _t[41];
      ret[79] = _t[50];
      ret[87] = _t[41];
      ret[97] = _t[50];
    }
    else if (*_tn == 35){
      ret[67] = _t[27];
      ret[76] = _t[27];
      ret[77] = 2*_t[34];
      ret[78] = _t[42];
      ret[79] = _t[51];
      ret[87] = _t[42];
      ret[97] = _t[51];
    }
    else if (*_tn == 36){
      ret[77] = 2*_t[35];
      ret[78] = _t[43];
      ret[79] = _t[52];
      ret[87] = _t[43];
      ret[97] = _t[52];
    }
    else if (*_tn == 37){
      ret[8] = _t[0];
      ret[18] = _t[1];
      ret[28] = _t[3];
      ret[38] = _t[6];
      ret[48] = _t[10];
      ret[58] = _t[15];
      ret[68] = _t[21];
      ret[78] = _t[28];
      ret[80] = _t[0];
      ret[81] = _t[1];
      ret[82] = _t[3];
      ret[83] = _t[6];
      ret[84] = _t[10];
      ret[85] = _t[15];
      ret[86] = _t[21];
      ret[87] = _t[28];
      ret[88] = 2*_t[36];
      ret[89] = _t[45];
      ret[98] = _t[45];
    }
    else if (*_tn == 38){
      ret[18] = _t[2];
      ret[28] = _t[4];
      ret[38] = _t[7];
      ret[48] = _t[11];
      ret[58] = _t[16];
      ret[68] = _t[22];
      ret[78] = _t[29];
      ret[81] = _t[2];
      ret[82] = _t[4];
      ret[83] = _t[7];
      ret[84] = _t[11];
      ret[85] = _t[16];
      ret[86] = _t[22];
      ret[87] = _t[29];
      ret[88] = 2*_t[37];
      ret[89] = _t[46];
      ret[98] = _t[46];
    }
    else if (*_tn == 39){
      ret[28] = _t[5];
      ret[38] = _t[8];
      ret[48] = _t[12];
      ret[58] = _t[17];
      ret[68] = _t[23];
      ret[78] = _t[30];
      ret[82] = _t[5];
      ret[83] = _t[8];
      ret[84] = _t[12];
      ret[85] = _t[17];
      ret[86] = _t[23];
      ret[87] = _t[30];
      ret[88] = 2*_t[38];
      ret[89] = _t[47];
      ret[98] = _t[47];
    }
    else if (*_tn == 40){
      ret[38] = _t[9];
      ret[48] = _t[13];
      ret[58] = _t[18];
      ret[68] = _t[24];
      ret[78] = _t[31];
      ret[83] = _t[9];
      ret[84] = _t[13];
      ret[85] = _t[18];
      ret[86] = _t[24];
      ret[87] = _t[31];
      ret[88] = 2*_t[39];
      ret[89] = _t[48];
      ret[98] = _t[48];
    }
    else if (*_tn == 41){
      ret[48] = _t[14];
      ret[58] = _t[19];
      ret[68] = _t[25];
      ret[78] = _t[32];
      ret[84] = _t[14];
      ret[85] = _t[19];
      ret[86] = _t[25];
      ret[87] = _t[32];
      ret[88] = 2*_t[40];
      ret[89] = _t[49];
      ret[98] = _t[49];
    }
    else if (*_tn == 42){
      ret[58] = _t[20];
      ret[68] = _t[26];
      ret[78] = _t[33];
      ret[85] = _t[20];
      ret[86] = _t[26];
      ret[87] = _t[33];
      ret[88] = 2*_t[41];
      ret[89] = _t[50];
      ret[98] = _t[50];
    }
    else if (*_tn == 43){
      ret[68] = _t[27];
      ret[78] = _t[34];
      ret[86] = _t[27];
      ret[87] = _t[34];
      ret[88] = 2*_t[42];
      ret[89] = _t[51];
      ret[98] = _t[51];
    }
    else if (*_tn == 44){
      ret[78] = _t[35];
      ret[87] = _t[35];
      ret[88] = 2*_t[43];
      ret[89] = _t[52];
      ret[98] = _t[52];
    }
    else if (*_tn == 45){
      ret[88] = 2*_t[44];
      ret[89] = _t[53];
      ret[98] = _t[53];
    }
    else if (*_tn == 46){
      ret[9] = _t[0];
      ret[19] = _t[1];
      ret[29] = _t[3];
      ret[39] = _t[6];
      ret[49] = _t[10];
      ret[59] = _t[15];
      ret[69] = _t[21];
      ret[79] = _t[28];
      ret[89] = _t[36];
      ret[90] = _t[0];
      ret[91] = _t[1];
      ret[92] = _t[3];
      ret[93] = _t[6];
      ret[94] = _t[10];
      ret[95] = _t[15];
      ret[96] = _t[21];
      ret[97] = _t[28];
      ret[98] = _t[36];
      ret[99] = 2*_t[45];
    }
    else if (*_tn == 47){
      ret[19] = _t[2];
      ret[29] = _t[4];
      ret[39] = _t[7];
      ret[49] = _t[11];
      ret[59] = _t[16];
      ret[69] = _t[22];
      ret[79] = _t[29];
      ret[89] = _t[37];
      ret[91] = _t[2];
      ret[92] = _t[4];
      ret[93] = _t[7];
      ret[94] = _t[11];
      ret[95] = _t[16];
      ret[96] = _t[22];
      ret[97] = _t[29];
      ret[98] = _t[37];
      ret[99] = 2*_t[46];
    }
    else if (*_tn == 48){
      ret[29] = _t[5];
      ret[39] = _t[8];
      ret[49] = _t[12];
      ret[59] = _t[17];
      ret[69] = _t[23];
      ret[79] = _t[30];
      ret[89] = _t[38];
      ret[92] = _t[5];
      ret[93] = _t[8];
      ret[94] = _t[12];
      ret[95] = _t[17];
      ret[96] = _t[23];
      ret[97] = _t[30];
      ret[98] = _t[38];
      ret[99] = 2*_t[47];
    }
    else if (*_tn == 49){
      ret[39] = _t[9];
      ret[49] = _t[13];
      ret[59] = _t[18];
      ret[69] = _t[24];
      ret[79] = _t[31];
      ret[89] = _t[39];
      ret[93] = _t[9];
      ret[94] = _t[13];
      ret[95] = _t[18];
      ret[96] = _t[24];
      ret[97] = _t[31];
      ret[98] = _t[39];
      ret[99] = 2*_t[48];
    }
    else if (*_tn == 50){
      ret[49] = _t[14];
      ret[59] = _t[19];
      ret[69] = _t[25];
      ret[79] = _t[32];
      ret[89] = _t[40];
      ret[94] = _t[14];
      ret[95] = _t[19];
      ret[96] = _t[25];
      ret[97] = _t[32];
      ret[98] = _t[40];
      ret[99] = 2*_t[49];
    }
    else if (*_tn == 51){
      ret[59] = _t[20];
      ret[69] = _t[26];
      ret[79] = _t[33];
      ret[89] = _t[41];
      ret[95] = _t[20];
      ret[96] = _t[26];
      ret[97] = _t[33];
      ret[98] = _t[41];
      ret[99] = 2*_t[50];
    }
    else if (*_tn == 52){
      ret[69] = _t[27];
      ret[79] = _t[34];
      ret[89] = _t[42];
      ret[96] = _t[27];
      ret[97] = _t[34];
      ret[98] = _t[42];
      ret[99] = 2*_t[51];
    }
    else if (*_tn == 53){
      ret[79] = _t[35];
      ret[89] = _t[43];
      ret[97] = _t[35];
      ret[98] = _t[43];
      ret[99] = 2*_t[52];
    }
    else if (*_tn == 54){
      ret[89] = _t[44];
      ret[98] = _t[44];
      ret[99] = 2*_t[53];
    }
    else if (*_tn == 55){
      ret[99] = 2*_t[54];
    }
    return;
  }
else if (*dm == 11) {
  if (*_tn== NA_INTEGER){
    ret[0]=4;
    ret[1]=5;
    ret[2]=4;
    ret[3]=5;
    ret[4]=5;
    ret[5]=4;
    ret[6]=5;
    ret[7]=5;
    ret[8]=5;
    ret[9]=4;
    ret[10]=5;
    ret[11]=5;
    ret[12]=5;
    ret[13]=5;
    ret[14]=4;
    ret[15]=5;
    ret[16]=5;
    ret[17]=5;
    ret[18]=5;
    ret[19]=5;
    ret[20]=4;
    ret[21]=5;
    ret[22]=5;
    ret[23]=5;
    ret[24]=5;
    ret[25]=5;
    ret[26]=5;
    ret[27]=4;
    ret[28]=5;
    ret[29]=5;
    ret[30]=5;
    ret[31]=5;
    ret[32]=5;
    ret[33]=5;
    ret[34]=5;
    ret[35]=4;
    ret[36]=5;
    ret[37]=5;
    ret[38]=5;
    ret[39]=5;
    ret[40]=5;
    ret[41]=5;
    ret[42]=5;
    ret[43]=5;
    ret[44]=4;
    ret[45]=5;
    ret[46]=5;
    ret[47]=5;
    ret[48]=5;
    ret[49]=5;
    ret[50]=5;
    ret[51]=5;
    ret[52]=5;
    ret[53]=5;
    ret[54]=4;
    ret[55]=5;
    ret[56]=5;
    ret[57]=5;
    ret[58]=5;
    ret[59]=5;
    ret[60]=5;
    ret[61]=5;
    ret[62]=5;
    ret[63]=5;
    ret[64]=5;
    ret[65]=4;
    return;  
}

if (*_tn == -2){
    ret[0] = 66;
    return;
  }
  else if (*_tn < -68 || *_tn > 66){
    ret[0] = NA_REAL;ret[1] = -1; //error("d(Omega^-1) derivative outside bounds");
  }
  else if (*length_theta != 66){
    ret[0] = NA_REAL;ret[1] = *length_theta;//error("requires vector with 66 arguments");
  }
    if (*_tn == 0){
      ret[0] = (_t[0]);
      ret[11] = _t[1];
      ret[12] = (_t[2]);
      ret[22] = _t[3];
      ret[23] = _t[4];
      ret[24] = (_t[5]);
      ret[33] = _t[6];
      ret[34] = _t[7];
      ret[35] = _t[8];
      ret[36] = (_t[9]);
      ret[44] = _t[10];
      ret[45] = _t[11];
      ret[46] = _t[12];
      ret[47] = _t[13];
      ret[48] = (_t[14]);
      ret[55] = _t[15];
      ret[56] = _t[16];
      ret[57] = _t[17];
      ret[58] = _t[18];
      ret[59] = _t[19];
      ret[60] = (_t[20]);
      ret[66] = _t[21];
      ret[67] = _t[22];
      ret[68] = _t[23];
      ret[69] = _t[24];
      ret[70] = _t[25];
      ret[71] = _t[26];
      ret[72] = (_t[27]);
      ret[77] = _t[28];
      ret[78] = _t[29];
      ret[79] = _t[30];
      ret[80] = _t[31];
      ret[81] = _t[32];
      ret[82] = _t[33];
      ret[83] = _t[34];
      ret[84] = (_t[35]);
      ret[88] = _t[36];
      ret[89] = _t[37];
      ret[90] = _t[38];
      ret[91] = _t[39];
      ret[92] = _t[40];
      ret[93] = _t[41];
      ret[94] = _t[42];
      ret[95] = _t[43];
      ret[96] = (_t[44]);
      ret[99] = _t[45];
      ret[100] = _t[46];
      ret[101] = _t[47];
      ret[102] = _t[48];
      ret[103] = _t[49];
      ret[104] = _t[50];
      ret[105] = _t[51];
      ret[106] = _t[52];
      ret[107] = _t[53];
      ret[108] = (_t[54]);
      ret[110] = _t[55];
      ret[111] = _t[56];
      ret[112] = _t[57];
      ret[113] = _t[58];
      ret[114] = _t[59];
      ret[115] = _t[60];
      ret[116] = _t[61];
      ret[117] = _t[62];
      ret[118] = _t[63];
      ret[119] = _t[64];
      ret[120] = (_t[65]);
    }
    else if (*_tn == -1){
      ret[0] = pow(_t[0], 2);
      ret[1] = _t[1]*_t[0];
      ret[2] = _t[0]*_t[3];
      ret[3] = _t[6]*_t[0];
      ret[4] = _t[0]*_t[10];
      ret[5] = _t[0]*_t[15];
      ret[6] = _t[0]*_t[21];
      ret[7] = _t[0]*_t[28];
      ret[8] = _t[0]*_t[36];
      ret[9] = _t[0]*_t[45];
      ret[10] = _t[0]*_t[55];
      ret[11] = _t[1]*_t[0];
      ret[12] = pow(_t[1], 2)+pow(_t[2], 2);
      ret[13] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[14] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[15] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[16] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[17] = _t[1]*_t[21]+_t[2]*_t[22];
      ret[18] = _t[1]*_t[28]+_t[2]*_t[29];
      ret[19] = _t[1]*_t[36]+_t[2]*_t[37];
      ret[20] = _t[1]*_t[45]+_t[2]*_t[46];
      ret[21] = _t[1]*_t[55]+_t[2]*_t[56];
      ret[22] = _t[0]*_t[3];
      ret[23] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[24] = pow(_t[3], 2)+pow(_t[4], 2)+pow(_t[5], 2);
      ret[25] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[26] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[27] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[28] = _t[3]*_t[21]+_t[4]*_t[22]+_t[5]*_t[23];
      ret[29] = _t[3]*_t[28]+_t[4]*_t[29]+_t[5]*_t[30];
      ret[30] = _t[3]*_t[36]+_t[4]*_t[37]+_t[5]*_t[38];
      ret[31] = _t[3]*_t[45]+_t[4]*_t[46]+_t[5]*_t[47];
      ret[32] = _t[3]*_t[55]+_t[4]*_t[56]+_t[5]*_t[57];
      ret[33] = _t[6]*_t[0];
      ret[34] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[35] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[36] = pow(_t[6], 2)+pow(_t[7], 2)+pow(_t[8], 2)+pow(_t[9], 2);
      ret[37] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[38] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[39] = _t[6]*_t[21]+_t[7]*_t[22]+_t[8]*_t[23]+_t[9]*_t[24];
      ret[40] = _t[6]*_t[28]+_t[7]*_t[29]+_t[8]*_t[30]+_t[9]*_t[31];
      ret[41] = _t[6]*_t[36]+_t[7]*_t[37]+_t[8]*_t[38]+_t[9]*_t[39];
      ret[42] = _t[6]*_t[45]+_t[7]*_t[46]+_t[8]*_t[47]+_t[9]*_t[48];
      ret[43] = _t[6]*_t[55]+_t[7]*_t[56]+_t[8]*_t[57]+_t[9]*_t[58];
      ret[44] = _t[0]*_t[10];
      ret[45] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[46] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[47] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[48] = pow(_t[10], 2)+pow(_t[11], 2)+pow(_t[12], 2)+pow(_t[13], 2)+pow(_t[14], 2);
      ret[49] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[50] = _t[21]*_t[10]+_t[22]*_t[11]+_t[23]*_t[12]+_t[24]*_t[13]+_t[25]*_t[14];
      ret[51] = _t[28]*_t[10]+_t[29]*_t[11]+_t[30]*_t[12]+_t[31]*_t[13]+_t[32]*_t[14];
      ret[52] = _t[36]*_t[10]+_t[37]*_t[11]+_t[38]*_t[12]+_t[39]*_t[13]+_t[40]*_t[14];
      ret[53] = _t[45]*_t[10]+_t[46]*_t[11]+_t[47]*_t[12]+_t[48]*_t[13]+_t[49]*_t[14];
      ret[54] = _t[55]*_t[10]+_t[56]*_t[11]+_t[57]*_t[12]+_t[58]*_t[13]+_t[59]*_t[14];
      ret[55] = _t[0]*_t[15];
      ret[56] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[57] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[58] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[59] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[60] = pow(_t[15], 2)+pow(_t[16], 2)+pow(_t[17], 2)+pow(_t[18], 2)+pow(_t[19], 2)+pow(_t[20], 2);
      ret[61] = _t[21]*_t[15]+_t[22]*_t[16]+_t[23]*_t[17]+_t[24]*_t[18]+_t[25]*_t[19]+_t[26]*_t[20];
      ret[62] = _t[20]*_t[33]+_t[28]*_t[15]+_t[29]*_t[16]+_t[30]*_t[17]+_t[31]*_t[18]+_t[32]*_t[19];
      ret[63] = _t[36]*_t[15]+_t[37]*_t[16]+_t[38]*_t[17]+_t[39]*_t[18]+_t[40]*_t[19]+_t[41]*_t[20];
      ret[64] = _t[45]*_t[15]+_t[46]*_t[16]+_t[47]*_t[17]+_t[48]*_t[18]+_t[49]*_t[19]+_t[50]*_t[20];
      ret[65] = _t[20]*_t[60]+_t[55]*_t[15]+_t[56]*_t[16]+_t[57]*_t[17]+_t[58]*_t[18]+_t[59]*_t[19];
      ret[66] = _t[0]*_t[21];
      ret[67] = _t[1]*_t[21]+_t[2]*_t[22];
      ret[68] = _t[3]*_t[21]+_t[4]*_t[22]+_t[5]*_t[23];
      ret[69] = _t[6]*_t[21]+_t[7]*_t[22]+_t[8]*_t[23]+_t[9]*_t[24];
      ret[70] = _t[21]*_t[10]+_t[22]*_t[11]+_t[23]*_t[12]+_t[24]*_t[13]+_t[25]*_t[14];
      ret[71] = _t[21]*_t[15]+_t[22]*_t[16]+_t[23]*_t[17]+_t[24]*_t[18]+_t[25]*_t[19]+_t[26]*_t[20];
      ret[72] = pow(_t[21], 2)+pow(_t[22], 2)+pow(_t[23], 2)+pow(_t[24], 2)+pow(_t[25], 2)+pow(_t[26], 2)+pow(_t[27], 2);
      ret[73] = _t[21]*_t[28]+_t[23]*_t[30]+_t[24]*_t[31]+_t[25]*_t[32]+_t[26]*_t[33]+_t[27]*_t[34]+_t[29]*_t[22];
      ret[74] = _t[21]*_t[36]+_t[22]*_t[37]+_t[23]*_t[38]+_t[24]*_t[39]+_t[40]*_t[25]+_t[41]*_t[26]+_t[42]*_t[27];
      ret[75] = _t[45]*_t[21]+_t[46]*_t[22]+_t[47]*_t[23]+_t[48]*_t[24]+_t[49]*_t[25]+_t[50]*_t[26]+_t[51]*_t[27];
      ret[76] = _t[26]*_t[60]+_t[27]*_t[61]+_t[55]*_t[21]+_t[56]*_t[22]+_t[57]*_t[23]+_t[58]*_t[24]+_t[59]*_t[25];
      ret[77] = _t[0]*_t[28];
      ret[78] = _t[1]*_t[28]+_t[2]*_t[29];
      ret[79] = _t[3]*_t[28]+_t[4]*_t[29]+_t[5]*_t[30];
      ret[80] = _t[6]*_t[28]+_t[7]*_t[29]+_t[8]*_t[30]+_t[9]*_t[31];
      ret[81] = _t[28]*_t[10]+_t[29]*_t[11]+_t[30]*_t[12]+_t[31]*_t[13]+_t[32]*_t[14];
      ret[82] = _t[20]*_t[33]+_t[28]*_t[15]+_t[29]*_t[16]+_t[30]*_t[17]+_t[31]*_t[18]+_t[32]*_t[19];
      ret[83] = _t[21]*_t[28]+_t[23]*_t[30]+_t[24]*_t[31]+_t[25]*_t[32]+_t[26]*_t[33]+_t[27]*_t[34]+_t[29]*_t[22];
      ret[84] = pow(_t[28], 2)+pow(_t[29], 2)+pow(_t[30], 2)+pow(_t[31], 2)+pow(_t[32], 2)+pow(_t[33], 2)+pow(_t[34], 2)+pow(_t[35], 2);
      ret[85] = _t[28]*_t[36]+_t[29]*_t[37]+_t[38]*_t[30]+_t[39]*_t[31]+_t[40]*_t[32]+_t[41]*_t[33]+_t[42]*_t[34]+_t[43]*_t[35];
      ret[86] = _t[45]*_t[28]+_t[46]*_t[29]+_t[47]*_t[30]+_t[48]*_t[31]+_t[49]*_t[32]+_t[50]*_t[33]+_t[51]*_t[34]+_t[52]*_t[35];
      ret[87] = _t[33]*_t[60]+_t[34]*_t[61]+_t[35]*_t[62]+_t[55]*_t[28]+_t[56]*_t[29]+_t[57]*_t[30]+_t[58]*_t[31]+_t[59]*_t[32];
      ret[88] = _t[0]*_t[36];
      ret[89] = _t[1]*_t[36]+_t[2]*_t[37];
      ret[90] = _t[3]*_t[36]+_t[4]*_t[37]+_t[5]*_t[38];
      ret[91] = _t[6]*_t[36]+_t[7]*_t[37]+_t[8]*_t[38]+_t[9]*_t[39];
      ret[92] = _t[36]*_t[10]+_t[37]*_t[11]+_t[38]*_t[12]+_t[39]*_t[13]+_t[40]*_t[14];
      ret[93] = _t[36]*_t[15]+_t[37]*_t[16]+_t[38]*_t[17]+_t[39]*_t[18]+_t[40]*_t[19]+_t[41]*_t[20];
      ret[94] = _t[21]*_t[36]+_t[22]*_t[37]+_t[23]*_t[38]+_t[24]*_t[39]+_t[40]*_t[25]+_t[41]*_t[26]+_t[42]*_t[27];
      ret[95] = _t[28]*_t[36]+_t[29]*_t[37]+_t[38]*_t[30]+_t[39]*_t[31]+_t[40]*_t[32]+_t[41]*_t[33]+_t[42]*_t[34]+_t[43]*_t[35];
      ret[96] = pow(_t[36], 2)+pow(_t[37], 2)+pow(_t[38], 2)+pow(_t[39], 2)+pow(_t[40], 2)+pow(_t[41], 2)+pow(_t[42], 2)+pow(_t[43], 2)+pow(_t[44], 2);
      ret[97] = _t[41]*_t[50]+_t[42]*_t[51]+_t[43]*_t[52]+_t[44]*_t[53]+_t[45]*_t[36]+_t[46]*_t[37]+_t[47]*_t[38]+_t[48]*_t[39]+_t[49]*_t[40];
      ret[98] = _t[40]*_t[59]+_t[41]*_t[60]+_t[42]*_t[61]+_t[43]*_t[62]+_t[44]*_t[63]+_t[55]*_t[36]+_t[56]*_t[37]+_t[57]*_t[38]+_t[58]*_t[39];
      ret[99] = _t[0]*_t[45];
      ret[100] = _t[1]*_t[45]+_t[2]*_t[46];
      ret[101] = _t[3]*_t[45]+_t[4]*_t[46]+_t[5]*_t[47];
      ret[102] = _t[6]*_t[45]+_t[7]*_t[46]+_t[8]*_t[47]+_t[9]*_t[48];
      ret[103] = _t[45]*_t[10]+_t[46]*_t[11]+_t[47]*_t[12]+_t[48]*_t[13]+_t[49]*_t[14];
      ret[104] = _t[45]*_t[15]+_t[46]*_t[16]+_t[47]*_t[17]+_t[48]*_t[18]+_t[49]*_t[19]+_t[50]*_t[20];
      ret[105] = _t[45]*_t[21]+_t[46]*_t[22]+_t[47]*_t[23]+_t[48]*_t[24]+_t[49]*_t[25]+_t[50]*_t[26]+_t[51]*_t[27];
      ret[106] = _t[45]*_t[28]+_t[46]*_t[29]+_t[47]*_t[30]+_t[48]*_t[31]+_t[49]*_t[32]+_t[50]*_t[33]+_t[51]*_t[34]+_t[52]*_t[35];
      ret[107] = _t[41]*_t[50]+_t[42]*_t[51]+_t[43]*_t[52]+_t[44]*_t[53]+_t[45]*_t[36]+_t[46]*_t[37]+_t[47]*_t[38]+_t[48]*_t[39]+_t[49]*_t[40];
      ret[108] = pow(_t[45], 2)+pow(_t[46], 2)+pow(_t[47], 2)+pow(_t[48], 2)+pow(_t[49], 2)+pow(_t[50], 2)+pow(_t[51], 2)+pow(_t[52], 2)+pow(_t[53], 2)+pow(_t[54], 2);
      ret[109] = _t[45]*_t[55]+_t[46]*_t[56]+_t[47]*_t[57]+_t[48]*_t[58]+_t[49]*_t[59]+_t[50]*_t[60]+_t[51]*_t[61]+_t[52]*_t[62]+_t[53]*_t[63]+_t[54]*_t[64];
      ret[110] = _t[0]*_t[55];
      ret[111] = _t[1]*_t[55]+_t[2]*_t[56];
      ret[112] = _t[3]*_t[55]+_t[4]*_t[56]+_t[5]*_t[57];
      ret[113] = _t[6]*_t[55]+_t[7]*_t[56]+_t[8]*_t[57]+_t[9]*_t[58];
      ret[114] = _t[55]*_t[10]+_t[56]*_t[11]+_t[57]*_t[12]+_t[58]*_t[13]+_t[59]*_t[14];
      ret[115] = _t[20]*_t[60]+_t[55]*_t[15]+_t[56]*_t[16]+_t[57]*_t[17]+_t[58]*_t[18]+_t[59]*_t[19];
      ret[116] = _t[26]*_t[60]+_t[27]*_t[61]+_t[55]*_t[21]+_t[56]*_t[22]+_t[57]*_t[23]+_t[58]*_t[24]+_t[59]*_t[25];
      ret[117] = _t[33]*_t[60]+_t[34]*_t[61]+_t[35]*_t[62]+_t[55]*_t[28]+_t[56]*_t[29]+_t[57]*_t[30]+_t[58]*_t[31]+_t[59]*_t[32];
      ret[118] = _t[40]*_t[59]+_t[41]*_t[60]+_t[42]*_t[61]+_t[43]*_t[62]+_t[44]*_t[63]+_t[55]*_t[36]+_t[56]*_t[37]+_t[57]*_t[38]+_t[58]*_t[39];
      ret[119] = _t[45]*_t[55]+_t[46]*_t[56]+_t[47]*_t[57]+_t[48]*_t[58]+_t[49]*_t[59]+_t[50]*_t[60]+_t[51]*_t[61]+_t[52]*_t[62]+_t[53]*_t[63]+_t[54]*_t[64];
      ret[120] = pow(_t[55], 2)+pow(_t[56], 2)+pow(_t[57], 2)+pow(_t[58], 2)+pow(_t[59], 2)+pow(_t[60], 2)+pow(_t[61], 2)+pow(_t[62], 2)+pow(_t[63], 2)+pow(_t[64], 2)+pow(_t[65], 2);
    }
    else if (*_tn == 1){
      ret[0] = 2*_t[0];
      ret[1] = _t[1];
      ret[2] = _t[3];
      ret[3] = _t[6];
      ret[4] = _t[10];
      ret[5] = _t[15];
      ret[6] = _t[21];
      ret[7] = _t[28];
      ret[8] = _t[36];
      ret[9] = _t[45];
      ret[10] = _t[55];
      ret[11] = _t[1];
      ret[22] = _t[3];
      ret[33] = _t[6];
      ret[44] = _t[10];
      ret[55] = _t[15];
      ret[66] = _t[21];
      ret[77] = _t[28];
      ret[88] = _t[36];
      ret[99] = _t[45];
      ret[110] = _t[55];
    }
    else if (*_tn == 2){
      ret[1] = _t[0];
      ret[11] = _t[0];
      ret[12] = 2*_t[1];
      ret[13] = _t[3];
      ret[14] = _t[6];
      ret[15] = _t[10];
      ret[16] = _t[15];
      ret[17] = _t[21];
      ret[18] = _t[28];
      ret[19] = _t[36];
      ret[20] = _t[45];
      ret[21] = _t[55];
      ret[23] = _t[3];
      ret[34] = _t[6];
      ret[45] = _t[10];
      ret[56] = _t[15];
      ret[67] = _t[21];
      ret[78] = _t[28];
      ret[89] = _t[36];
      ret[100] = _t[45];
      ret[111] = _t[55];
    }
    else if (*_tn == 3){
      ret[12] = 2*_t[2];
      ret[13] = _t[4];
      ret[14] = _t[7];
      ret[15] = _t[11];
      ret[16] = _t[16];
      ret[17] = _t[22];
      ret[18] = _t[29];
      ret[19] = _t[37];
      ret[20] = _t[46];
      ret[21] = _t[56];
      ret[23] = _t[4];
      ret[34] = _t[7];
      ret[45] = _t[11];
      ret[56] = _t[16];
      ret[67] = _t[22];
      ret[78] = _t[29];
      ret[89] = _t[37];
      ret[100] = _t[46];
      ret[111] = _t[56];
    }
    else if (*_tn == 4){
      ret[2] = _t[0];
      ret[13] = _t[1];
      ret[22] = _t[0];
      ret[23] = _t[1];
      ret[24] = 2*_t[3];
      ret[25] = _t[6];
      ret[26] = _t[10];
      ret[27] = _t[15];
      ret[28] = _t[21];
      ret[29] = _t[28];
      ret[30] = _t[36];
      ret[31] = _t[45];
      ret[32] = _t[55];
      ret[35] = _t[6];
      ret[46] = _t[10];
      ret[57] = _t[15];
      ret[68] = _t[21];
      ret[79] = _t[28];
      ret[90] = _t[36];
      ret[101] = _t[45];
      ret[112] = _t[55];
    }
    else if (*_tn == 5){
      ret[13] = _t[2];
      ret[23] = _t[2];
      ret[24] = 2*_t[4];
      ret[25] = _t[7];
      ret[26] = _t[11];
      ret[27] = _t[16];
      ret[28] = _t[22];
      ret[29] = _t[29];
      ret[30] = _t[37];
      ret[31] = _t[46];
      ret[32] = _t[56];
      ret[35] = _t[7];
      ret[46] = _t[11];
      ret[57] = _t[16];
      ret[68] = _t[22];
      ret[79] = _t[29];
      ret[90] = _t[37];
      ret[101] = _t[46];
      ret[112] = _t[56];
    }
    else if (*_tn == 6){
      ret[24] = 2*_t[5];
      ret[25] = _t[8];
      ret[26] = _t[12];
      ret[27] = _t[17];
      ret[28] = _t[23];
      ret[29] = _t[30];
      ret[30] = _t[38];
      ret[31] = _t[47];
      ret[32] = _t[57];
      ret[35] = _t[8];
      ret[46] = _t[12];
      ret[57] = _t[17];
      ret[68] = _t[23];
      ret[79] = _t[30];
      ret[90] = _t[38];
      ret[101] = _t[47];
      ret[112] = _t[57];
    }
    else if (*_tn == 7){
      ret[3] = _t[0];
      ret[14] = _t[1];
      ret[25] = _t[3];
      ret[33] = _t[0];
      ret[34] = _t[1];
      ret[35] = _t[3];
      ret[36] = 2*_t[6];
      ret[37] = _t[10];
      ret[38] = _t[15];
      ret[39] = _t[21];
      ret[40] = _t[28];
      ret[41] = _t[36];
      ret[42] = _t[45];
      ret[43] = _t[55];
      ret[47] = _t[10];
      ret[58] = _t[15];
      ret[69] = _t[21];
      ret[80] = _t[28];
      ret[91] = _t[36];
      ret[102] = _t[45];
      ret[113] = _t[55];
    }
    else if (*_tn == 8){
      ret[14] = _t[2];
      ret[25] = _t[4];
      ret[34] = _t[2];
      ret[35] = _t[4];
      ret[36] = 2*_t[7];
      ret[37] = _t[11];
      ret[38] = _t[16];
      ret[39] = _t[22];
      ret[40] = _t[29];
      ret[41] = _t[37];
      ret[42] = _t[46];
      ret[43] = _t[56];
      ret[47] = _t[11];
      ret[58] = _t[16];
      ret[69] = _t[22];
      ret[80] = _t[29];
      ret[91] = _t[37];
      ret[102] = _t[46];
      ret[113] = _t[56];
    }
    else if (*_tn == 9){
      ret[25] = _t[5];
      ret[35] = _t[5];
      ret[36] = 2*_t[8];
      ret[37] = _t[12];
      ret[38] = _t[17];
      ret[39] = _t[23];
      ret[40] = _t[30];
      ret[41] = _t[38];
      ret[42] = _t[47];
      ret[43] = _t[57];
      ret[47] = _t[12];
      ret[58] = _t[17];
      ret[69] = _t[23];
      ret[80] = _t[30];
      ret[91] = _t[38];
      ret[102] = _t[47];
      ret[113] = _t[57];
    }
    else if (*_tn == 10){
      ret[36] = 2*_t[9];
      ret[37] = _t[13];
      ret[38] = _t[18];
      ret[39] = _t[24];
      ret[40] = _t[31];
      ret[41] = _t[39];
      ret[42] = _t[48];
      ret[43] = _t[58];
      ret[47] = _t[13];
      ret[58] = _t[18];
      ret[69] = _t[24];
      ret[80] = _t[31];
      ret[91] = _t[39];
      ret[102] = _t[48];
      ret[113] = _t[58];
    }
    else if (*_tn == 11){
      ret[4] = _t[0];
      ret[15] = _t[1];
      ret[26] = _t[3];
      ret[37] = _t[6];
      ret[44] = _t[0];
      ret[45] = _t[1];
      ret[46] = _t[3];
      ret[47] = _t[6];
      ret[48] = 2*_t[10];
      ret[49] = _t[15];
      ret[50] = _t[21];
      ret[51] = _t[28];
      ret[52] = _t[36];
      ret[53] = _t[45];
      ret[54] = _t[55];
      ret[59] = _t[15];
      ret[70] = _t[21];
      ret[81] = _t[28];
      ret[92] = _t[36];
      ret[103] = _t[45];
      ret[114] = _t[55];
    }
    else if (*_tn == 12){
      ret[15] = _t[2];
      ret[26] = _t[4];
      ret[37] = _t[7];
      ret[45] = _t[2];
      ret[46] = _t[4];
      ret[47] = _t[7];
      ret[48] = 2*_t[11];
      ret[49] = _t[16];
      ret[50] = _t[22];
      ret[51] = _t[29];
      ret[52] = _t[37];
      ret[53] = _t[46];
      ret[54] = _t[56];
      ret[59] = _t[16];
      ret[70] = _t[22];
      ret[81] = _t[29];
      ret[92] = _t[37];
      ret[103] = _t[46];
      ret[114] = _t[56];
    }
    else if (*_tn == 13){
      ret[26] = _t[5];
      ret[37] = _t[8];
      ret[46] = _t[5];
      ret[47] = _t[8];
      ret[48] = 2*_t[12];
      ret[49] = _t[17];
      ret[50] = _t[23];
      ret[51] = _t[30];
      ret[52] = _t[38];
      ret[53] = _t[47];
      ret[54] = _t[57];
      ret[59] = _t[17];
      ret[70] = _t[23];
      ret[81] = _t[30];
      ret[92] = _t[38];
      ret[103] = _t[47];
      ret[114] = _t[57];
    }
    else if (*_tn == 14){
      ret[37] = _t[9];
      ret[47] = _t[9];
      ret[48] = 2*_t[13];
      ret[49] = _t[18];
      ret[50] = _t[24];
      ret[51] = _t[31];
      ret[52] = _t[39];
      ret[53] = _t[48];
      ret[54] = _t[58];
      ret[59] = _t[18];
      ret[70] = _t[24];
      ret[81] = _t[31];
      ret[92] = _t[39];
      ret[103] = _t[48];
      ret[114] = _t[58];
    }
    else if (*_tn == 15){
      ret[48] = 2*_t[14];
      ret[49] = _t[19];
      ret[50] = _t[25];
      ret[51] = _t[32];
      ret[52] = _t[40];
      ret[53] = _t[49];
      ret[54] = _t[59];
      ret[59] = _t[19];
      ret[70] = _t[25];
      ret[81] = _t[32];
      ret[92] = _t[40];
      ret[103] = _t[49];
      ret[114] = _t[59];
    }
    else if (*_tn == 16){
      ret[5] = _t[0];
      ret[16] = _t[1];
      ret[27] = _t[3];
      ret[38] = _t[6];
      ret[49] = _t[10];
      ret[55] = _t[0];
      ret[56] = _t[1];
      ret[57] = _t[3];
      ret[58] = _t[6];
      ret[59] = _t[10];
      ret[60] = 2*_t[15];
      ret[61] = _t[21];
      ret[62] = _t[28];
      ret[63] = _t[36];
      ret[64] = _t[45];
      ret[65] = _t[55];
      ret[71] = _t[21];
      ret[82] = _t[28];
      ret[93] = _t[36];
      ret[104] = _t[45];
      ret[115] = _t[55];
    }
    else if (*_tn == 17){
      ret[16] = _t[2];
      ret[27] = _t[4];
      ret[38] = _t[7];
      ret[49] = _t[11];
      ret[56] = _t[2];
      ret[57] = _t[4];
      ret[58] = _t[7];
      ret[59] = _t[11];
      ret[60] = 2*_t[16];
      ret[61] = _t[22];
      ret[62] = _t[29];
      ret[63] = _t[37];
      ret[64] = _t[46];
      ret[65] = _t[56];
      ret[71] = _t[22];
      ret[82] = _t[29];
      ret[93] = _t[37];
      ret[104] = _t[46];
      ret[115] = _t[56];
    }
    else if (*_tn == 18){
      ret[27] = _t[5];
      ret[38] = _t[8];
      ret[49] = _t[12];
      ret[57] = _t[5];
      ret[58] = _t[8];
      ret[59] = _t[12];
      ret[60] = 2*_t[17];
      ret[61] = _t[23];
      ret[62] = _t[30];
      ret[63] = _t[38];
      ret[64] = _t[47];
      ret[65] = _t[57];
      ret[71] = _t[23];
      ret[82] = _t[30];
      ret[93] = _t[38];
      ret[104] = _t[47];
      ret[115] = _t[57];
    }
    else if (*_tn == 19){
      ret[38] = _t[9];
      ret[49] = _t[13];
      ret[58] = _t[9];
      ret[59] = _t[13];
      ret[60] = 2*_t[18];
      ret[61] = _t[24];
      ret[62] = _t[31];
      ret[63] = _t[39];
      ret[64] = _t[48];
      ret[65] = _t[58];
      ret[71] = _t[24];
      ret[82] = _t[31];
      ret[93] = _t[39];
      ret[104] = _t[48];
      ret[115] = _t[58];
    }
    else if (*_tn == 20){
      ret[49] = _t[14];
      ret[59] = _t[14];
      ret[60] = 2*_t[19];
      ret[61] = _t[25];
      ret[62] = _t[32];
      ret[63] = _t[40];
      ret[64] = _t[49];
      ret[65] = _t[59];
      ret[71] = _t[25];
      ret[82] = _t[32];
      ret[93] = _t[40];
      ret[104] = _t[49];
      ret[115] = _t[59];
    }
    else if (*_tn == 21){
      ret[60] = 2*_t[20];
      ret[61] = _t[26];
      ret[62] = _t[33];
      ret[63] = _t[41];
      ret[64] = _t[50];
      ret[65] = _t[60];
      ret[71] = _t[26];
      ret[82] = _t[33];
      ret[93] = _t[41];
      ret[104] = _t[50];
      ret[115] = _t[60];
    }
    else if (*_tn == 22){
      ret[6] = _t[0];
      ret[17] = _t[1];
      ret[28] = _t[3];
      ret[39] = _t[6];
      ret[50] = _t[10];
      ret[61] = _t[15];
      ret[66] = _t[0];
      ret[67] = _t[1];
      ret[68] = _t[3];
      ret[69] = _t[6];
      ret[70] = _t[10];
      ret[71] = _t[15];
      ret[72] = 2*_t[21];
      ret[73] = _t[28];
      ret[74] = _t[36];
      ret[75] = _t[45];
      ret[76] = _t[55];
      ret[83] = _t[28];
      ret[94] = _t[36];
      ret[105] = _t[45];
      ret[116] = _t[55];
    }
    else if (*_tn == 23){
      ret[17] = _t[2];
      ret[28] = _t[4];
      ret[39] = _t[7];
      ret[50] = _t[11];
      ret[61] = _t[16];
      ret[67] = _t[2];
      ret[68] = _t[4];
      ret[69] = _t[7];
      ret[70] = _t[11];
      ret[71] = _t[16];
      ret[72] = 2*_t[22];
      ret[73] = _t[29];
      ret[74] = _t[37];
      ret[75] = _t[46];
      ret[76] = _t[56];
      ret[83] = _t[29];
      ret[94] = _t[37];
      ret[105] = _t[46];
      ret[116] = _t[56];
    }
    else if (*_tn == 24){
      ret[28] = _t[5];
      ret[39] = _t[8];
      ret[50] = _t[12];
      ret[61] = _t[17];
      ret[68] = _t[5];
      ret[69] = _t[8];
      ret[70] = _t[12];
      ret[71] = _t[17];
      ret[72] = 2*_t[23];
      ret[73] = _t[30];
      ret[74] = _t[38];
      ret[75] = _t[47];
      ret[76] = _t[57];
      ret[83] = _t[30];
      ret[94] = _t[38];
      ret[105] = _t[47];
      ret[116] = _t[57];
    }
    else if (*_tn == 25){
      ret[39] = _t[9];
      ret[50] = _t[13];
      ret[61] = _t[18];
      ret[69] = _t[9];
      ret[70] = _t[13];
      ret[71] = _t[18];
      ret[72] = 2*_t[24];
      ret[73] = _t[31];
      ret[74] = _t[39];
      ret[75] = _t[48];
      ret[76] = _t[58];
      ret[83] = _t[31];
      ret[94] = _t[39];
      ret[105] = _t[48];
      ret[116] = _t[58];
    }
    else if (*_tn == 26){
      ret[50] = _t[14];
      ret[61] = _t[19];
      ret[70] = _t[14];
      ret[71] = _t[19];
      ret[72] = 2*_t[25];
      ret[73] = _t[32];
      ret[74] = _t[40];
      ret[75] = _t[49];
      ret[76] = _t[59];
      ret[83] = _t[32];
      ret[94] = _t[40];
      ret[105] = _t[49];
      ret[116] = _t[59];
    }
    else if (*_tn == 27){
      ret[61] = _t[20];
      ret[71] = _t[20];
      ret[72] = 2*_t[26];
      ret[73] = _t[33];
      ret[74] = _t[41];
      ret[75] = _t[50];
      ret[76] = _t[60];
      ret[83] = _t[33];
      ret[94] = _t[41];
      ret[105] = _t[50];
      ret[116] = _t[60];
    }
    else if (*_tn == 28){
      ret[72] = 2*_t[27];
      ret[73] = _t[34];
      ret[74] = _t[42];
      ret[75] = _t[51];
      ret[76] = _t[61];
      ret[83] = _t[34];
      ret[94] = _t[42];
      ret[105] = _t[51];
      ret[116] = _t[61];
    }
    else if (*_tn == 29){
      ret[7] = _t[0];
      ret[18] = _t[1];
      ret[29] = _t[3];
      ret[40] = _t[6];
      ret[51] = _t[10];
      ret[62] = _t[15];
      ret[73] = _t[21];
      ret[77] = _t[0];
      ret[78] = _t[1];
      ret[79] = _t[3];
      ret[80] = _t[6];
      ret[81] = _t[10];
      ret[82] = _t[15];
      ret[83] = _t[21];
      ret[84] = 2*_t[28];
      ret[85] = _t[36];
      ret[86] = _t[45];
      ret[87] = _t[55];
      ret[95] = _t[36];
      ret[106] = _t[45];
      ret[117] = _t[55];
    }
    else if (*_tn == 30){
      ret[18] = _t[2];
      ret[29] = _t[4];
      ret[40] = _t[7];
      ret[51] = _t[11];
      ret[62] = _t[16];
      ret[73] = _t[22];
      ret[78] = _t[2];
      ret[79] = _t[4];
      ret[80] = _t[7];
      ret[81] = _t[11];
      ret[82] = _t[16];
      ret[83] = _t[22];
      ret[84] = 2*_t[29];
      ret[85] = _t[37];
      ret[86] = _t[46];
      ret[87] = _t[56];
      ret[95] = _t[37];
      ret[106] = _t[46];
      ret[117] = _t[56];
    }
    else if (*_tn == 31){
      ret[29] = _t[5];
      ret[40] = _t[8];
      ret[51] = _t[12];
      ret[62] = _t[17];
      ret[73] = _t[23];
      ret[79] = _t[5];
      ret[80] = _t[8];
      ret[81] = _t[12];
      ret[82] = _t[17];
      ret[83] = _t[23];
      ret[84] = 2*_t[30];
      ret[85] = _t[38];
      ret[86] = _t[47];
      ret[87] = _t[57];
      ret[95] = _t[38];
      ret[106] = _t[47];
      ret[117] = _t[57];
    }
    else if (*_tn == 32){
      ret[40] = _t[9];
      ret[51] = _t[13];
      ret[62] = _t[18];
      ret[73] = _t[24];
      ret[80] = _t[9];
      ret[81] = _t[13];
      ret[82] = _t[18];
      ret[83] = _t[24];
      ret[84] = 2*_t[31];
      ret[85] = _t[39];
      ret[86] = _t[48];
      ret[87] = _t[58];
      ret[95] = _t[39];
      ret[106] = _t[48];
      ret[117] = _t[58];
    }
    else if (*_tn == 33){
      ret[51] = _t[14];
      ret[62] = _t[19];
      ret[73] = _t[25];
      ret[81] = _t[14];
      ret[82] = _t[19];
      ret[83] = _t[25];
      ret[84] = 2*_t[32];
      ret[85] = _t[40];
      ret[86] = _t[49];
      ret[87] = _t[59];
      ret[95] = _t[40];
      ret[106] = _t[49];
      ret[117] = _t[59];
    }
    else if (*_tn == 34){
      ret[62] = _t[20];
      ret[73] = _t[26];
      ret[82] = _t[20];
      ret[83] = _t[26];
      ret[84] = 2*_t[33];
      ret[85] = _t[41];
      ret[86] = _t[50];
      ret[87] = _t[60];
      ret[95] = _t[41];
      ret[106] = _t[50];
      ret[117] = _t[60];
    }
    else if (*_tn == 35){
      ret[73] = _t[27];
      ret[83] = _t[27];
      ret[84] = 2*_t[34];
      ret[85] = _t[42];
      ret[86] = _t[51];
      ret[87] = _t[61];
      ret[95] = _t[42];
      ret[106] = _t[51];
      ret[117] = _t[61];
    }
    else if (*_tn == 36){
      ret[84] = 2*_t[35];
      ret[85] = _t[43];
      ret[86] = _t[52];
      ret[87] = _t[62];
      ret[95] = _t[43];
      ret[106] = _t[52];
      ret[117] = _t[62];
    }
    else if (*_tn == 37){
      ret[8] = _t[0];
      ret[19] = _t[1];
      ret[30] = _t[3];
      ret[41] = _t[6];
      ret[52] = _t[10];
      ret[63] = _t[15];
      ret[74] = _t[21];
      ret[85] = _t[28];
      ret[88] = _t[0];
      ret[89] = _t[1];
      ret[90] = _t[3];
      ret[91] = _t[6];
      ret[92] = _t[10];
      ret[93] = _t[15];
      ret[94] = _t[21];
      ret[95] = _t[28];
      ret[96] = 2*_t[36];
      ret[97] = _t[45];
      ret[98] = _t[55];
      ret[107] = _t[45];
      ret[118] = _t[55];
    }
    else if (*_tn == 38){
      ret[19] = _t[2];
      ret[30] = _t[4];
      ret[41] = _t[7];
      ret[52] = _t[11];
      ret[63] = _t[16];
      ret[74] = _t[22];
      ret[85] = _t[29];
      ret[89] = _t[2];
      ret[90] = _t[4];
      ret[91] = _t[7];
      ret[92] = _t[11];
      ret[93] = _t[16];
      ret[94] = _t[22];
      ret[95] = _t[29];
      ret[96] = 2*_t[37];
      ret[97] = _t[46];
      ret[98] = _t[56];
      ret[107] = _t[46];
      ret[118] = _t[56];
    }
    else if (*_tn == 39){
      ret[30] = _t[5];
      ret[41] = _t[8];
      ret[52] = _t[12];
      ret[63] = _t[17];
      ret[74] = _t[23];
      ret[85] = _t[30];
      ret[90] = _t[5];
      ret[91] = _t[8];
      ret[92] = _t[12];
      ret[93] = _t[17];
      ret[94] = _t[23];
      ret[95] = _t[30];
      ret[96] = 2*_t[38];
      ret[97] = _t[47];
      ret[98] = _t[57];
      ret[107] = _t[47];
      ret[118] = _t[57];
    }
    else if (*_tn == 40){
      ret[41] = _t[9];
      ret[52] = _t[13];
      ret[63] = _t[18];
      ret[74] = _t[24];
      ret[85] = _t[31];
      ret[91] = _t[9];
      ret[92] = _t[13];
      ret[93] = _t[18];
      ret[94] = _t[24];
      ret[95] = _t[31];
      ret[96] = 2*_t[39];
      ret[97] = _t[48];
      ret[98] = _t[58];
      ret[107] = _t[48];
      ret[118] = _t[58];
    }
    else if (*_tn == 41){
      ret[52] = _t[14];
      ret[63] = _t[19];
      ret[74] = _t[25];
      ret[85] = _t[32];
      ret[92] = _t[14];
      ret[93] = _t[19];
      ret[94] = _t[25];
      ret[95] = _t[32];
      ret[96] = 2*_t[40];
      ret[97] = _t[49];
      ret[98] = _t[59];
      ret[107] = _t[49];
      ret[118] = _t[59];
    }
    else if (*_tn == 42){
      ret[63] = _t[20];
      ret[74] = _t[26];
      ret[85] = _t[33];
      ret[93] = _t[20];
      ret[94] = _t[26];
      ret[95] = _t[33];
      ret[96] = 2*_t[41];
      ret[97] = _t[50];
      ret[98] = _t[60];
      ret[107] = _t[50];
      ret[118] = _t[60];
    }
    else if (*_tn == 43){
      ret[74] = _t[27];
      ret[85] = _t[34];
      ret[94] = _t[27];
      ret[95] = _t[34];
      ret[96] = 2*_t[42];
      ret[97] = _t[51];
      ret[98] = _t[61];
      ret[107] = _t[51];
      ret[118] = _t[61];
    }
    else if (*_tn == 44){
      ret[85] = _t[35];
      ret[95] = _t[35];
      ret[96] = 2*_t[43];
      ret[97] = _t[52];
      ret[98] = _t[62];
      ret[107] = _t[52];
      ret[118] = _t[62];
    }
    else if (*_tn == 45){
      ret[96] = 2*_t[44];
      ret[97] = _t[53];
      ret[98] = _t[63];
      ret[107] = _t[53];
      ret[118] = _t[63];
    }
    else if (*_tn == 46){
      ret[9] = _t[0];
      ret[20] = _t[1];
      ret[31] = _t[3];
      ret[42] = _t[6];
      ret[53] = _t[10];
      ret[64] = _t[15];
      ret[75] = _t[21];
      ret[86] = _t[28];
      ret[97] = _t[36];
      ret[99] = _t[0];
      ret[100] = _t[1];
      ret[101] = _t[3];
      ret[102] = _t[6];
      ret[103] = _t[10];
      ret[104] = _t[15];
      ret[105] = _t[21];
      ret[106] = _t[28];
      ret[107] = _t[36];
      ret[108] = 2*_t[45];
      ret[109] = _t[55];
      ret[119] = _t[55];
    }
    else if (*_tn == 47){
      ret[20] = _t[2];
      ret[31] = _t[4];
      ret[42] = _t[7];
      ret[53] = _t[11];
      ret[64] = _t[16];
      ret[75] = _t[22];
      ret[86] = _t[29];
      ret[97] = _t[37];
      ret[100] = _t[2];
      ret[101] = _t[4];
      ret[102] = _t[7];
      ret[103] = _t[11];
      ret[104] = _t[16];
      ret[105] = _t[22];
      ret[106] = _t[29];
      ret[107] = _t[37];
      ret[108] = 2*_t[46];
      ret[109] = _t[56];
      ret[119] = _t[56];
    }
    else if (*_tn == 48){
      ret[31] = _t[5];
      ret[42] = _t[8];
      ret[53] = _t[12];
      ret[64] = _t[17];
      ret[75] = _t[23];
      ret[86] = _t[30];
      ret[97] = _t[38];
      ret[101] = _t[5];
      ret[102] = _t[8];
      ret[103] = _t[12];
      ret[104] = _t[17];
      ret[105] = _t[23];
      ret[106] = _t[30];
      ret[107] = _t[38];
      ret[108] = 2*_t[47];
      ret[109] = _t[57];
      ret[119] = _t[57];
    }
    else if (*_tn == 49){
      ret[42] = _t[9];
      ret[53] = _t[13];
      ret[64] = _t[18];
      ret[75] = _t[24];
      ret[86] = _t[31];
      ret[97] = _t[39];
      ret[102] = _t[9];
      ret[103] = _t[13];
      ret[104] = _t[18];
      ret[105] = _t[24];
      ret[106] = _t[31];
      ret[107] = _t[39];
      ret[108] = 2*_t[48];
      ret[109] = _t[58];
      ret[119] = _t[58];
    }
    else if (*_tn == 50){
      ret[53] = _t[14];
      ret[64] = _t[19];
      ret[75] = _t[25];
      ret[86] = _t[32];
      ret[97] = _t[40];
      ret[103] = _t[14];
      ret[104] = _t[19];
      ret[105] = _t[25];
      ret[106] = _t[32];
      ret[107] = _t[40];
      ret[108] = 2*_t[49];
      ret[109] = _t[59];
      ret[119] = _t[59];
    }
    else if (*_tn == 51){
      ret[64] = _t[20];
      ret[75] = _t[26];
      ret[86] = _t[33];
      ret[97] = _t[41];
      ret[104] = _t[20];
      ret[105] = _t[26];
      ret[106] = _t[33];
      ret[107] = _t[41];
      ret[108] = 2*_t[50];
      ret[109] = _t[60];
      ret[119] = _t[60];
    }
    else if (*_tn == 52){
      ret[75] = _t[27];
      ret[86] = _t[34];
      ret[97] = _t[42];
      ret[105] = _t[27];
      ret[106] = _t[34];
      ret[107] = _t[42];
      ret[108] = 2*_t[51];
      ret[109] = _t[61];
      ret[119] = _t[61];
    }
    else if (*_tn == 53){
      ret[86] = _t[35];
      ret[97] = _t[43];
      ret[106] = _t[35];
      ret[107] = _t[43];
      ret[108] = 2*_t[52];
      ret[109] = _t[62];
      ret[119] = _t[62];
    }
    else if (*_tn == 54){
      ret[97] = _t[44];
      ret[107] = _t[44];
      ret[108] = 2*_t[53];
      ret[109] = _t[63];
      ret[119] = _t[63];
    }
    else if (*_tn == 55){
      ret[108] = 2*_t[54];
      ret[109] = _t[64];
      ret[119] = _t[64];
    }
    else if (*_tn == 56){
      ret[10] = _t[0];
      ret[21] = _t[1];
      ret[32] = _t[3];
      ret[43] = _t[6];
      ret[54] = _t[10];
      ret[65] = _t[15];
      ret[76] = _t[21];
      ret[87] = _t[28];
      ret[98] = _t[36];
      ret[109] = _t[45];
      ret[110] = _t[0];
      ret[111] = _t[1];
      ret[112] = _t[3];
      ret[113] = _t[6];
      ret[114] = _t[10];
      ret[115] = _t[15];
      ret[116] = _t[21];
      ret[117] = _t[28];
      ret[118] = _t[36];
      ret[119] = _t[45];
      ret[120] = 2*_t[55];
    }
    else if (*_tn == 57){
      ret[21] = _t[2];
      ret[32] = _t[4];
      ret[43] = _t[7];
      ret[54] = _t[11];
      ret[65] = _t[16];
      ret[76] = _t[22];
      ret[87] = _t[29];
      ret[98] = _t[37];
      ret[109] = _t[46];
      ret[111] = _t[2];
      ret[112] = _t[4];
      ret[113] = _t[7];
      ret[114] = _t[11];
      ret[115] = _t[16];
      ret[116] = _t[22];
      ret[117] = _t[29];
      ret[118] = _t[37];
      ret[119] = _t[46];
      ret[120] = 2*_t[56];
    }
    else if (*_tn == 58){
      ret[32] = _t[5];
      ret[43] = _t[8];
      ret[54] = _t[12];
      ret[65] = _t[17];
      ret[76] = _t[23];
      ret[87] = _t[30];
      ret[98] = _t[38];
      ret[109] = _t[47];
      ret[112] = _t[5];
      ret[113] = _t[8];
      ret[114] = _t[12];
      ret[115] = _t[17];
      ret[116] = _t[23];
      ret[117] = _t[30];
      ret[118] = _t[38];
      ret[119] = _t[47];
      ret[120] = 2*_t[57];
    }
    else if (*_tn == 59){
      ret[43] = _t[9];
      ret[54] = _t[13];
      ret[65] = _t[18];
      ret[76] = _t[24];
      ret[87] = _t[31];
      ret[98] = _t[39];
      ret[109] = _t[48];
      ret[113] = _t[9];
      ret[114] = _t[13];
      ret[115] = _t[18];
      ret[116] = _t[24];
      ret[117] = _t[31];
      ret[118] = _t[39];
      ret[119] = _t[48];
      ret[120] = 2*_t[58];
    }
    else if (*_tn == 60){
      ret[54] = _t[14];
      ret[65] = _t[19];
      ret[76] = _t[25];
      ret[87] = _t[32];
      ret[98] = _t[40];
      ret[109] = _t[49];
      ret[114] = _t[14];
      ret[115] = _t[19];
      ret[116] = _t[25];
      ret[117] = _t[32];
      ret[118] = _t[40];
      ret[119] = _t[49];
      ret[120] = 2*_t[59];
    }
    else if (*_tn == 61){
      ret[65] = _t[20];
      ret[76] = _t[26];
      ret[87] = _t[33];
      ret[98] = _t[41];
      ret[109] = _t[50];
      ret[115] = _t[20];
      ret[116] = _t[26];
      ret[117] = _t[33];
      ret[118] = _t[41];
      ret[119] = _t[50];
      ret[120] = 2*_t[60];
    }
    else if (*_tn == 62){
      ret[76] = _t[27];
      ret[87] = _t[34];
      ret[98] = _t[42];
      ret[109] = _t[51];
      ret[116] = _t[27];
      ret[117] = _t[34];
      ret[118] = _t[42];
      ret[119] = _t[51];
      ret[120] = 2*_t[61];
    }
    else if (*_tn == 63){
      ret[87] = _t[35];
      ret[98] = _t[43];
      ret[109] = _t[52];
      ret[117] = _t[35];
      ret[118] = _t[43];
      ret[119] = _t[52];
      ret[120] = 2*_t[62];
    }
    else if (*_tn == 64){
      ret[98] = _t[44];
      ret[109] = _t[53];
      ret[118] = _t[44];
      ret[119] = _t[53];
      ret[120] = 2*_t[63];
    }
    else if (*_tn == 65){
      ret[109] = _t[54];
      ret[119] = _t[54];
      ret[120] = 2*_t[64];
    }
    else if (*_tn == 66){
      ret[120] = 2*_t[65];
    }
    return;
  }
else if (*dm == 12) {
  if (*_tn== NA_INTEGER){
    ret[0]=4;
    ret[1]=5;
    ret[2]=4;
    ret[3]=5;
    ret[4]=5;
    ret[5]=4;
    ret[6]=5;
    ret[7]=5;
    ret[8]=5;
    ret[9]=4;
    ret[10]=5;
    ret[11]=5;
    ret[12]=5;
    ret[13]=5;
    ret[14]=4;
    ret[15]=5;
    ret[16]=5;
    ret[17]=5;
    ret[18]=5;
    ret[19]=5;
    ret[20]=4;
    ret[21]=5;
    ret[22]=5;
    ret[23]=5;
    ret[24]=5;
    ret[25]=5;
    ret[26]=5;
    ret[27]=4;
    ret[28]=5;
    ret[29]=5;
    ret[30]=5;
    ret[31]=5;
    ret[32]=5;
    ret[33]=5;
    ret[34]=5;
    ret[35]=4;
    ret[36]=5;
    ret[37]=5;
    ret[38]=5;
    ret[39]=5;
    ret[40]=5;
    ret[41]=5;
    ret[42]=5;
    ret[43]=5;
    ret[44]=4;
    ret[45]=5;
    ret[46]=5;
    ret[47]=5;
    ret[48]=5;
    ret[49]=5;
    ret[50]=5;
    ret[51]=5;
    ret[52]=5;
    ret[53]=5;
    ret[54]=4;
    ret[55]=5;
    ret[56]=5;
    ret[57]=5;
    ret[58]=5;
    ret[59]=5;
    ret[60]=5;
    ret[61]=5;
    ret[62]=5;
    ret[63]=5;
    ret[64]=5;
    ret[65]=4;
    ret[66]=5;
    ret[67]=5;
    ret[68]=5;
    ret[69]=5;
    ret[70]=5;
    ret[71]=5;
    ret[72]=5;
    ret[73]=5;
    ret[74]=5;
    ret[75]=5;
    ret[76]=5;
    ret[77]=4;
    return;  
}

if (*_tn == -2){
    ret[0] = 78;
    return;
  }
  else if (*_tn < -80 || *_tn > 78){
    ret[0] = NA_REAL;ret[1] = -1; //error("d(Omega^-1) derivative outside bounds");
  }
  else if (*length_theta != 78){
    ret[0] = NA_REAL;ret[1] = *length_theta;//error("requires vector with 78 arguments");
  }
    if (*_tn == 0){
      ret[0] = (_t[0]);
      ret[12] = _t[1];
      ret[13] = (_t[2]);
      ret[24] = _t[3];
      ret[25] = _t[4];
      ret[26] = (_t[5]);
      ret[36] = _t[6];
      ret[37] = _t[7];
      ret[38] = _t[8];
      ret[39] = (_t[9]);
      ret[48] = _t[10];
      ret[49] = _t[11];
      ret[50] = _t[12];
      ret[51] = _t[13];
      ret[52] = (_t[14]);
      ret[60] = _t[15];
      ret[61] = _t[16];
      ret[62] = _t[17];
      ret[63] = _t[18];
      ret[64] = _t[19];
      ret[65] = (_t[20]);
      ret[72] = _t[21];
      ret[73] = _t[22];
      ret[74] = _t[23];
      ret[75] = _t[24];
      ret[76] = _t[25];
      ret[77] = _t[26];
      ret[78] = (_t[27]);
      ret[84] = _t[28];
      ret[85] = _t[29];
      ret[86] = _t[30];
      ret[87] = _t[31];
      ret[88] = _t[32];
      ret[89] = _t[33];
      ret[90] = _t[34];
      ret[91] = (_t[35]);
      ret[96] = _t[36];
      ret[97] = _t[37];
      ret[98] = _t[38];
      ret[99] = _t[39];
      ret[100] = _t[40];
      ret[101] = _t[41];
      ret[102] = _t[42];
      ret[103] = _t[43];
      ret[104] = (_t[44]);
      ret[108] = _t[45];
      ret[109] = _t[46];
      ret[110] = _t[47];
      ret[111] = _t[48];
      ret[112] = _t[49];
      ret[113] = _t[50];
      ret[114] = _t[51];
      ret[115] = _t[52];
      ret[116] = _t[53];
      ret[117] = (_t[54]);
      ret[120] = _t[55];
      ret[121] = _t[56];
      ret[122] = _t[57];
      ret[123] = _t[58];
      ret[124] = _t[59];
      ret[125] = _t[60];
      ret[126] = _t[61];
      ret[127] = _t[62];
      ret[128] = _t[63];
      ret[129] = _t[64];
      ret[130] = (_t[65]);
      ret[132] = _t[66];
      ret[133] = _t[67];
      ret[134] = _t[68];
      ret[135] = _t[69];
      ret[136] = _t[70];
      ret[137] = _t[71];
      ret[138] = _t[72];
      ret[139] = _t[73];
      ret[140] = _t[74];
      ret[141] = _t[75];
      ret[142] = _t[76];
      ret[143] = (_t[77]);
    }
    else if (*_tn == -1){
      ret[0] = pow(_t[0], 2);
      ret[1] = _t[1]*_t[0];
      ret[2] = _t[0]*_t[3];
      ret[3] = _t[6]*_t[0];
      ret[4] = _t[0]*_t[10];
      ret[5] = _t[0]*_t[15];
      ret[6] = _t[0]*_t[21];
      ret[7] = _t[0]*_t[28];
      ret[8] = _t[0]*_t[36];
      ret[9] = _t[0]*_t[45];
      ret[10] = _t[0]*_t[55];
      ret[11] = _t[0]*_t[66];
      ret[12] = _t[1]*_t[0];
      ret[13] = pow(_t[1], 2)+pow(_t[2], 2);
      ret[14] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[15] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[16] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[17] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[18] = _t[1]*_t[21]+_t[2]*_t[22];
      ret[19] = _t[1]*_t[28]+_t[2]*_t[29];
      ret[20] = _t[1]*_t[36]+_t[2]*_t[37];
      ret[21] = _t[1]*_t[45]+_t[2]*_t[46];
      ret[22] = _t[1]*_t[55]+_t[2]*_t[56];
      ret[23] = _t[1]*_t[66]+_t[2]*_t[67];
      ret[24] = _t[0]*_t[3];
      ret[25] = _t[1]*_t[3]+_t[4]*_t[2];
      ret[26] = pow(_t[3], 2)+pow(_t[4], 2)+pow(_t[5], 2);
      ret[27] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[28] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[29] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[30] = _t[3]*_t[21]+_t[4]*_t[22]+_t[5]*_t[23];
      ret[31] = _t[3]*_t[28]+_t[4]*_t[29]+_t[5]*_t[30];
      ret[32] = _t[3]*_t[36]+_t[4]*_t[37]+_t[5]*_t[38];
      ret[33] = _t[3]*_t[45]+_t[4]*_t[46]+_t[5]*_t[47];
      ret[34] = _t[3]*_t[55]+_t[4]*_t[56]+_t[5]*_t[57];
      ret[35] = _t[3]*_t[66]+_t[4]*_t[67]+_t[5]*_t[68];
      ret[36] = _t[6]*_t[0];
      ret[37] = _t[6]*_t[1]+_t[7]*_t[2];
      ret[38] = _t[4]*_t[7]+_t[6]*_t[3]+_t[8]*_t[5];
      ret[39] = pow(_t[6], 2)+pow(_t[7], 2)+pow(_t[8], 2)+pow(_t[9], 2);
      ret[40] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[41] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[42] = _t[6]*_t[21]+_t[7]*_t[22]+_t[8]*_t[23]+_t[9]*_t[24];
      ret[43] = _t[6]*_t[28]+_t[7]*_t[29]+_t[8]*_t[30]+_t[9]*_t[31];
      ret[44] = _t[6]*_t[36]+_t[7]*_t[37]+_t[8]*_t[38]+_t[9]*_t[39];
      ret[45] = _t[6]*_t[45]+_t[7]*_t[46]+_t[8]*_t[47]+_t[9]*_t[48];
      ret[46] = _t[6]*_t[55]+_t[7]*_t[56]+_t[8]*_t[57]+_t[9]*_t[58];
      ret[47] = _t[6]*_t[66]+_t[7]*_t[67]+_t[8]*_t[68]+_t[9]*_t[69];
      ret[48] = _t[0]*_t[10];
      ret[49] = _t[1]*_t[10]+_t[2]*_t[11];
      ret[50] = _t[3]*_t[10]+_t[4]*_t[11]+_t[5]*_t[12];
      ret[51] = _t[6]*_t[10]+_t[7]*_t[11]+_t[8]*_t[12]+_t[9]*_t[13];
      ret[52] = pow(_t[10], 2)+pow(_t[11], 2)+pow(_t[12], 2)+pow(_t[13], 2)+pow(_t[14], 2);
      ret[53] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[54] = _t[21]*_t[10]+_t[22]*_t[11]+_t[23]*_t[12]+_t[24]*_t[13]+_t[25]*_t[14];
      ret[55] = _t[28]*_t[10]+_t[29]*_t[11]+_t[30]*_t[12]+_t[31]*_t[13]+_t[32]*_t[14];
      ret[56] = _t[36]*_t[10]+_t[37]*_t[11]+_t[38]*_t[12]+_t[39]*_t[13]+_t[40]*_t[14];
      ret[57] = _t[45]*_t[10]+_t[46]*_t[11]+_t[47]*_t[12]+_t[48]*_t[13]+_t[49]*_t[14];
      ret[58] = _t[55]*_t[10]+_t[56]*_t[11]+_t[57]*_t[12]+_t[58]*_t[13]+_t[59]*_t[14];
      ret[59] = _t[66]*_t[10]+_t[67]*_t[11]+_t[68]*_t[12]+_t[69]*_t[13]+_t[70]*_t[14];
      ret[60] = _t[0]*_t[15];
      ret[61] = _t[1]*_t[15]+_t[2]*_t[16];
      ret[62] = _t[3]*_t[15]+_t[4]*_t[16]+_t[5]*_t[17];
      ret[63] = _t[6]*_t[15]+_t[7]*_t[16]+_t[8]*_t[17]+_t[9]*_t[18];
      ret[64] = _t[11]*_t[16]+_t[12]*_t[17]+_t[13]*_t[18]+_t[15]*_t[10]+_t[19]*_t[14];
      ret[65] = pow(_t[15], 2)+pow(_t[16], 2)+pow(_t[17], 2)+pow(_t[18], 2)+pow(_t[19], 2)+pow(_t[20], 2);
      ret[66] = _t[21]*_t[15]+_t[22]*_t[16]+_t[23]*_t[17]+_t[24]*_t[18]+_t[25]*_t[19]+_t[26]*_t[20];
      ret[67] = _t[20]*_t[33]+_t[28]*_t[15]+_t[29]*_t[16]+_t[30]*_t[17]+_t[31]*_t[18]+_t[32]*_t[19];
      ret[68] = _t[36]*_t[15]+_t[37]*_t[16]+_t[38]*_t[17]+_t[39]*_t[18]+_t[40]*_t[19]+_t[41]*_t[20];
      ret[69] = _t[45]*_t[15]+_t[46]*_t[16]+_t[47]*_t[17]+_t[48]*_t[18]+_t[49]*_t[19]+_t[50]*_t[20];
      ret[70] = _t[20]*_t[60]+_t[55]*_t[15]+_t[56]*_t[16]+_t[57]*_t[17]+_t[58]*_t[18]+_t[59]*_t[19];
      ret[71] = _t[66]*_t[15]+_t[67]*_t[16]+_t[68]*_t[17]+_t[69]*_t[18]+_t[70]*_t[19]+_t[71]*_t[20];
      ret[72] = _t[0]*_t[21];
      ret[73] = _t[1]*_t[21]+_t[2]*_t[22];
      ret[74] = _t[3]*_t[21]+_t[4]*_t[22]+_t[5]*_t[23];
      ret[75] = _t[6]*_t[21]+_t[7]*_t[22]+_t[8]*_t[23]+_t[9]*_t[24];
      ret[76] = _t[21]*_t[10]+_t[22]*_t[11]+_t[23]*_t[12]+_t[24]*_t[13]+_t[25]*_t[14];
      ret[77] = _t[21]*_t[15]+_t[22]*_t[16]+_t[23]*_t[17]+_t[24]*_t[18]+_t[25]*_t[19]+_t[26]*_t[20];
      ret[78] = pow(_t[21], 2)+pow(_t[22], 2)+pow(_t[23], 2)+pow(_t[24], 2)+pow(_t[25], 2)+pow(_t[26], 2)+pow(_t[27], 2);
      ret[79] = _t[21]*_t[28]+_t[23]*_t[30]+_t[24]*_t[31]+_t[25]*_t[32]+_t[26]*_t[33]+_t[27]*_t[34]+_t[29]*_t[22];
      ret[80] = _t[21]*_t[36]+_t[22]*_t[37]+_t[23]*_t[38]+_t[24]*_t[39]+_t[40]*_t[25]+_t[41]*_t[26]+_t[42]*_t[27];
      ret[81] = _t[45]*_t[21]+_t[46]*_t[22]+_t[47]*_t[23]+_t[48]*_t[24]+_t[49]*_t[25]+_t[50]*_t[26]+_t[51]*_t[27];
      ret[82] = _t[26]*_t[60]+_t[27]*_t[61]+_t[55]*_t[21]+_t[56]*_t[22]+_t[57]*_t[23]+_t[58]*_t[24]+_t[59]*_t[25];
      ret[83] = _t[21]*_t[66]+_t[22]*_t[67]+_t[23]*_t[68]+_t[24]*_t[69]+_t[70]*_t[25]+_t[71]*_t[26]+_t[72]*_t[27];
      ret[84] = _t[0]*_t[28];
      ret[85] = _t[1]*_t[28]+_t[2]*_t[29];
      ret[86] = _t[3]*_t[28]+_t[4]*_t[29]+_t[5]*_t[30];
      ret[87] = _t[6]*_t[28]+_t[7]*_t[29]+_t[8]*_t[30]+_t[9]*_t[31];
      ret[88] = _t[28]*_t[10]+_t[29]*_t[11]+_t[30]*_t[12]+_t[31]*_t[13]+_t[32]*_t[14];
      ret[89] = _t[20]*_t[33]+_t[28]*_t[15]+_t[29]*_t[16]+_t[30]*_t[17]+_t[31]*_t[18]+_t[32]*_t[19];
      ret[90] = _t[21]*_t[28]+_t[23]*_t[30]+_t[24]*_t[31]+_t[25]*_t[32]+_t[26]*_t[33]+_t[27]*_t[34]+_t[29]*_t[22];
      ret[91] = pow(_t[28], 2)+pow(_t[29], 2)+pow(_t[30], 2)+pow(_t[31], 2)+pow(_t[32], 2)+pow(_t[33], 2)+pow(_t[34], 2)+pow(_t[35], 2);
      ret[92] = _t[28]*_t[36]+_t[29]*_t[37]+_t[38]*_t[30]+_t[39]*_t[31]+_t[40]*_t[32]+_t[41]*_t[33]+_t[42]*_t[34]+_t[43]*_t[35];
      ret[93] = _t[45]*_t[28]+_t[46]*_t[29]+_t[47]*_t[30]+_t[48]*_t[31]+_t[49]*_t[32]+_t[50]*_t[33]+_t[51]*_t[34]+_t[52]*_t[35];
      ret[94] = _t[33]*_t[60]+_t[34]*_t[61]+_t[35]*_t[62]+_t[55]*_t[28]+_t[56]*_t[29]+_t[57]*_t[30]+_t[58]*_t[31]+_t[59]*_t[32];
      ret[95] = _t[28]*_t[66]+_t[29]*_t[67]+_t[30]*_t[68]+_t[31]*_t[69]+_t[70]*_t[32]+_t[71]*_t[33]+_t[72]*_t[34]+_t[73]*_t[35];
      ret[96] = _t[0]*_t[36];
      ret[97] = _t[1]*_t[36]+_t[2]*_t[37];
      ret[98] = _t[3]*_t[36]+_t[4]*_t[37]+_t[5]*_t[38];
      ret[99] = _t[6]*_t[36]+_t[7]*_t[37]+_t[8]*_t[38]+_t[9]*_t[39];
      ret[100] = _t[36]*_t[10]+_t[37]*_t[11]+_t[38]*_t[12]+_t[39]*_t[13]+_t[40]*_t[14];
      ret[101] = _t[36]*_t[15]+_t[37]*_t[16]+_t[38]*_t[17]+_t[39]*_t[18]+_t[40]*_t[19]+_t[41]*_t[20];
      ret[102] = _t[21]*_t[36]+_t[22]*_t[37]+_t[23]*_t[38]+_t[24]*_t[39]+_t[40]*_t[25]+_t[41]*_t[26]+_t[42]*_t[27];
      ret[103] = _t[28]*_t[36]+_t[29]*_t[37]+_t[38]*_t[30]+_t[39]*_t[31]+_t[40]*_t[32]+_t[41]*_t[33]+_t[42]*_t[34]+_t[43]*_t[35];
      ret[104] = pow(_t[36], 2)+pow(_t[37], 2)+pow(_t[38], 2)+pow(_t[39], 2)+pow(_t[40], 2)+pow(_t[41], 2)+pow(_t[42], 2)+pow(_t[43], 2)+pow(_t[44], 2);
      ret[105] = _t[41]*_t[50]+_t[42]*_t[51]+_t[43]*_t[52]+_t[44]*_t[53]+_t[45]*_t[36]+_t[46]*_t[37]+_t[47]*_t[38]+_t[48]*_t[39]+_t[49]*_t[40];
      ret[106] = _t[40]*_t[59]+_t[41]*_t[60]+_t[42]*_t[61]+_t[43]*_t[62]+_t[44]*_t[63]+_t[55]*_t[36]+_t[56]*_t[37]+_t[57]*_t[38]+_t[58]*_t[39];
      ret[107] = _t[36]*_t[66]+_t[37]*_t[67]+_t[38]*_t[68]+_t[39]*_t[69]+_t[40]*_t[70]+_t[41]*_t[71]+_t[42]*_t[72]+_t[43]*_t[73]+_t[44]*_t[74];
      ret[108] = _t[0]*_t[45];
      ret[109] = _t[1]*_t[45]+_t[2]*_t[46];
      ret[110] = _t[3]*_t[45]+_t[4]*_t[46]+_t[5]*_t[47];
      ret[111] = _t[6]*_t[45]+_t[7]*_t[46]+_t[8]*_t[47]+_t[9]*_t[48];
      ret[112] = _t[45]*_t[10]+_t[46]*_t[11]+_t[47]*_t[12]+_t[48]*_t[13]+_t[49]*_t[14];
      ret[113] = _t[45]*_t[15]+_t[46]*_t[16]+_t[47]*_t[17]+_t[48]*_t[18]+_t[49]*_t[19]+_t[50]*_t[20];
      ret[114] = _t[45]*_t[21]+_t[46]*_t[22]+_t[47]*_t[23]+_t[48]*_t[24]+_t[49]*_t[25]+_t[50]*_t[26]+_t[51]*_t[27];
      ret[115] = _t[45]*_t[28]+_t[46]*_t[29]+_t[47]*_t[30]+_t[48]*_t[31]+_t[49]*_t[32]+_t[50]*_t[33]+_t[51]*_t[34]+_t[52]*_t[35];
      ret[116] = _t[41]*_t[50]+_t[42]*_t[51]+_t[43]*_t[52]+_t[44]*_t[53]+_t[45]*_t[36]+_t[46]*_t[37]+_t[47]*_t[38]+_t[48]*_t[39]+_t[49]*_t[40];
      ret[117] = pow(_t[45], 2)+pow(_t[46], 2)+pow(_t[47], 2)+pow(_t[48], 2)+pow(_t[49], 2)+pow(_t[50], 2)+pow(_t[51], 2)+pow(_t[52], 2)+pow(_t[53], 2)+pow(_t[54], 2);
      ret[118] = _t[45]*_t[55]+_t[46]*_t[56]+_t[47]*_t[57]+_t[48]*_t[58]+_t[49]*_t[59]+_t[50]*_t[60]+_t[51]*_t[61]+_t[52]*_t[62]+_t[53]*_t[63]+_t[54]*_t[64];
      ret[119] = _t[45]*_t[66]+_t[46]*_t[67]+_t[47]*_t[68]+_t[48]*_t[69]+_t[49]*_t[70]+_t[71]*_t[50]+_t[72]*_t[51]+_t[73]*_t[52]+_t[74]*_t[53]+_t[75]*_t[54];
      ret[120] = _t[0]*_t[55];
      ret[121] = _t[1]*_t[55]+_t[2]*_t[56];
      ret[122] = _t[3]*_t[55]+_t[4]*_t[56]+_t[5]*_t[57];
      ret[123] = _t[6]*_t[55]+_t[7]*_t[56]+_t[8]*_t[57]+_t[9]*_t[58];
      ret[124] = _t[55]*_t[10]+_t[56]*_t[11]+_t[57]*_t[12]+_t[58]*_t[13]+_t[59]*_t[14];
      ret[125] = _t[20]*_t[60]+_t[55]*_t[15]+_t[56]*_t[16]+_t[57]*_t[17]+_t[58]*_t[18]+_t[59]*_t[19];
      ret[126] = _t[26]*_t[60]+_t[27]*_t[61]+_t[55]*_t[21]+_t[56]*_t[22]+_t[57]*_t[23]+_t[58]*_t[24]+_t[59]*_t[25];
      ret[127] = _t[33]*_t[60]+_t[34]*_t[61]+_t[35]*_t[62]+_t[55]*_t[28]+_t[56]*_t[29]+_t[57]*_t[30]+_t[58]*_t[31]+_t[59]*_t[32];
      ret[128] = _t[40]*_t[59]+_t[41]*_t[60]+_t[42]*_t[61]+_t[43]*_t[62]+_t[44]*_t[63]+_t[55]*_t[36]+_t[56]*_t[37]+_t[57]*_t[38]+_t[58]*_t[39];
      ret[129] = _t[45]*_t[55]+_t[46]*_t[56]+_t[47]*_t[57]+_t[48]*_t[58]+_t[49]*_t[59]+_t[50]*_t[60]+_t[51]*_t[61]+_t[52]*_t[62]+_t[53]*_t[63]+_t[54]*_t[64];
      ret[130] = pow(_t[55], 2)+pow(_t[56], 2)+pow(_t[57], 2)+pow(_t[58], 2)+pow(_t[59], 2)+pow(_t[60], 2)+pow(_t[61], 2)+pow(_t[62], 2)+pow(_t[63], 2)+pow(_t[64], 2)+pow(_t[65], 2);
      ret[131] = _t[55]*_t[66]+_t[56]*_t[67]+_t[57]*_t[68]+_t[58]*_t[69]+_t[70]*_t[59]+_t[71]*_t[60]+_t[72]*_t[61]+_t[73]*_t[62]+_t[74]*_t[63]+_t[75]*_t[64]+_t[76]*_t[65];
      ret[132] = _t[0]*_t[66];
      ret[133] = _t[1]*_t[66]+_t[2]*_t[67];
      ret[134] = _t[3]*_t[66]+_t[4]*_t[67]+_t[5]*_t[68];
      ret[135] = _t[6]*_t[66]+_t[7]*_t[67]+_t[8]*_t[68]+_t[9]*_t[69];
      ret[136] = _t[66]*_t[10]+_t[67]*_t[11]+_t[68]*_t[12]+_t[69]*_t[13]+_t[70]*_t[14];
      ret[137] = _t[66]*_t[15]+_t[67]*_t[16]+_t[68]*_t[17]+_t[69]*_t[18]+_t[70]*_t[19]+_t[71]*_t[20];
      ret[138] = _t[21]*_t[66]+_t[22]*_t[67]+_t[23]*_t[68]+_t[24]*_t[69]+_t[70]*_t[25]+_t[71]*_t[26]+_t[72]*_t[27];
      ret[139] = _t[28]*_t[66]+_t[29]*_t[67]+_t[30]*_t[68]+_t[31]*_t[69]+_t[70]*_t[32]+_t[71]*_t[33]+_t[72]*_t[34]+_t[73]*_t[35];
      ret[140] = _t[36]*_t[66]+_t[37]*_t[67]+_t[38]*_t[68]+_t[39]*_t[69]+_t[40]*_t[70]+_t[41]*_t[71]+_t[42]*_t[72]+_t[43]*_t[73]+_t[44]*_t[74];
      ret[141] = _t[45]*_t[66]+_t[46]*_t[67]+_t[47]*_t[68]+_t[48]*_t[69]+_t[49]*_t[70]+_t[71]*_t[50]+_t[72]*_t[51]+_t[73]*_t[52]+_t[74]*_t[53]+_t[75]*_t[54];
      ret[142] = _t[55]*_t[66]+_t[56]*_t[67]+_t[57]*_t[68]+_t[58]*_t[69]+_t[70]*_t[59]+_t[71]*_t[60]+_t[72]*_t[61]+_t[73]*_t[62]+_t[74]*_t[63]+_t[75]*_t[64]+_t[76]*_t[65];
      ret[143] = pow(_t[66], 2)+pow(_t[67], 2)+pow(_t[68], 2)+pow(_t[69], 2)+pow(_t[70], 2)+pow(_t[71], 2)+pow(_t[72], 2)+pow(_t[73], 2)+pow(_t[74], 2)+pow(_t[75], 2)+pow(_t[76], 2)+pow(_t[77], 2);
    }
    else if (*_tn == 1){
      ret[0] = 2*_t[0];
      ret[1] = _t[1];
      ret[2] = _t[3];
      ret[3] = _t[6];
      ret[4] = _t[10];
      ret[5] = _t[15];
      ret[6] = _t[21];
      ret[7] = _t[28];
      ret[8] = _t[36];
      ret[9] = _t[45];
      ret[10] = _t[55];
      ret[11] = _t[66];
      ret[12] = _t[1];
      ret[24] = _t[3];
      ret[36] = _t[6];
      ret[48] = _t[10];
      ret[60] = _t[15];
      ret[72] = _t[21];
      ret[84] = _t[28];
      ret[96] = _t[36];
      ret[108] = _t[45];
      ret[120] = _t[55];
      ret[132] = _t[66];
    }
    else if (*_tn == 2){
      ret[1] = _t[0];
      ret[12] = _t[0];
      ret[13] = 2*_t[1];
      ret[14] = _t[3];
      ret[15] = _t[6];
      ret[16] = _t[10];
      ret[17] = _t[15];
      ret[18] = _t[21];
      ret[19] = _t[28];
      ret[20] = _t[36];
      ret[21] = _t[45];
      ret[22] = _t[55];
      ret[23] = _t[66];
      ret[25] = _t[3];
      ret[37] = _t[6];
      ret[49] = _t[10];
      ret[61] = _t[15];
      ret[73] = _t[21];
      ret[85] = _t[28];
      ret[97] = _t[36];
      ret[109] = _t[45];
      ret[121] = _t[55];
      ret[133] = _t[66];
    }
    else if (*_tn == 3){
      ret[13] = 2*_t[2];
      ret[14] = _t[4];
      ret[15] = _t[7];
      ret[16] = _t[11];
      ret[17] = _t[16];
      ret[18] = _t[22];
      ret[19] = _t[29];
      ret[20] = _t[37];
      ret[21] = _t[46];
      ret[22] = _t[56];
      ret[23] = _t[67];
      ret[25] = _t[4];
      ret[37] = _t[7];
      ret[49] = _t[11];
      ret[61] = _t[16];
      ret[73] = _t[22];
      ret[85] = _t[29];
      ret[97] = _t[37];
      ret[109] = _t[46];
      ret[121] = _t[56];
      ret[133] = _t[67];
    }
    else if (*_tn == 4){
      ret[2] = _t[0];
      ret[14] = _t[1];
      ret[24] = _t[0];
      ret[25] = _t[1];
      ret[26] = 2*_t[3];
      ret[27] = _t[6];
      ret[28] = _t[10];
      ret[29] = _t[15];
      ret[30] = _t[21];
      ret[31] = _t[28];
      ret[32] = _t[36];
      ret[33] = _t[45];
      ret[34] = _t[55];
      ret[35] = _t[66];
      ret[38] = _t[6];
      ret[50] = _t[10];
      ret[62] = _t[15];
      ret[74] = _t[21];
      ret[86] = _t[28];
      ret[98] = _t[36];
      ret[110] = _t[45];
      ret[122] = _t[55];
      ret[134] = _t[66];
    }
    else if (*_tn == 5){
      ret[14] = _t[2];
      ret[25] = _t[2];
      ret[26] = 2*_t[4];
      ret[27] = _t[7];
      ret[28] = _t[11];
      ret[29] = _t[16];
      ret[30] = _t[22];
      ret[31] = _t[29];
      ret[32] = _t[37];
      ret[33] = _t[46];
      ret[34] = _t[56];
      ret[35] = _t[67];
      ret[38] = _t[7];
      ret[50] = _t[11];
      ret[62] = _t[16];
      ret[74] = _t[22];
      ret[86] = _t[29];
      ret[98] = _t[37];
      ret[110] = _t[46];
      ret[122] = _t[56];
      ret[134] = _t[67];
    }
    else if (*_tn == 6){
      ret[26] = 2*_t[5];
      ret[27] = _t[8];
      ret[28] = _t[12];
      ret[29] = _t[17];
      ret[30] = _t[23];
      ret[31] = _t[30];
      ret[32] = _t[38];
      ret[33] = _t[47];
      ret[34] = _t[57];
      ret[35] = _t[68];
      ret[38] = _t[8];
      ret[50] = _t[12];
      ret[62] = _t[17];
      ret[74] = _t[23];
      ret[86] = _t[30];
      ret[98] = _t[38];
      ret[110] = _t[47];
      ret[122] = _t[57];
      ret[134] = _t[68];
    }
    else if (*_tn == 7){
      ret[3] = _t[0];
      ret[15] = _t[1];
      ret[27] = _t[3];
      ret[36] = _t[0];
      ret[37] = _t[1];
      ret[38] = _t[3];
      ret[39] = 2*_t[6];
      ret[40] = _t[10];
      ret[41] = _t[15];
      ret[42] = _t[21];
      ret[43] = _t[28];
      ret[44] = _t[36];
      ret[45] = _t[45];
      ret[46] = _t[55];
      ret[47] = _t[66];
      ret[51] = _t[10];
      ret[63] = _t[15];
      ret[75] = _t[21];
      ret[87] = _t[28];
      ret[99] = _t[36];
      ret[111] = _t[45];
      ret[123] = _t[55];
      ret[135] = _t[66];
    }
    else if (*_tn == 8){
      ret[15] = _t[2];
      ret[27] = _t[4];
      ret[37] = _t[2];
      ret[38] = _t[4];
      ret[39] = 2*_t[7];
      ret[40] = _t[11];
      ret[41] = _t[16];
      ret[42] = _t[22];
      ret[43] = _t[29];
      ret[44] = _t[37];
      ret[45] = _t[46];
      ret[46] = _t[56];
      ret[47] = _t[67];
      ret[51] = _t[11];
      ret[63] = _t[16];
      ret[75] = _t[22];
      ret[87] = _t[29];
      ret[99] = _t[37];
      ret[111] = _t[46];
      ret[123] = _t[56];
      ret[135] = _t[67];
    }
    else if (*_tn == 9){
      ret[27] = _t[5];
      ret[38] = _t[5];
      ret[39] = 2*_t[8];
      ret[40] = _t[12];
      ret[41] = _t[17];
      ret[42] = _t[23];
      ret[43] = _t[30];
      ret[44] = _t[38];
      ret[45] = _t[47];
      ret[46] = _t[57];
      ret[47] = _t[68];
      ret[51] = _t[12];
      ret[63] = _t[17];
      ret[75] = _t[23];
      ret[87] = _t[30];
      ret[99] = _t[38];
      ret[111] = _t[47];
      ret[123] = _t[57];
      ret[135] = _t[68];
    }
    else if (*_tn == 10){
      ret[39] = 2*_t[9];
      ret[40] = _t[13];
      ret[41] = _t[18];
      ret[42] = _t[24];
      ret[43] = _t[31];
      ret[44] = _t[39];
      ret[45] = _t[48];
      ret[46] = _t[58];
      ret[47] = _t[69];
      ret[51] = _t[13];
      ret[63] = _t[18];
      ret[75] = _t[24];
      ret[87] = _t[31];
      ret[99] = _t[39];
      ret[111] = _t[48];
      ret[123] = _t[58];
      ret[135] = _t[69];
    }
    else if (*_tn == 11){
      ret[4] = _t[0];
      ret[16] = _t[1];
      ret[28] = _t[3];
      ret[40] = _t[6];
      ret[48] = _t[0];
      ret[49] = _t[1];
      ret[50] = _t[3];
      ret[51] = _t[6];
      ret[52] = 2*_t[10];
      ret[53] = _t[15];
      ret[54] = _t[21];
      ret[55] = _t[28];
      ret[56] = _t[36];
      ret[57] = _t[45];
      ret[58] = _t[55];
      ret[59] = _t[66];
      ret[64] = _t[15];
      ret[76] = _t[21];
      ret[88] = _t[28];
      ret[100] = _t[36];
      ret[112] = _t[45];
      ret[124] = _t[55];
      ret[136] = _t[66];
    }
    else if (*_tn == 12){
      ret[16] = _t[2];
      ret[28] = _t[4];
      ret[40] = _t[7];
      ret[49] = _t[2];
      ret[50] = _t[4];
      ret[51] = _t[7];
      ret[52] = 2*_t[11];
      ret[53] = _t[16];
      ret[54] = _t[22];
      ret[55] = _t[29];
      ret[56] = _t[37];
      ret[57] = _t[46];
      ret[58] = _t[56];
      ret[59] = _t[67];
      ret[64] = _t[16];
      ret[76] = _t[22];
      ret[88] = _t[29];
      ret[100] = _t[37];
      ret[112] = _t[46];
      ret[124] = _t[56];
      ret[136] = _t[67];
    }
    else if (*_tn == 13){
      ret[28] = _t[5];
      ret[40] = _t[8];
      ret[50] = _t[5];
      ret[51] = _t[8];
      ret[52] = 2*_t[12];
      ret[53] = _t[17];
      ret[54] = _t[23];
      ret[55] = _t[30];
      ret[56] = _t[38];
      ret[57] = _t[47];
      ret[58] = _t[57];
      ret[59] = _t[68];
      ret[64] = _t[17];
      ret[76] = _t[23];
      ret[88] = _t[30];
      ret[100] = _t[38];
      ret[112] = _t[47];
      ret[124] = _t[57];
      ret[136] = _t[68];
    }
    else if (*_tn == 14){
      ret[40] = _t[9];
      ret[51] = _t[9];
      ret[52] = 2*_t[13];
      ret[53] = _t[18];
      ret[54] = _t[24];
      ret[55] = _t[31];
      ret[56] = _t[39];
      ret[57] = _t[48];
      ret[58] = _t[58];
      ret[59] = _t[69];
      ret[64] = _t[18];
      ret[76] = _t[24];
      ret[88] = _t[31];
      ret[100] = _t[39];
      ret[112] = _t[48];
      ret[124] = _t[58];
      ret[136] = _t[69];
    }
    else if (*_tn == 15){
      ret[52] = 2*_t[14];
      ret[53] = _t[19];
      ret[54] = _t[25];
      ret[55] = _t[32];
      ret[56] = _t[40];
      ret[57] = _t[49];
      ret[58] = _t[59];
      ret[59] = _t[70];
      ret[64] = _t[19];
      ret[76] = _t[25];
      ret[88] = _t[32];
      ret[100] = _t[40];
      ret[112] = _t[49];
      ret[124] = _t[59];
      ret[136] = _t[70];
    }
    else if (*_tn == 16){
      ret[5] = _t[0];
      ret[17] = _t[1];
      ret[29] = _t[3];
      ret[41] = _t[6];
      ret[53] = _t[10];
      ret[60] = _t[0];
      ret[61] = _t[1];
      ret[62] = _t[3];
      ret[63] = _t[6];
      ret[64] = _t[10];
      ret[65] = 2*_t[15];
      ret[66] = _t[21];
      ret[67] = _t[28];
      ret[68] = _t[36];
      ret[69] = _t[45];
      ret[70] = _t[55];
      ret[71] = _t[66];
      ret[77] = _t[21];
      ret[89] = _t[28];
      ret[101] = _t[36];
      ret[113] = _t[45];
      ret[125] = _t[55];
      ret[137] = _t[66];
    }
    else if (*_tn == 17){
      ret[17] = _t[2];
      ret[29] = _t[4];
      ret[41] = _t[7];
      ret[53] = _t[11];
      ret[61] = _t[2];
      ret[62] = _t[4];
      ret[63] = _t[7];
      ret[64] = _t[11];
      ret[65] = 2*_t[16];
      ret[66] = _t[22];
      ret[67] = _t[29];
      ret[68] = _t[37];
      ret[69] = _t[46];
      ret[70] = _t[56];
      ret[71] = _t[67];
      ret[77] = _t[22];
      ret[89] = _t[29];
      ret[101] = _t[37];
      ret[113] = _t[46];
      ret[125] = _t[56];
      ret[137] = _t[67];
    }
    else if (*_tn == 18){
      ret[29] = _t[5];
      ret[41] = _t[8];
      ret[53] = _t[12];
      ret[62] = _t[5];
      ret[63] = _t[8];
      ret[64] = _t[12];
      ret[65] = 2*_t[17];
      ret[66] = _t[23];
      ret[67] = _t[30];
      ret[68] = _t[38];
      ret[69] = _t[47];
      ret[70] = _t[57];
      ret[71] = _t[68];
      ret[77] = _t[23];
      ret[89] = _t[30];
      ret[101] = _t[38];
      ret[113] = _t[47];
      ret[125] = _t[57];
      ret[137] = _t[68];
    }
    else if (*_tn == 19){
      ret[41] = _t[9];
      ret[53] = _t[13];
      ret[63] = _t[9];
      ret[64] = _t[13];
      ret[65] = 2*_t[18];
      ret[66] = _t[24];
      ret[67] = _t[31];
      ret[68] = _t[39];
      ret[69] = _t[48];
      ret[70] = _t[58];
      ret[71] = _t[69];
      ret[77] = _t[24];
      ret[89] = _t[31];
      ret[101] = _t[39];
      ret[113] = _t[48];
      ret[125] = _t[58];
      ret[137] = _t[69];
    }
    else if (*_tn == 20){
      ret[53] = _t[14];
      ret[64] = _t[14];
      ret[65] = 2*_t[19];
      ret[66] = _t[25];
      ret[67] = _t[32];
      ret[68] = _t[40];
      ret[69] = _t[49];
      ret[70] = _t[59];
      ret[71] = _t[70];
      ret[77] = _t[25];
      ret[89] = _t[32];
      ret[101] = _t[40];
      ret[113] = _t[49];
      ret[125] = _t[59];
      ret[137] = _t[70];
    }
    else if (*_tn == 21){
      ret[65] = 2*_t[20];
      ret[66] = _t[26];
      ret[67] = _t[33];
      ret[68] = _t[41];
      ret[69] = _t[50];
      ret[70] = _t[60];
      ret[71] = _t[71];
      ret[77] = _t[26];
      ret[89] = _t[33];
      ret[101] = _t[41];
      ret[113] = _t[50];
      ret[125] = _t[60];
      ret[137] = _t[71];
    }
    else if (*_tn == 22){
      ret[6] = _t[0];
      ret[18] = _t[1];
      ret[30] = _t[3];
      ret[42] = _t[6];
      ret[54] = _t[10];
      ret[66] = _t[15];
      ret[72] = _t[0];
      ret[73] = _t[1];
      ret[74] = _t[3];
      ret[75] = _t[6];
      ret[76] = _t[10];
      ret[77] = _t[15];
      ret[78] = 2*_t[21];
      ret[79] = _t[28];
      ret[80] = _t[36];
      ret[81] = _t[45];
      ret[82] = _t[55];
      ret[83] = _t[66];
      ret[90] = _t[28];
      ret[102] = _t[36];
      ret[114] = _t[45];
      ret[126] = _t[55];
      ret[138] = _t[66];
    }
    else if (*_tn == 23){
      ret[18] = _t[2];
      ret[30] = _t[4];
      ret[42] = _t[7];
      ret[54] = _t[11];
      ret[66] = _t[16];
      ret[73] = _t[2];
      ret[74] = _t[4];
      ret[75] = _t[7];
      ret[76] = _t[11];
      ret[77] = _t[16];
      ret[78] = 2*_t[22];
      ret[79] = _t[29];
      ret[80] = _t[37];
      ret[81] = _t[46];
      ret[82] = _t[56];
      ret[83] = _t[67];
      ret[90] = _t[29];
      ret[102] = _t[37];
      ret[114] = _t[46];
      ret[126] = _t[56];
      ret[138] = _t[67];
    }
    else if (*_tn == 24){
      ret[30] = _t[5];
      ret[42] = _t[8];
      ret[54] = _t[12];
      ret[66] = _t[17];
      ret[74] = _t[5];
      ret[75] = _t[8];
      ret[76] = _t[12];
      ret[77] = _t[17];
      ret[78] = 2*_t[23];
      ret[79] = _t[30];
      ret[80] = _t[38];
      ret[81] = _t[47];
      ret[82] = _t[57];
      ret[83] = _t[68];
      ret[90] = _t[30];
      ret[102] = _t[38];
      ret[114] = _t[47];
      ret[126] = _t[57];
      ret[138] = _t[68];
    }
    else if (*_tn == 25){
      ret[42] = _t[9];
      ret[54] = _t[13];
      ret[66] = _t[18];
      ret[75] = _t[9];
      ret[76] = _t[13];
      ret[77] = _t[18];
      ret[78] = 2*_t[24];
      ret[79] = _t[31];
      ret[80] = _t[39];
      ret[81] = _t[48];
      ret[82] = _t[58];
      ret[83] = _t[69];
      ret[90] = _t[31];
      ret[102] = _t[39];
      ret[114] = _t[48];
      ret[126] = _t[58];
      ret[138] = _t[69];
    }
    else if (*_tn == 26){
      ret[54] = _t[14];
      ret[66] = _t[19];
      ret[76] = _t[14];
      ret[77] = _t[19];
      ret[78] = 2*_t[25];
      ret[79] = _t[32];
      ret[80] = _t[40];
      ret[81] = _t[49];
      ret[82] = _t[59];
      ret[83] = _t[70];
      ret[90] = _t[32];
      ret[102] = _t[40];
      ret[114] = _t[49];
      ret[126] = _t[59];
      ret[138] = _t[70];
    }
    else if (*_tn == 27){
      ret[66] = _t[20];
      ret[77] = _t[20];
      ret[78] = 2*_t[26];
      ret[79] = _t[33];
      ret[80] = _t[41];
      ret[81] = _t[50];
      ret[82] = _t[60];
      ret[83] = _t[71];
      ret[90] = _t[33];
      ret[102] = _t[41];
      ret[114] = _t[50];
      ret[126] = _t[60];
      ret[138] = _t[71];
    }
    else if (*_tn == 28){
      ret[78] = 2*_t[27];
      ret[79] = _t[34];
      ret[80] = _t[42];
      ret[81] = _t[51];
      ret[82] = _t[61];
      ret[83] = _t[72];
      ret[90] = _t[34];
      ret[102] = _t[42];
      ret[114] = _t[51];
      ret[126] = _t[61];
      ret[138] = _t[72];
    }
    else if (*_tn == 29){
      ret[7] = _t[0];
      ret[19] = _t[1];
      ret[31] = _t[3];
      ret[43] = _t[6];
      ret[55] = _t[10];
      ret[67] = _t[15];
      ret[79] = _t[21];
      ret[84] = _t[0];
      ret[85] = _t[1];
      ret[86] = _t[3];
      ret[87] = _t[6];
      ret[88] = _t[10];
      ret[89] = _t[15];
      ret[90] = _t[21];
      ret[91] = 2*_t[28];
      ret[92] = _t[36];
      ret[93] = _t[45];
      ret[94] = _t[55];
      ret[95] = _t[66];
      ret[103] = _t[36];
      ret[115] = _t[45];
      ret[127] = _t[55];
      ret[139] = _t[66];
    }
    else if (*_tn == 30){
      ret[19] = _t[2];
      ret[31] = _t[4];
      ret[43] = _t[7];
      ret[55] = _t[11];
      ret[67] = _t[16];
      ret[79] = _t[22];
      ret[85] = _t[2];
      ret[86] = _t[4];
      ret[87] = _t[7];
      ret[88] = _t[11];
      ret[89] = _t[16];
      ret[90] = _t[22];
      ret[91] = 2*_t[29];
      ret[92] = _t[37];
      ret[93] = _t[46];
      ret[94] = _t[56];
      ret[95] = _t[67];
      ret[103] = _t[37];
      ret[115] = _t[46];
      ret[127] = _t[56];
      ret[139] = _t[67];
    }
    else if (*_tn == 31){
      ret[31] = _t[5];
      ret[43] = _t[8];
      ret[55] = _t[12];
      ret[67] = _t[17];
      ret[79] = _t[23];
      ret[86] = _t[5];
      ret[87] = _t[8];
      ret[88] = _t[12];
      ret[89] = _t[17];
      ret[90] = _t[23];
      ret[91] = 2*_t[30];
      ret[92] = _t[38];
      ret[93] = _t[47];
      ret[94] = _t[57];
      ret[95] = _t[68];
      ret[103] = _t[38];
      ret[115] = _t[47];
      ret[127] = _t[57];
      ret[139] = _t[68];
    }
    else if (*_tn == 32){
      ret[43] = _t[9];
      ret[55] = _t[13];
      ret[67] = _t[18];
      ret[79] = _t[24];
      ret[87] = _t[9];
      ret[88] = _t[13];
      ret[89] = _t[18];
      ret[90] = _t[24];
      ret[91] = 2*_t[31];
      ret[92] = _t[39];
      ret[93] = _t[48];
      ret[94] = _t[58];
      ret[95] = _t[69];
      ret[103] = _t[39];
      ret[115] = _t[48];
      ret[127] = _t[58];
      ret[139] = _t[69];
    }
    else if (*_tn == 33){
      ret[55] = _t[14];
      ret[67] = _t[19];
      ret[79] = _t[25];
      ret[88] = _t[14];
      ret[89] = _t[19];
      ret[90] = _t[25];
      ret[91] = 2*_t[32];
      ret[92] = _t[40];
      ret[93] = _t[49];
      ret[94] = _t[59];
      ret[95] = _t[70];
      ret[103] = _t[40];
      ret[115] = _t[49];
      ret[127] = _t[59];
      ret[139] = _t[70];
    }
    else if (*_tn == 34){
      ret[67] = _t[20];
      ret[79] = _t[26];
      ret[89] = _t[20];
      ret[90] = _t[26];
      ret[91] = 2*_t[33];
      ret[92] = _t[41];
      ret[93] = _t[50];
      ret[94] = _t[60];
      ret[95] = _t[71];
      ret[103] = _t[41];
      ret[115] = _t[50];
      ret[127] = _t[60];
      ret[139] = _t[71];
    }
    else if (*_tn == 35){
      ret[79] = _t[27];
      ret[90] = _t[27];
      ret[91] = 2*_t[34];
      ret[92] = _t[42];
      ret[93] = _t[51];
      ret[94] = _t[61];
      ret[95] = _t[72];
      ret[103] = _t[42];
      ret[115] = _t[51];
      ret[127] = _t[61];
      ret[139] = _t[72];
    }
    else if (*_tn == 36){
      ret[91] = 2*_t[35];
      ret[92] = _t[43];
      ret[93] = _t[52];
      ret[94] = _t[62];
      ret[95] = _t[73];
      ret[103] = _t[43];
      ret[115] = _t[52];
      ret[127] = _t[62];
      ret[139] = _t[73];
    }
    else if (*_tn == 37){
      ret[8] = _t[0];
      ret[20] = _t[1];
      ret[32] = _t[3];
      ret[44] = _t[6];
      ret[56] = _t[10];
      ret[68] = _t[15];
      ret[80] = _t[21];
      ret[92] = _t[28];
      ret[96] = _t[0];
      ret[97] = _t[1];
      ret[98] = _t[3];
      ret[99] = _t[6];
      ret[100] = _t[10];
      ret[101] = _t[15];
      ret[102] = _t[21];
      ret[103] = _t[28];
      ret[104] = 2*_t[36];
      ret[105] = _t[45];
      ret[106] = _t[55];
      ret[107] = _t[66];
      ret[116] = _t[45];
      ret[128] = _t[55];
      ret[140] = _t[66];
    }
    else if (*_tn == 38){
      ret[20] = _t[2];
      ret[32] = _t[4];
      ret[44] = _t[7];
      ret[56] = _t[11];
      ret[68] = _t[16];
      ret[80] = _t[22];
      ret[92] = _t[29];
      ret[97] = _t[2];
      ret[98] = _t[4];
      ret[99] = _t[7];
      ret[100] = _t[11];
      ret[101] = _t[16];
      ret[102] = _t[22];
      ret[103] = _t[29];
      ret[104] = 2*_t[37];
      ret[105] = _t[46];
      ret[106] = _t[56];
      ret[107] = _t[67];
      ret[116] = _t[46];
      ret[128] = _t[56];
      ret[140] = _t[67];
    }
    else if (*_tn == 39){
      ret[32] = _t[5];
      ret[44] = _t[8];
      ret[56] = _t[12];
      ret[68] = _t[17];
      ret[80] = _t[23];
      ret[92] = _t[30];
      ret[98] = _t[5];
      ret[99] = _t[8];
      ret[100] = _t[12];
      ret[101] = _t[17];
      ret[102] = _t[23];
      ret[103] = _t[30];
      ret[104] = 2*_t[38];
      ret[105] = _t[47];
      ret[106] = _t[57];
      ret[107] = _t[68];
      ret[116] = _t[47];
      ret[128] = _t[57];
      ret[140] = _t[68];
    }
    else if (*_tn == 40){
      ret[44] = _t[9];
      ret[56] = _t[13];
      ret[68] = _t[18];
      ret[80] = _t[24];
      ret[92] = _t[31];
      ret[99] = _t[9];
      ret[100] = _t[13];
      ret[101] = _t[18];
      ret[102] = _t[24];
      ret[103] = _t[31];
      ret[104] = 2*_t[39];
      ret[105] = _t[48];
      ret[106] = _t[58];
      ret[107] = _t[69];
      ret[116] = _t[48];
      ret[128] = _t[58];
      ret[140] = _t[69];
    }
    else if (*_tn == 41){
      ret[56] = _t[14];
      ret[68] = _t[19];
      ret[80] = _t[25];
      ret[92] = _t[32];
      ret[100] = _t[14];
      ret[101] = _t[19];
      ret[102] = _t[25];
      ret[103] = _t[32];
      ret[104] = 2*_t[40];
      ret[105] = _t[49];
      ret[106] = _t[59];
      ret[107] = _t[70];
      ret[116] = _t[49];
      ret[128] = _t[59];
      ret[140] = _t[70];
    }
    else if (*_tn == 42){
      ret[68] = _t[20];
      ret[80] = _t[26];
      ret[92] = _t[33];
      ret[101] = _t[20];
      ret[102] = _t[26];
      ret[103] = _t[33];
      ret[104] = 2*_t[41];
      ret[105] = _t[50];
      ret[106] = _t[60];
      ret[107] = _t[71];
      ret[116] = _t[50];
      ret[128] = _t[60];
      ret[140] = _t[71];
    }
    else if (*_tn == 43){
      ret[80] = _t[27];
      ret[92] = _t[34];
      ret[102] = _t[27];
      ret[103] = _t[34];
      ret[104] = 2*_t[42];
      ret[105] = _t[51];
      ret[106] = _t[61];
      ret[107] = _t[72];
      ret[116] = _t[51];
      ret[128] = _t[61];
      ret[140] = _t[72];
    }
    else if (*_tn == 44){
      ret[92] = _t[35];
      ret[103] = _t[35];
      ret[104] = 2*_t[43];
      ret[105] = _t[52];
      ret[106] = _t[62];
      ret[107] = _t[73];
      ret[116] = _t[52];
      ret[128] = _t[62];
      ret[140] = _t[73];
    }
    else if (*_tn == 45){
      ret[104] = 2*_t[44];
      ret[105] = _t[53];
      ret[106] = _t[63];
      ret[107] = _t[74];
      ret[116] = _t[53];
      ret[128] = _t[63];
      ret[140] = _t[74];
    }
    else if (*_tn == 46){
      ret[9] = _t[0];
      ret[21] = _t[1];
      ret[33] = _t[3];
      ret[45] = _t[6];
      ret[57] = _t[10];
      ret[69] = _t[15];
      ret[81] = _t[21];
      ret[93] = _t[28];
      ret[105] = _t[36];
      ret[108] = _t[0];
      ret[109] = _t[1];
      ret[110] = _t[3];
      ret[111] = _t[6];
      ret[112] = _t[10];
      ret[113] = _t[15];
      ret[114] = _t[21];
      ret[115] = _t[28];
      ret[116] = _t[36];
      ret[117] = 2*_t[45];
      ret[118] = _t[55];
      ret[119] = _t[66];
      ret[129] = _t[55];
      ret[141] = _t[66];
    }
    else if (*_tn == 47){
      ret[21] = _t[2];
      ret[33] = _t[4];
      ret[45] = _t[7];
      ret[57] = _t[11];
      ret[69] = _t[16];
      ret[81] = _t[22];
      ret[93] = _t[29];
      ret[105] = _t[37];
      ret[109] = _t[2];
      ret[110] = _t[4];
      ret[111] = _t[7];
      ret[112] = _t[11];
      ret[113] = _t[16];
      ret[114] = _t[22];
      ret[115] = _t[29];
      ret[116] = _t[37];
      ret[117] = 2*_t[46];
      ret[118] = _t[56];
      ret[119] = _t[67];
      ret[129] = _t[56];
      ret[141] = _t[67];
    }
    else if (*_tn == 48){
      ret[33] = _t[5];
      ret[45] = _t[8];
      ret[57] = _t[12];
      ret[69] = _t[17];
      ret[81] = _t[23];
      ret[93] = _t[30];
      ret[105] = _t[38];
      ret[110] = _t[5];
      ret[111] = _t[8];
      ret[112] = _t[12];
      ret[113] = _t[17];
      ret[114] = _t[23];
      ret[115] = _t[30];
      ret[116] = _t[38];
      ret[117] = 2*_t[47];
      ret[118] = _t[57];
      ret[119] = _t[68];
      ret[129] = _t[57];
      ret[141] = _t[68];
    }
    else if (*_tn == 49){
      ret[45] = _t[9];
      ret[57] = _t[13];
      ret[69] = _t[18];
      ret[81] = _t[24];
      ret[93] = _t[31];
      ret[105] = _t[39];
      ret[111] = _t[9];
      ret[112] = _t[13];
      ret[113] = _t[18];
      ret[114] = _t[24];
      ret[115] = _t[31];
      ret[116] = _t[39];
      ret[117] = 2*_t[48];
      ret[118] = _t[58];
      ret[119] = _t[69];
      ret[129] = _t[58];
      ret[141] = _t[69];
    }
    else if (*_tn == 50){
      ret[57] = _t[14];
      ret[69] = _t[19];
      ret[81] = _t[25];
      ret[93] = _t[32];
      ret[105] = _t[40];
      ret[112] = _t[14];
      ret[113] = _t[19];
      ret[114] = _t[25];
      ret[115] = _t[32];
      ret[116] = _t[40];
      ret[117] = 2*_t[49];
      ret[118] = _t[59];
      ret[119] = _t[70];
      ret[129] = _t[59];
      ret[141] = _t[70];
    }
    else if (*_tn == 51){
      ret[69] = _t[20];
      ret[81] = _t[26];
      ret[93] = _t[33];
      ret[105] = _t[41];
      ret[113] = _t[20];
      ret[114] = _t[26];
      ret[115] = _t[33];
      ret[116] = _t[41];
      ret[117] = 2*_t[50];
      ret[118] = _t[60];
      ret[119] = _t[71];
      ret[129] = _t[60];
      ret[141] = _t[71];
    }
    else if (*_tn == 52){
      ret[81] = _t[27];
      ret[93] = _t[34];
      ret[105] = _t[42];
      ret[114] = _t[27];
      ret[115] = _t[34];
      ret[116] = _t[42];
      ret[117] = 2*_t[51];
      ret[118] = _t[61];
      ret[119] = _t[72];
      ret[129] = _t[61];
      ret[141] = _t[72];
    }
    else if (*_tn == 53){
      ret[93] = _t[35];
      ret[105] = _t[43];
      ret[115] = _t[35];
      ret[116] = _t[43];
      ret[117] = 2*_t[52];
      ret[118] = _t[62];
      ret[119] = _t[73];
      ret[129] = _t[62];
      ret[141] = _t[73];
    }
    else if (*_tn == 54){
      ret[105] = _t[44];
      ret[116] = _t[44];
      ret[117] = 2*_t[53];
      ret[118] = _t[63];
      ret[119] = _t[74];
      ret[129] = _t[63];
      ret[141] = _t[74];
    }
    else if (*_tn == 55){
      ret[117] = 2*_t[54];
      ret[118] = _t[64];
      ret[119] = _t[75];
      ret[129] = _t[64];
      ret[141] = _t[75];
    }
    else if (*_tn == 56){
      ret[10] = _t[0];
      ret[22] = _t[1];
      ret[34] = _t[3];
      ret[46] = _t[6];
      ret[58] = _t[10];
      ret[70] = _t[15];
      ret[82] = _t[21];
      ret[94] = _t[28];
      ret[106] = _t[36];
      ret[118] = _t[45];
      ret[120] = _t[0];
      ret[121] = _t[1];
      ret[122] = _t[3];
      ret[123] = _t[6];
      ret[124] = _t[10];
      ret[125] = _t[15];
      ret[126] = _t[21];
      ret[127] = _t[28];
      ret[128] = _t[36];
      ret[129] = _t[45];
      ret[130] = 2*_t[55];
      ret[131] = _t[66];
      ret[142] = _t[66];
    }
    else if (*_tn == 57){
      ret[22] = _t[2];
      ret[34] = _t[4];
      ret[46] = _t[7];
      ret[58] = _t[11];
      ret[70] = _t[16];
      ret[82] = _t[22];
      ret[94] = _t[29];
      ret[106] = _t[37];
      ret[118] = _t[46];
      ret[121] = _t[2];
      ret[122] = _t[4];
      ret[123] = _t[7];
      ret[124] = _t[11];
      ret[125] = _t[16];
      ret[126] = _t[22];
      ret[127] = _t[29];
      ret[128] = _t[37];
      ret[129] = _t[46];
      ret[130] = 2*_t[56];
      ret[131] = _t[67];
      ret[142] = _t[67];
    }
    else if (*_tn == 58){
      ret[34] = _t[5];
      ret[46] = _t[8];
      ret[58] = _t[12];
      ret[70] = _t[17];
      ret[82] = _t[23];
      ret[94] = _t[30];
      ret[106] = _t[38];
      ret[118] = _t[47];
      ret[122] = _t[5];
      ret[123] = _t[8];
      ret[124] = _t[12];
      ret[125] = _t[17];
      ret[126] = _t[23];
      ret[127] = _t[30];
      ret[128] = _t[38];
      ret[129] = _t[47];
      ret[130] = 2*_t[57];
      ret[131] = _t[68];
      ret[142] = _t[68];
    }
    else if (*_tn == 59){
      ret[46] = _t[9];
      ret[58] = _t[13];
      ret[70] = _t[18];
      ret[82] = _t[24];
      ret[94] = _t[31];
      ret[106] = _t[39];
      ret[118] = _t[48];
      ret[123] = _t[9];
      ret[124] = _t[13];
      ret[125] = _t[18];
      ret[126] = _t[24];
      ret[127] = _t[31];
      ret[128] = _t[39];
      ret[129] = _t[48];
      ret[130] = 2*_t[58];
      ret[131] = _t[69];
      ret[142] = _t[69];
    }
    else if (*_tn == 60){
      ret[58] = _t[14];
      ret[70] = _t[19];
      ret[82] = _t[25];
      ret[94] = _t[32];
      ret[106] = _t[40];
      ret[118] = _t[49];
      ret[124] = _t[14];
      ret[125] = _t[19];
      ret[126] = _t[25];
      ret[127] = _t[32];
      ret[128] = _t[40];
      ret[129] = _t[49];
      ret[130] = 2*_t[59];
      ret[131] = _t[70];
      ret[142] = _t[70];
    }
    else if (*_tn == 61){
      ret[70] = _t[20];
      ret[82] = _t[26];
      ret[94] = _t[33];
      ret[106] = _t[41];
      ret[118] = _t[50];
      ret[125] = _t[20];
      ret[126] = _t[26];
      ret[127] = _t[33];
      ret[128] = _t[41];
      ret[129] = _t[50];
      ret[130] = 2*_t[60];
      ret[131] = _t[71];
      ret[142] = _t[71];
    }
    else if (*_tn == 62){
      ret[82] = _t[27];
      ret[94] = _t[34];
      ret[106] = _t[42];
      ret[118] = _t[51];
      ret[126] = _t[27];
      ret[127] = _t[34];
      ret[128] = _t[42];
      ret[129] = _t[51];
      ret[130] = 2*_t[61];
      ret[131] = _t[72];
      ret[142] = _t[72];
    }
    else if (*_tn == 63){
      ret[94] = _t[35];
      ret[106] = _t[43];
      ret[118] = _t[52];
      ret[127] = _t[35];
      ret[128] = _t[43];
      ret[129] = _t[52];
      ret[130] = 2*_t[62];
      ret[131] = _t[73];
      ret[142] = _t[73];
    }
    else if (*_tn == 64){
      ret[106] = _t[44];
      ret[118] = _t[53];
      ret[128] = _t[44];
      ret[129] = _t[53];
      ret[130] = 2*_t[63];
      ret[131] = _t[74];
      ret[142] = _t[74];
    }
    else if (*_tn == 65){
      ret[118] = _t[54];
      ret[129] = _t[54];
      ret[130] = 2*_t[64];
      ret[131] = _t[75];
      ret[142] = _t[75];
    }
    else if (*_tn == 66){
      ret[130] = 2*_t[65];
      ret[131] = _t[76];
      ret[142] = _t[76];
    }
    else if (*_tn == 67){
      ret[11] = _t[0];
      ret[23] = _t[1];
      ret[35] = _t[3];
      ret[47] = _t[6];
      ret[59] = _t[10];
      ret[71] = _t[15];
      ret[83] = _t[21];
      ret[95] = _t[28];
      ret[107] = _t[36];
      ret[119] = _t[45];
      ret[131] = _t[55];
      ret[132] = _t[0];
      ret[133] = _t[1];
      ret[134] = _t[3];
      ret[135] = _t[6];
      ret[136] = _t[10];
      ret[137] = _t[15];
      ret[138] = _t[21];
      ret[139] = _t[28];
      ret[140] = _t[36];
      ret[141] = _t[45];
      ret[142] = _t[55];
      ret[143] = 2*_t[66];
    }
    else if (*_tn == 68){
      ret[23] = _t[2];
      ret[35] = _t[4];
      ret[47] = _t[7];
      ret[59] = _t[11];
      ret[71] = _t[16];
      ret[83] = _t[22];
      ret[95] = _t[29];
      ret[107] = _t[37];
      ret[119] = _t[46];
      ret[131] = _t[56];
      ret[133] = _t[2];
      ret[134] = _t[4];
      ret[135] = _t[7];
      ret[136] = _t[11];
      ret[137] = _t[16];
      ret[138] = _t[22];
      ret[139] = _t[29];
      ret[140] = _t[37];
      ret[141] = _t[46];
      ret[142] = _t[56];
      ret[143] = 2*_t[67];
    }
    else if (*_tn == 69){
      ret[35] = _t[5];
      ret[47] = _t[8];
      ret[59] = _t[12];
      ret[71] = _t[17];
      ret[83] = _t[23];
      ret[95] = _t[30];
      ret[107] = _t[38];
      ret[119] = _t[47];
      ret[131] = _t[57];
      ret[134] = _t[5];
      ret[135] = _t[8];
      ret[136] = _t[12];
      ret[137] = _t[17];
      ret[138] = _t[23];
      ret[139] = _t[30];
      ret[140] = _t[38];
      ret[141] = _t[47];
      ret[142] = _t[57];
      ret[143] = 2*_t[68];
    }
    else if (*_tn == 70){
      ret[47] = _t[9];
      ret[59] = _t[13];
      ret[71] = _t[18];
      ret[83] = _t[24];
      ret[95] = _t[31];
      ret[107] = _t[39];
      ret[119] = _t[48];
      ret[131] = _t[58];
      ret[135] = _t[9];
      ret[136] = _t[13];
      ret[137] = _t[18];
      ret[138] = _t[24];
      ret[139] = _t[31];
      ret[140] = _t[39];
      ret[141] = _t[48];
      ret[142] = _t[58];
      ret[143] = 2*_t[69];
    }
    else if (*_tn == 71){
      ret[59] = _t[14];
      ret[71] = _t[19];
      ret[83] = _t[25];
      ret[95] = _t[32];
      ret[107] = _t[40];
      ret[119] = _t[49];
      ret[131] = _t[59];
      ret[136] = _t[14];
      ret[137] = _t[19];
      ret[138] = _t[25];
      ret[139] = _t[32];
      ret[140] = _t[40];
      ret[141] = _t[49];
      ret[142] = _t[59];
      ret[143] = 2*_t[70];
    }
    else if (*_tn == 72){
      ret[71] = _t[20];
      ret[83] = _t[26];
      ret[95] = _t[33];
      ret[107] = _t[41];
      ret[119] = _t[50];
      ret[131] = _t[60];
      ret[137] = _t[20];
      ret[138] = _t[26];
      ret[139] = _t[33];
      ret[140] = _t[41];
      ret[141] = _t[50];
      ret[142] = _t[60];
      ret[143] = 2*_t[71];
    }
    else if (*_tn == 73){
      ret[83] = _t[27];
      ret[95] = _t[34];
      ret[107] = _t[42];
      ret[119] = _t[51];
      ret[131] = _t[61];
      ret[138] = _t[27];
      ret[139] = _t[34];
      ret[140] = _t[42];
      ret[141] = _t[51];
      ret[142] = _t[61];
      ret[143] = 2*_t[72];
    }
    else if (*_tn == 74){
      ret[95] = _t[35];
      ret[107] = _t[43];
      ret[119] = _t[52];
      ret[131] = _t[62];
      ret[139] = _t[35];
      ret[140] = _t[43];
      ret[141] = _t[52];
      ret[142] = _t[62];
      ret[143] = 2*_t[73];
    }
    else if (*_tn == 75){
      ret[107] = _t[44];
      ret[119] = _t[53];
      ret[131] = _t[63];
      ret[140] = _t[44];
      ret[141] = _t[53];
      ret[142] = _t[63];
      ret[143] = 2*_t[74];
    }
    else if (*_tn == 76){
      ret[119] = _t[54];
      ret[131] = _t[64];
      ret[141] = _t[54];
      ret[142] = _t[64];
      ret[143] = 2*_t[75];
    }
    else if (*_tn == 77){
      ret[131] = _t[65];
      ret[142] = _t[65];
      ret[143] = 2*_t[76];
    }
    else if (*_tn == 78){
      ret[143] = 2*_t[77];
    }
    return;
  }

  return;
}
