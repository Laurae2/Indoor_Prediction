/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:42:52.607+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_32x6_Tanh_09

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_32x6_Tanh_09 > h2o_nn_32x6_Tanh_09.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_32x6_Tanh_09.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_32x6_Tanh_09", algorithm="deeplearning")
public class h2o_nn_32x6_Tanh_09 extends GenModel {
  public hex.ModelCategory getModelCategory() { return hex.ModelCategory.Multinomial; }
  public boolean isSupervised() { return true; }
  public int nfeatures() { return 13; }
  public int nclasses() { return 6; }
  // Thread-local storage for input neuron activation values.
  final double[] NUMS = new double[13];
  static class NORMMUL implements java.io.Serializable {
    public static final double[] VALUES = null;
}
  static class NORMSUB implements java.io.Serializable {
    public static final double[] VALUES = null;
}
  // Workspace for categorical offsets.
  public static final int[] CATOFFSETS = {0};
  // Number of neurons for each layer.
  public static final int[] NEURONS = {13,32,6};
    // Thread-local storage for neuron activation values.
    final double[][] ACTIVATION = new double[][] {
      /* Input */ h2o_nn_32x6_Tanh_09_Activation_0.VALUES,
      /* Tanh */ h2o_nn_32x6_Tanh_09_Activation_1.VALUES,
      /* Softmax */ h2o_nn_32x6_Tanh_09_Activation_2.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_32x6_Tanh_09_Bias_0.VALUES,
      /* Tanh */ h2o_nn_32x6_Tanh_09_Bias_1.VALUES,
      /* Softmax */ h2o_nn_32x6_Tanh_09_Bias_2.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_32x6_Tanh_09_Weight_0.VALUES,
      /* Tanh */ h2o_nn_32x6_Tanh_09_Weight_1.VALUES,
      /* Softmax */ h2o_nn_32x6_Tanh_09_Weight_2.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_32x6_Tanh_09.VALUES;
  // Number of output classes included in training data response column.
  public static final int NCLASSES = 6;

  // Column domains. The last array contains domain of response column.
  public static final String[][] DOMAINS = new String[][] {
    /* X5 */ null,
    /* X12 */ null,
    /* X15 */ null,
    /* X17 */ null,
    /* X18 */ null,
    /* X19 */ null,
    /* X20 */ null,
    /* X21 */ null,
    /* X23 */ null,
    /* X27 */ null,
    /* X28 */ null,
    /* X34 */ null,
    /* X35 */ null,
    /* Label */ h2o_nn_32x6_Tanh_09_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.25961538461538464,0.11538461538461539,0.11538461538461539,0.125,0.25961538461538464,0.125};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_32x6_Tanh_09() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(-9044536028205388592L); }

  // Pass in data in a double[], pre-aligned to the Model's requirements.
  // Jam predictions into the preds[] array; preds[0] is reserved for the
  // main prediction (class for classifiers or value for regression),
  // and remaining columns hold a probability distribution for classifiers.
  public final double[] score0( double[] data, double[] preds ) {
    java.util.Arrays.fill(preds,0);
    java.util.Arrays.fill(NUMS,0);
    int i = 0, ncats = 0;
    final int n = data.length;
    for(; i<n; ++i) {
      NUMS[i] = Double.isNaN(data[i]) ? 0 : data[i];
    }
    java.util.Arrays.fill(ACTIVATION[0],0);
    for (i=0; i<NUMS.length; ++i) {
      ACTIVATION[0][CATOFFSETS[CATOFFSETS.length-1] + i] = Double.isNaN(NUMS[i]) ? 0 : NUMS[i];
    }
    for (i=1; i<ACTIVATION.length; ++i) {
      java.util.Arrays.fill(ACTIVATION[i],0);
      int cols = ACTIVATION[i-1].length;
      int rows = ACTIVATION[i].length;
      int extra=cols-cols%8;
      int multiple = (cols/8)*8-1;
      int idx = 0;
      float[] a = WEIGHT[i];
      double[] x = ACTIVATION[i-1];
      double[] y = BIAS[i];
      double[] res = ACTIVATION[i];
      for (int row=0; row<rows; ++row) {
        double psum0 = 0, psum1 = 0, psum2 = 0, psum3 = 0, psum4 = 0, psum5 = 0, psum6 = 0, psum7 = 0;
        for (int col = 0; col < multiple; col += 8) {
          int off = idx + col;
          psum0 += a[off    ] * x[col    ];
          psum1 += a[off + 1] * x[col + 1];
          psum2 += a[off + 2] * x[col + 2];
          psum3 += a[off + 3] * x[col + 3];
          psum4 += a[off + 4] * x[col + 4];
          psum5 += a[off + 5] * x[col + 5];
          psum6 += a[off + 6] * x[col + 6];
          psum7 += a[off + 7] * x[col + 7];
        }
        res[row] += psum0 + psum1 + psum2 + psum3;
        res[row] += psum4 + psum5 + psum6 + psum7;
        for (int col = extra; col < cols; col++)
          res[row] += a[idx + col] * x[col];
        res[row] += y[row];
        idx += cols;
      }
      if (i<ACTIVATION.length-1) {
        for (int r=0; r<ACTIVATION[i].length; ++r) {
          ACTIVATION[i][r] = 1 - 2 / (1 + Math.exp(2*ACTIVATION[i][r]));
        }
      }
      if (i == ACTIVATION.length-1) {
        double max = ACTIVATION[i][0];
        for (int r=1; r<ACTIVATION[i].length; r++) {
          if (ACTIVATION[i][r]>max) max = ACTIVATION[i][r];
        }
        double scale = 0;
        for (int r=0; r<ACTIVATION[i].length; r++) {
          ACTIVATION[i][r] = Math.exp(ACTIVATION[i][r] - max);
          scale += ACTIVATION[i][r];
        }
        for (int r=0; r<ACTIVATION[i].length; r++) {
          if (Double.isNaN(ACTIVATION[i][r]))
            throw new RuntimeException("Numerical instability, predicted NaN.");
          ACTIVATION[i][r] /= scale;
          preds[r+1] = ACTIVATION[i][r];
        }
      }
    }
    preds[0] = hex.genmodel.GenModel.getPrediction(preds, PRIOR_CLASS_DISTRIB, data, 0.5);
    return preds;
  }
}
// Neuron activation values for Input layer
class h2o_nn_32x6_Tanh_09_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_32x6_Tanh_09_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_09_Activation_0_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.0;
      sa[1] = 0.0;
      sa[2] = 0.0;
      sa[3] = 0.0;
      sa[4] = 0.0;
      sa[5] = 0.0;
      sa[6] = 0.0;
      sa[7] = 0.0;
      sa[8] = 0.0;
      sa[9] = 0.0;
      sa[10] = 0.0;
      sa[11] = 0.0;
      sa[12] = 0.0;
    }
  }
}
// Neuron activation values for Tanh layer
class h2o_nn_32x6_Tanh_09_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_Tanh_09_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_09_Activation_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.0;
      sa[1] = 0.0;
      sa[2] = 0.0;
      sa[3] = 0.0;
      sa[4] = 0.0;
      sa[5] = 0.0;
      sa[6] = 0.0;
      sa[7] = 0.0;
      sa[8] = 0.0;
      sa[9] = 0.0;
      sa[10] = 0.0;
      sa[11] = 0.0;
      sa[12] = 0.0;
      sa[13] = 0.0;
      sa[14] = 0.0;
      sa[15] = 0.0;
      sa[16] = 0.0;
      sa[17] = 0.0;
      sa[18] = 0.0;
      sa[19] = 0.0;
      sa[20] = 0.0;
      sa[21] = 0.0;
      sa[22] = 0.0;
      sa[23] = 0.0;
      sa[24] = 0.0;
      sa[25] = 0.0;
      sa[26] = 0.0;
      sa[27] = 0.0;
      sa[28] = 0.0;
      sa[29] = 0.0;
      sa[30] = 0.0;
      sa[31] = 0.0;
    }
  }
}
// Neuron activation values for Softmax layer
class h2o_nn_32x6_Tanh_09_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_32x6_Tanh_09_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_09_Activation_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.0;
      sa[1] = 0.0;
      sa[2] = 0.0;
      sa[3] = 0.0;
      sa[4] = 0.0;
      sa[5] = 0.0;
    }
  }
}
// Neuron bias values for Input layer
class h2o_nn_32x6_Tanh_09_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Tanh layer
class h2o_nn_32x6_Tanh_09_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_Tanh_09_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_09_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.05186932479986468;
      sa[1] = 0.06608392922506634;
      sa[2] = -0.10034713075174884;
      sa[3] = -0.15332815983805523;
      sa[4] = -0.016082193147348017;
      sa[5] = 0.044072603595880416;
      sa[6] = 0.07192748980472952;
      sa[7] = -0.04300012195641489;
      sa[8] = -0.05450869225715856;
      sa[9] = 0.14127323044819629;
      sa[10] = 0.036251621904615144;
      sa[11] = 0.06075854422269798;
      sa[12] = -0.0050041679468380345;
      sa[13] = 0.023342538718973457;
      sa[14] = -0.07499043722263797;
      sa[15] = 0.02063024597700664;
      sa[16] = 0.10104949280156977;
      sa[17] = 5.90287554234105E-4;
      sa[18] = -0.017355234801173996;
      sa[19] = -0.04241382280566611;
      sa[20] = 0.18585570123307657;
      sa[21] = -0.017366971161690194;
      sa[22] = -0.03888876993861616;
      sa[23] = 0.008193253495856846;
      sa[24] = -0.05179879532410907;
      sa[25] = 0.04214804245405331;
      sa[26] = 0.008166980679328436;
      sa[27] = 0.05649684874669404;
      sa[28] = -0.12102558430819876;
      sa[29] = 0.04906977432514668;
      sa[30] = 0.04572942165905614;
      sa[31] = 0.02594363976477674;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_32x6_Tanh_09_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_32x6_Tanh_09_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_09_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.12934501815446103;
      sa[1] = -0.22178274236274123;
      sa[2] = -0.04382041926477065;
      sa[3] = 0.037201767243057265;
      sa[4] = -0.05077135816727776;
      sa[5] = -0.21603254308743386;
    }
  }
}
class h2o_nn_32x6_Tanh_09_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Tanh layer
class h2o_nn_32x6_Tanh_09_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[416];
  static {
    h2o_nn_32x6_Tanh_09_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_09_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.03728082f;
      sa[1] = 0.22594003f;
      sa[2] = 0.13388312f;
      sa[3] = 0.11300102f;
      sa[4] = -0.2883157f;
      sa[5] = -0.21742496f;
      sa[6] = -0.03941723f;
      sa[7] = 0.3115504f;
      sa[8] = 0.12742928f;
      sa[9] = -0.10570079f;
      sa[10] = -0.016810132f;
      sa[11] = -0.03999064f;
      sa[12] = -0.100649156f;
      sa[13] = -0.3534603f;
      sa[14] = -0.14986074f;
      sa[15] = -0.015200965f;
      sa[16] = 0.038135786f;
      sa[17] = -0.019450255f;
      sa[18] = -0.22579268f;
      sa[19] = 0.1289997f;
      sa[20] = 0.23789121f;
      sa[21] = -0.3671144f;
      sa[22] = 0.31631535f;
      sa[23] = -0.26764303f;
      sa[24] = 0.20721208f;
      sa[25] = 0.36344248f;
      sa[26] = -0.31762242f;
      sa[27] = 0.31560898f;
      sa[28] = 0.25637835f;
      sa[29] = -0.2181038f;
      sa[30] = -0.086196795f;
      sa[31] = 0.07267041f;
      sa[32] = 0.28656235f;
      sa[33] = -0.28934118f;
      sa[34] = 0.12845872f;
      sa[35] = -0.13777503f;
      sa[36] = 0.31285217f;
      sa[37] = 0.42951012f;
      sa[38] = -0.21404776f;
      sa[39] = 0.24258815f;
      sa[40] = 0.36936805f;
      sa[41] = 0.049781702f;
      sa[42] = -0.13120082f;
      sa[43] = 0.026339028f;
      sa[44] = 0.2718473f;
      sa[45] = -0.5624042f;
      sa[46] = 0.046932567f;
      sa[47] = 0.13000038f;
      sa[48] = 0.11617956f;
      sa[49] = -0.05728852f;
      sa[50] = 0.29750785f;
      sa[51] = -0.2878209f;
      sa[52] = -0.1212471f;
      sa[53] = 0.24704158f;
      sa[54] = 0.16605432f;
      sa[55] = -0.11750991f;
      sa[56] = -0.40776938f;
      sa[57] = -0.13730387f;
      sa[58] = 0.044727992f;
      sa[59] = -0.19944362f;
      sa[60] = -0.3462823f;
      sa[61] = 0.32459813f;
      sa[62] = 0.25819117f;
      sa[63] = 0.080803685f;
      sa[64] = 0.13701577f;
      sa[65] = 0.12792957f;
      sa[66] = -0.01517764f;
      sa[67] = 0.14833337f;
      sa[68] = -0.1547819f;
      sa[69] = 0.14219326f;
      sa[70] = -0.03673373f;
      sa[71] = 0.31519985f;
      sa[72] = -0.20860423f;
      sa[73] = 0.21519609f;
      sa[74] = -0.1655573f;
      sa[75] = 0.046793666f;
      sa[76] = -0.2882921f;
      sa[77] = 0.07830412f;
      sa[78] = -0.076830335f;
      sa[79] = -0.041494973f;
      sa[80] = -0.30919722f;
      sa[81] = -0.13408296f;
      sa[82] = 0.03716909f;
      sa[83] = 0.18001212f;
      sa[84] = 0.09915264f;
      sa[85] = -0.22978908f;
      sa[86] = 0.13197283f;
      sa[87] = 0.40264517f;
      sa[88] = 0.4444114f;
      sa[89] = -0.025022881f;
      sa[90] = -0.01905134f;
      sa[91] = 0.20380308f;
      sa[92] = 0.24584009f;
      sa[93] = -0.13973306f;
      sa[94] = -0.14302135f;
      sa[95] = -0.09422406f;
      sa[96] = 0.10170364f;
      sa[97] = 0.10143606f;
      sa[98] = 0.27142012f;
      sa[99] = -0.065588444f;
      sa[100] = -0.18002975f;
      sa[101] = -0.50406504f;
      sa[102] = -0.28686848f;
      sa[103] = -0.19529866f;
      sa[104] = 0.0011401927f;
      sa[105] = -0.2921661f;
      sa[106] = -0.32009956f;
      sa[107] = 0.2228949f;
      sa[108] = 0.24733008f;
      sa[109] = -0.23615344f;
      sa[110] = -0.1331053f;
      sa[111] = 0.18176116f;
      sa[112] = 0.12792593f;
      sa[113] = 0.06324674f;
      sa[114] = 0.1320323f;
      sa[115] = -0.16684796f;
      sa[116] = 0.016833872f;
      sa[117] = -0.02950148f;
      sa[118] = -0.17844652f;
      sa[119] = -0.20220782f;
      sa[120] = 0.07551885f;
      sa[121] = -0.003052914f;
      sa[122] = 0.36328864f;
      sa[123] = 0.22367956f;
      sa[124] = 0.31453463f;
      sa[125] = 0.3489f;
      sa[126] = -0.2816549f;
      sa[127] = -0.3524192f;
      sa[128] = 0.21411154f;
      sa[129] = 0.033162225f;
      sa[130] = -0.1575751f;
      sa[131] = 0.35434207f;
      sa[132] = 0.19611625f;
      sa[133] = -0.23023355f;
      sa[134] = -6.1205577E-4f;
      sa[135] = -0.45017245f;
      sa[136] = -0.21014896f;
      sa[137] = 0.016525682f;
      sa[138] = 0.16476874f;
      sa[139] = -0.17945759f;
      sa[140] = 0.02008609f;
      sa[141] = -0.25774628f;
      sa[142] = 0.20948784f;
      sa[143] = -0.39141604f;
      sa[144] = 0.24451645f;
      sa[145] = -0.21411698f;
      sa[146] = -0.33940023f;
      sa[147] = 0.0019871262f;
      sa[148] = 0.21544383f;
      sa[149] = 0.3163072f;
      sa[150] = 0.31887856f;
      sa[151] = -0.32513502f;
      sa[152] = 0.116041824f;
      sa[153] = 0.47139263f;
      sa[154] = 0.13618547f;
      sa[155] = -0.079519875f;
      sa[156] = 0.18894531f;
      sa[157] = -0.2778799f;
      sa[158] = 0.09394808f;
      sa[159] = 0.19676672f;
      sa[160] = 0.18701659f;
      sa[161] = 0.12144923f;
      sa[162] = -0.014731801f;
      sa[163] = -0.027157735f;
      sa[164] = 0.29777282f;
      sa[165] = -0.056437626f;
      sa[166] = 0.100923076f;
      sa[167] = -0.05205707f;
      sa[168] = 0.093394086f;
      sa[169] = 0.284453f;
      sa[170] = -0.16221887f;
      sa[171] = -0.15957695f;
      sa[172] = -0.026854506f;
      sa[173] = -0.34020624f;
      sa[174] = -0.0980873f;
      sa[175] = -0.10408122f;
      sa[176] = -0.024089469f;
      sa[177] = -0.13623026f;
      sa[178] = 0.046852596f;
      sa[179] = 0.50196785f;
      sa[180] = 0.05540998f;
      sa[181] = -0.36760983f;
      sa[182] = -0.11069097f;
      sa[183] = 0.048684895f;
      sa[184] = 0.38990337f;
      sa[185] = 0.32375753f;
      sa[186] = -0.14388059f;
      sa[187] = 0.09155704f;
      sa[188] = 0.0074647893f;
      sa[189] = 0.28125337f;
      sa[190] = 0.08834263f;
      sa[191] = -0.12595303f;
      sa[192] = -0.21601109f;
      sa[193] = 0.0031884273f;
      sa[194] = 0.25296935f;
      sa[195] = 0.36151695f;
      sa[196] = 0.3438525f;
      sa[197] = -0.022298707f;
      sa[198] = 0.075611785f;
      sa[199] = 0.024756687f;
      sa[200] = 0.03572233f;
      sa[201] = -0.42862898f;
      sa[202] = 0.18467568f;
      sa[203] = -0.021222943f;
      sa[204] = -0.18490294f;
      sa[205] = 0.13568512f;
      sa[206] = 0.43162468f;
      sa[207] = -0.074335046f;
      sa[208] = -0.3814967f;
      sa[209] = -0.11584478f;
      sa[210] = -0.4075909f;
      sa[211] = -0.51079476f;
      sa[212] = 0.059975196f;
      sa[213] = -0.21825324f;
      sa[214] = -0.2709879f;
      sa[215] = 0.036777224f;
      sa[216] = 0.058253508f;
      sa[217] = 0.33060822f;
      sa[218] = -0.20989065f;
      sa[219] = 0.29097733f;
      sa[220] = -0.25142375f;
      sa[221] = 0.028731164f;
      sa[222] = 0.22777466f;
      sa[223] = 0.054275215f;
      sa[224] = 0.20115171f;
      sa[225] = 0.34516308f;
      sa[226] = -0.4566095f;
      sa[227] = 0.5302062f;
      sa[228] = 0.1742605f;
      sa[229] = 0.10274905f;
      sa[230] = 0.22672422f;
      sa[231] = -0.1508313f;
      sa[232] = -0.30220944f;
      sa[233] = -0.00826286f;
      sa[234] = 0.14015691f;
      sa[235] = -0.09201005f;
      sa[236] = 0.060397834f;
      sa[237] = -0.40133205f;
      sa[238] = -0.5129215f;
      sa[239] = 0.124664396f;
      sa[240] = -0.12137115f;
      sa[241] = -0.29399467f;
      sa[242] = -0.034379546f;
      sa[243] = -0.32449576f;
      sa[244] = -0.5319752f;
      sa[245] = -0.14546226f;
      sa[246] = -0.45351887f;
      sa[247] = -0.052373312f;
      sa[248] = 0.27683893f;
      sa[249] = 0.35613194f;
      sa[250] = -0.1261308f;
      sa[251] = -0.04318715f;
      sa[252] = -0.36245462f;
      sa[253] = -0.2602626f;
      sa[254] = -0.30799842f;
      sa[255] = 0.23939757f;
      sa[256] = -0.28506097f;
      sa[257] = 0.32249257f;
      sa[258] = -0.5382378f;
      sa[259] = 0.13358973f;
      sa[260] = -0.18573236f;
      sa[261] = -0.20267086f;
      sa[262] = -0.130908f;
      sa[263] = 0.23849751f;
      sa[264] = -0.063524835f;
      sa[265] = 0.07170376f;
      sa[266] = -0.14055753f;
      sa[267] = -0.35165778f;
      sa[268] = 0.10560747f;
      sa[269] = -0.20706613f;
      sa[270] = -0.1945453f;
      sa[271] = 0.1757577f;
      sa[272] = -0.31849048f;
      sa[273] = -0.15228511f;
      sa[274] = 0.15815143f;
      sa[275] = 0.02242323f;
      sa[276] = 0.08009714f;
      sa[277] = -0.252407f;
      sa[278] = -0.017457457f;
      sa[279] = 0.17100996f;
      sa[280] = -0.24648279f;
      sa[281] = 0.32498023f;
      sa[282] = -0.1199399f;
      sa[283] = -0.168921f;
      sa[284] = -0.04862308f;
      sa[285] = -0.0024990214f;
      sa[286] = 0.320465f;
      sa[287] = 0.10569634f;
      sa[288] = -0.22662933f;
      sa[289] = -0.13857247f;
      sa[290] = -0.0081043085f;
      sa[291] = -0.027409935f;
      sa[292] = 0.12909387f;
      sa[293] = -0.03131931f;
      sa[294] = 0.13152498f;
      sa[295] = 0.120928146f;
      sa[296] = 0.41916627f;
      sa[297] = 0.3616362f;
      sa[298] = -0.20833807f;
      sa[299] = -0.009269522f;
      sa[300] = 0.047444113f;
      sa[301] = -0.04207298f;
      sa[302] = 0.17009458f;
      sa[303] = 0.105191074f;
      sa[304] = -0.26330557f;
      sa[305] = -0.07890236f;
      sa[306] = 0.27842036f;
      sa[307] = -0.16794842f;
      sa[308] = 0.09375191f;
      sa[309] = 0.02832582f;
      sa[310] = 0.4755826f;
      sa[311] = 0.06728868f;
      sa[312] = -0.27565205f;
      sa[313] = -0.2744931f;
      sa[314] = -0.21861428f;
      sa[315] = 0.02058825f;
      sa[316] = -0.056810927f;
      sa[317] = -0.0749275f;
      sa[318] = 0.081615716f;
      sa[319] = 0.29799116f;
      sa[320] = -0.08221557f;
      sa[321] = 0.18922205f;
      sa[322] = 0.44119433f;
      sa[323] = -0.30586025f;
      sa[324] = 0.29161552f;
      sa[325] = 0.2983358f;
      sa[326] = -0.5687128f;
      sa[327] = -0.11703859f;
      sa[328] = 0.03772639f;
      sa[329] = 0.34609175f;
      sa[330] = -0.32867473f;
      sa[331] = 0.6424413f;
      sa[332] = -0.31068587f;
      sa[333] = 0.0652435f;
      sa[334] = -0.065874234f;
      sa[335] = 0.6443568f;
      sa[336] = -0.257755f;
      sa[337] = -0.24112743f;
      sa[338] = 0.31163457f;
      sa[339] = -0.25148806f;
      sa[340] = -0.34344995f;
      sa[341] = -0.3213591f;
      sa[342] = 0.16435187f;
      sa[343] = -0.027235718f;
      sa[344] = -0.29783833f;
      sa[345] = 0.20459844f;
      sa[346] = 0.24079384f;
      sa[347] = -0.18851063f;
      sa[348] = 0.27425274f;
      sa[349] = -0.4116435f;
      sa[350] = -0.11387839f;
      sa[351] = 0.14239916f;
      sa[352] = 0.3034058f;
      sa[353] = -0.051247377f;
      sa[354] = 0.05860699f;
      sa[355] = 0.10775964f;
      sa[356] = -0.2349343f;
      sa[357] = 0.31783456f;
      sa[358] = -0.32793933f;
      sa[359] = 0.040694676f;
      sa[360] = 0.10159703f;
      sa[361] = -0.14786494f;
      sa[362] = -0.12739447f;
      sa[363] = 0.0050330716f;
      sa[364] = -0.08166071f;
      sa[365] = -0.23014107f;
      sa[366] = 0.039829873f;
      sa[367] = -0.22662993f;
      sa[368] = 0.036842138f;
      sa[369] = 0.27274886f;
      sa[370] = -0.19111823f;
      sa[371] = 0.18042104f;
      sa[372] = 0.015998673f;
      sa[373] = 0.25374573f;
      sa[374] = -0.19764574f;
      sa[375] = -0.24623877f;
      sa[376] = -0.45038334f;
      sa[377] = -0.21429572f;
      sa[378] = 0.42453164f;
      sa[379] = 0.12992917f;
      sa[380] = -0.070969746f;
      sa[381] = -0.097912915f;
      sa[382] = -0.18525179f;
      sa[383] = 0.05682578f;
      sa[384] = -0.14830096f;
      sa[385] = 0.1565464f;
      sa[386] = -0.37537923f;
      sa[387] = -0.32574403f;
      sa[388] = 0.08218562f;
      sa[389] = -0.11709116f;
      sa[390] = -0.04489675f;
      sa[391] = -0.11203618f;
      sa[392] = 0.25785506f;
      sa[393] = 0.14612514f;
      sa[394] = -0.112765744f;
      sa[395] = 0.48102456f;
      sa[396] = 0.28050193f;
      sa[397] = -0.37592906f;
      sa[398] = 0.08043333f;
      sa[399] = 0.21109071f;
      sa[400] = 0.13048013f;
      sa[401] = 0.30507427f;
      sa[402] = -0.14396772f;
      sa[403] = 0.19604889f;
      sa[404] = 0.39694586f;
      sa[405] = -0.152801f;
      sa[406] = -0.0033813582f;
      sa[407] = 0.37681025f;
      sa[408] = -0.33419985f;
      sa[409] = -0.20606521f;
      sa[410] = -0.089379646f;
      sa[411] = 0.27703598f;
      sa[412] = -0.3485544f;
      sa[413] = -0.29359296f;
      sa[414] = 0.105788626f;
      sa[415] = 0.27394783f;
    }
  }
}
// Neuron weights connecting Tanh and Softmax layer
class h2o_nn_32x6_Tanh_09_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[192];
  static {
    h2o_nn_32x6_Tanh_09_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_09_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 1.1218563f;
      sa[1] = -0.18601103f;
      sa[2] = -0.20009372f;
      sa[3] = -0.7918046f;
      sa[4] = 0.08927946f;
      sa[5] = 0.14453754f;
      sa[6] = 1.3471233f;
      sa[7] = -0.60308164f;
      sa[8] = -1.0233946f;
      sa[9] = 0.5329286f;
      sa[10] = -0.26506215f;
      sa[11] = 0.8716452f;
      sa[12] = -0.12934203f;
      sa[13] = 0.24355224f;
      sa[14] = -1.3378698f;
      sa[15] = 1.0503165f;
      sa[16] = 2.058914f;
      sa[17] = -0.52812177f;
      sa[18] = 0.14027101f;
      sa[19] = -0.5753102f;
      sa[20] = 1.2818637f;
      sa[21] = -0.027109345f;
      sa[22] = -0.60183287f;
      sa[23] = 0.7118469f;
      sa[24] = -1.4720049f;
      sa[25] = 0.53552055f;
      sa[26] = -0.17546904f;
      sa[27] = 1.4276035f;
      sa[28] = -1.2959043f;
      sa[29] = -0.49096525f;
      sa[30] = 0.51091856f;
      sa[31] = -0.7525413f;
      sa[32] = -1.1739358f;
      sa[33] = -1.4599795f;
      sa[34] = 0.029891316f;
      sa[35] = 1.4210687f;
      sa[36] = -0.42194536f;
      sa[37] = -0.044136062f;
      sa[38] = 0.00526148f;
      sa[39] = 0.777726f;
      sa[40] = -0.68443954f;
      sa[41] = -1.1910102f;
      sa[42] = -1.0437897f;
      sa[43] = -1.4035438f;
      sa[44] = 0.07317654f;
      sa[45] = -1.3644397f;
      sa[46] = 1.173216f;
      sa[47] = 1.465435f;
      sa[48] = -1.2966889f;
      sa[49] = -0.8784357f;
      sa[50] = 0.34692803f;
      sa[51] = -0.3870835f;
      sa[52] = 0.8460155f;
      sa[53] = 0.8321651f;
      sa[54] = -0.27552477f;
      sa[55] = 1.1545087f;
      sa[56] = -1.248877f;
      sa[57] = -1.0656117f;
      sa[58] = 0.14304933f;
      sa[59] = -0.17072561f;
      sa[60] = 1.3182594f;
      sa[61] = -0.50310653f;
      sa[62] = -0.34579575f;
      sa[63] = 0.7438835f;
      sa[64] = 1.529428f;
      sa[65] = -0.6801361f;
      sa[66] = 0.78820825f;
      sa[67] = 1.7181728f;
      sa[68] = 0.72208494f;
      sa[69] = -1.098245f;
      sa[70] = -1.5414302f;
      sa[71] = -0.38654006f;
      sa[72] = -1.2377754f;
      sa[73] = 1.0511409f;
      sa[74] = 1.366319f;
      sa[75] = -0.0669533f;
      sa[76] = 0.72270274f;
      sa[77] = 0.58232963f;
      sa[78] = -1.2372837f;
      sa[79] = 1.1795533f;
      sa[80] = -0.6670255f;
      sa[81] = 0.1948614f;
      sa[82] = 0.9217737f;
      sa[83] = -0.52706254f;
      sa[84] = -1.4921101f;
      sa[85] = -1.1683476f;
      sa[86] = 0.37820882f;
      sa[87] = 1.2505704f;
      sa[88] = -1.6545007f;
      sa[89] = -0.5826571f;
      sa[90] = -1.0478214f;
      sa[91] = 1.4780926f;
      sa[92] = -0.29969105f;
      sa[93] = 0.4428898f;
      sa[94] = -1.5367926f;
      sa[95] = -0.17258288f;
      sa[96] = -0.8933573f;
      sa[97] = 0.27612817f;
      sa[98] = -0.1728946f;
      sa[99] = -0.65085155f;
      sa[100] = -1.0081679f;
      sa[101] = -0.043063164f;
      sa[102] = 0.92953527f;
      sa[103] = 0.08486977f;
      sa[104] = -0.015433016f;
      sa[105] = -0.53549504f;
      sa[106] = 1.591863f;
      sa[107] = 1.0115447f;
      sa[108] = 0.72254974f;
      sa[109] = -0.14284134f;
      sa[110] = 0.71798456f;
      sa[111] = -0.90931445f;
      sa[112] = -1.3891008f;
      sa[113] = 0.61613536f;
      sa[114] = -0.028749144f;
      sa[115] = -0.018926207f;
      sa[116] = 1.062765f;
      sa[117] = 0.27758324f;
      sa[118] = -1.4667658f;
      sa[119] = -0.82347876f;
      sa[120] = -0.11031243f;
      sa[121] = 1.4113339f;
      sa[122] = -0.038392864f;
      sa[123] = 0.85171825f;
      sa[124] = -0.8664208f;
      sa[125] = 0.7634105f;
      sa[126] = -1.3391457f;
      sa[127] = 0.5341404f;
      sa[128] = -0.8503234f;
      sa[129] = 0.7797731f;
      sa[130] = -0.4644786f;
      sa[131] = 0.7431654f;
      sa[132] = 0.5308148f;
      sa[133] = 0.4900632f;
      sa[134] = 0.59848154f;
      sa[135] = -1.0467741f;
      sa[136] = 0.41130918f;
      sa[137] = -0.57116926f;
      sa[138] = 0.084093966f;
      sa[139] = 1.0618893f;
      sa[140] = -0.39350018f;
      sa[141] = -0.26627177f;
      sa[142] = 0.54665947f;
      sa[143] = -0.7181792f;
      sa[144] = 0.8618534f;
      sa[145] = -1.027112f;
      sa[146] = 0.009179209f;
      sa[147] = -0.19835824f;
      sa[148] = -0.8995689f;
      sa[149] = 1.0733843f;
      sa[150] = -1.2622006f;
      sa[151] = -1.5439466f;
      sa[152] = 1.6446611f;
      sa[153] = -0.92878157f;
      sa[154] = 1.2157501f;
      sa[155] = -1.5136367f;
      sa[156] = -1.396803f;
      sa[157] = -1.1110132f;
      sa[158] = -0.33725542f;
      sa[159] = -0.5078873f;
      sa[160] = 1.0141621f;
      sa[161] = -0.90380037f;
      sa[162] = 1.4906123f;
      sa[163] = -0.9073672f;
      sa[164] = 0.7553789f;
      sa[165] = -0.4773352f;
      sa[166] = 1.5239506f;
      sa[167] = -1.7643281f;
      sa[168] = 1.3159337f;
      sa[169] = -1.4482732f;
      sa[170] = 0.9253775f;
      sa[171] = 1.026047f;
      sa[172] = 1.5301048f;
      sa[173] = 1.4905051f;
      sa[174] = -0.2920606f;
      sa[175] = -0.02360277f;
      sa[176] = -0.82435745f;
      sa[177] = 0.22721311f;
      sa[178] = -1.6501144f;
      sa[179] = -0.43996605f;
      sa[180] = -1.4246354f;
      sa[181] = -1.0207216f;
      sa[182] = 0.61412543f;
      sa[183] = -0.64957273f;
      sa[184] = 1.6495136f;
      sa[185] = 1.5338054f;
      sa[186] = -0.48475498f;
      sa[187] = -1.017854f;
      sa[188] = -0.59476894f;
      sa[189] = -0.8749216f;
      sa[190] = -1.2921549f;
      sa[191] = -1.0209601f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_32x6_Tanh_09 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_32x6_Tanh_09_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_32x6_Tanh_09_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "X5";
      sa[1] = "X12";
      sa[2] = "X15";
      sa[3] = "X17";
      sa[4] = "X18";
      sa[5] = "X19";
      sa[6] = "X20";
      sa[7] = "X21";
      sa[8] = "X23";
      sa[9] = "X27";
      sa[10] = "X28";
      sa[11] = "X34";
      sa[12] = "X35";
    }
  }
}
// The class representing column Label
class h2o_nn_32x6_Tanh_09_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[6];
  static {
    h2o_nn_32x6_Tanh_09_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_09_ColInfo_13_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "1";
      sa[1] = "2";
      sa[2] = "3";
      sa[3] = "4";
      sa[4] = "5";
      sa[5] = "6";
    }
  }
}


