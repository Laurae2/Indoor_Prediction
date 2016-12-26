/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:41:05.215+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_32x6_ReLU_01

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_32x6_ReLU_01 > h2o_nn_32x6_ReLU_01.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_32x6_ReLU_01.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_32x6_ReLU_01", algorithm="deeplearning")
public class h2o_nn_32x6_ReLU_01 extends GenModel {
  public hex.ModelCategory getModelCategory() { return hex.ModelCategory.Multinomial; }
  public boolean isSupervised() { return true; }
  public int nfeatures() { return 13; }
  public int nclasses() { return 5; }
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
  public static final int[] NEURONS = {13,32,5};
    // Thread-local storage for neuron activation values.
    final double[][] ACTIVATION = new double[][] {
      /* Input */ h2o_nn_32x6_ReLU_01_Activation_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_01_Activation_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_01_Activation_2.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_32x6_ReLU_01_Bias_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_01_Bias_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_01_Bias_2.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_32x6_ReLU_01_Weight_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_01_Weight_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_01_Weight_2.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_32x6_ReLU_01.VALUES;
  // Number of output classes included in training data response column.
  public static final int NCLASSES = 5;

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
    /* Label */ h2o_nn_32x6_ReLU_01_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.25,0.25,0.125,0.25,0.125};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_32x6_ReLU_01() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(-4003609716585956992L); }

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
          ACTIVATION[i][r] = Math.max(0, ACTIVATION[i][r]);
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
class h2o_nn_32x6_ReLU_01_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_32x6_ReLU_01_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_01_Activation_0_0 implements java.io.Serializable {
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
// Neuron activation values for Rectifier layer
class h2o_nn_32x6_ReLU_01_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_ReLU_01_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_01_Activation_1_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_01_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_32x6_ReLU_01_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_01_Activation_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.0;
      sa[1] = 0.0;
      sa[2] = 0.0;
      sa[3] = 0.0;
      sa[4] = 0.0;
    }
  }
}
// Neuron bias values for Input layer
class h2o_nn_32x6_ReLU_01_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Rectifier layer
class h2o_nn_32x6_ReLU_01_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_ReLU_01_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_01_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.4750054748029712;
      sa[1] = 0.5001571754455528;
      sa[2] = 0.4640051051673949;
      sa[3] = 0.5429322364050497;
      sa[4] = 0.4941105203955026;
      sa[5] = 0.49109408682831174;
      sa[6] = 0.49114190226501786;
      sa[7] = 0.5816762568107867;
      sa[8] = 0.49007134257869317;
      sa[9] = 0.49956323191966673;
      sa[10] = 0.4840043575665641;
      sa[11] = 0.5093501007674092;
      sa[12] = 0.5040327123492305;
      sa[13] = 0.45732697970927333;
      sa[14] = 0.5294029540016822;
      sa[15] = 0.5780713425538851;
      sa[16] = 0.4882708273437395;
      sa[17] = 0.5181504197701495;
      sa[18] = 0.505318220631048;
      sa[19] = 0.5075421305584157;
      sa[20] = 0.5509759783700494;
      sa[21] = 0.5265162338596826;
      sa[22] = 0.5103584869862685;
      sa[23] = 0.5180744359168828;
      sa[24] = 0.5012144847765523;
      sa[25] = 0.47249735825110883;
      sa[26] = 0.4965710006008774;
      sa[27] = 0.49888541790972984;
      sa[28] = 0.5667476540267594;
      sa[29] = 0.514026094430061;
      sa[30] = 0.4990739277319967;
      sa[31] = 0.4807846906493861;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_32x6_ReLU_01_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_32x6_ReLU_01_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_01_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = -0.02574927965974274;
      sa[1] = 0.030933116688784255;
      sa[2] = -0.012490891468919975;
      sa[3] = -0.009847104887364904;
      sa[4] = -0.03079845595763904;
    }
  }
}
class h2o_nn_32x6_ReLU_01_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Rectifier layer
class h2o_nn_32x6_ReLU_01_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[416];
  static {
    h2o_nn_32x6_ReLU_01_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_01_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.08230098f;
      sa[1] = 0.3586238f;
      sa[2] = 0.24866648f;
      sa[3] = 0.09664731f;
      sa[4] = -0.22652192f;
      sa[5] = -0.15321079f;
      sa[6] = -0.066444434f;
      sa[7] = 0.2898263f;
      sa[8] = 0.1922053f;
      sa[9] = -0.16296633f;
      sa[10] = -0.2463616f;
      sa[11] = -0.13339122f;
      sa[12] = 0.07451119f;
      sa[13] = -0.2760755f;
      sa[14] = -0.28133956f;
      sa[15] = -0.0242364f;
      sa[16] = 0.081682816f;
      sa[17] = 0.04282027f;
      sa[18] = -0.21584238f;
      sa[19] = 0.32378995f;
      sa[20] = 0.29230285f;
      sa[21] = -0.3585938f;
      sa[22] = 0.32084158f;
      sa[23] = -0.13854322f;
      sa[24] = 0.2521184f;
      sa[25] = 0.28349254f;
      sa[26] = -0.37336034f;
      sa[27] = 0.28494397f;
      sa[28] = 0.25854182f;
      sa[29] = -0.31362337f;
      sa[30] = -0.10312907f;
      sa[31] = 0.084221005f;
      sa[32] = 0.25613505f;
      sa[33] = -0.3072235f;
      sa[34] = 0.14206481f;
      sa[35] = -0.2133921f;
      sa[36] = 0.20794407f;
      sa[37] = 0.27394274f;
      sa[38] = -0.056903936f;
      sa[39] = 0.20498644f;
      sa[40] = -0.08203795f;
      sa[41] = -0.1406802f;
      sa[42] = -0.07957978f;
      sa[43] = 0.26184508f;
      sa[44] = 0.19105555f;
      sa[45] = -0.32828987f;
      sa[46] = 0.09154824f;
      sa[47] = 0.03859412f;
      sa[48] = 0.18217224f;
      sa[49] = 0.5806033f;
      sa[50] = 0.044643506f;
      sa[51] = -0.27673545f;
      sa[52] = -0.04135898f;
      sa[53] = 0.31263041f;
      sa[54] = 0.27402756f;
      sa[55] = -0.0116571905f;
      sa[56] = -0.10452735f;
      sa[57] = -0.34157246f;
      sa[58] = 0.24482481f;
      sa[59] = -0.2305261f;
      sa[60] = -0.31052297f;
      sa[61] = 0.20051318f;
      sa[62] = 0.19978441f;
      sa[63] = 0.005644421f;
      sa[64] = 0.13645175f;
      sa[65] = 0.18273622f;
      sa[66] = 0.103858136f;
      sa[67] = 0.16182521f;
      sa[68] = -0.09762564f;
      sa[69] = 0.13037816f;
      sa[70] = -0.091294385f;
      sa[71] = 0.258774f;
      sa[72] = -0.21156411f;
      sa[73] = 0.19413914f;
      sa[74] = -0.12624434f;
      sa[75] = 0.1120367f;
      sa[76] = -0.16939239f;
      sa[77] = -0.18196012f;
      sa[78] = -0.072929606f;
      sa[79] = 0.19577155f;
      sa[80] = -0.2533927f;
      sa[81] = -0.1417814f;
      sa[82] = -0.2361371f;
      sa[83] = 0.19102383f;
      sa[84] = 0.027254634f;
      sa[85] = -0.23492034f;
      sa[86] = 0.1542623f;
      sa[87] = 0.35797364f;
      sa[88] = 0.18822554f;
      sa[89] = 0.1322715f;
      sa[90] = -0.17546543f;
      sa[91] = 0.1062364f;
      sa[92] = 0.09261359f;
      sa[93] = -0.24685928f;
      sa[94] = 0.03245403f;
      sa[95] = -0.22584507f;
      sa[96] = 0.04949086f;
      sa[97] = -0.035460208f;
      sa[98] = 0.26493886f;
      sa[99] = -0.15724415f;
      sa[100] = -0.11463056f;
      sa[101] = -0.27642006f;
      sa[102] = -0.31303442f;
      sa[103] = -0.16743055f;
      sa[104] = 0.011797034f;
      sa[105] = -0.24574783f;
      sa[106] = -0.32185125f;
      sa[107] = 0.18489261f;
      sa[108] = 0.19853187f;
      sa[109] = -0.28298813f;
      sa[110] = -0.106710896f;
      sa[111] = 0.17572807f;
      sa[112] = 0.11837069f;
      sa[113] = 0.026619738f;
      sa[114] = -0.065838605f;
      sa[115] = -0.216926f;
      sa[116] = -0.16598493f;
      sa[117] = 0.01190377f;
      sa[118] = -0.23418306f;
      sa[119] = -0.15626363f;
      sa[120] = 0.15850079f;
      sa[121] = 0.22911848f;
      sa[122] = 0.4015924f;
      sa[123] = 0.33466175f;
      sa[124] = 0.32815945f;
      sa[125] = 0.38794947f;
      sa[126] = -0.2737528f;
      sa[127] = -0.2968597f;
      sa[128] = 0.18300162f;
      sa[129] = 0.20818378f;
      sa[130] = -0.186181f;
      sa[131] = 0.22640155f;
      sa[132] = 0.16168375f;
      sa[133] = -0.44131392f;
      sa[134] = 0.069655456f;
      sa[135] = -0.3211283f;
      sa[136] = -0.07930122f;
      sa[137] = 0.061296627f;
      sa[138] = 0.18299487f;
      sa[139] = -0.22080773f;
      sa[140] = -0.102451496f;
      sa[141] = -0.28003058f;
      sa[142] = 0.40101707f;
      sa[143] = -0.31668618f;
      sa[144] = 0.23097622f;
      sa[145] = -0.17354928f;
      sa[146] = -0.3545154f;
      sa[147] = 0.06140799f;
      sa[148] = 0.25051373f;
      sa[149] = 0.41311914f;
      sa[150] = 0.3753032f;
      sa[151] = -0.27682513f;
      sa[152] = 0.052396815f;
      sa[153] = 0.32984748f;
      sa[154] = 0.20767517f;
      sa[155] = -0.11350306f;
      sa[156] = 0.10554214f;
      sa[157] = -0.29665133f;
      sa[158] = 0.10716753f;
      sa[159] = 0.05482201f;
      sa[160] = 0.05101728f;
      sa[161] = 0.22298874f;
      sa[162] = -0.050347157f;
      sa[163] = -0.025571968f;
      sa[164] = 0.32433552f;
      sa[165] = -0.08970556f;
      sa[166] = -0.1387619f;
      sa[167] = -0.13491088f;
      sa[168] = 0.36935622f;
      sa[169] = 0.35641658f;
      sa[170] = -0.03105992f;
      sa[171] = -0.03297334f;
      sa[172] = -0.22810316f;
      sa[173] = -0.26106045f;
      sa[174] = 0.036605813f;
      sa[175] = -0.14758605f;
      sa[176] = -0.006165753f;
      sa[177] = 0.008435993f;
      sa[178] = -0.025513662f;
      sa[179] = 0.100499f;
      sa[180] = -0.07030436f;
      sa[181] = -0.2373718f;
      sa[182] = -0.17716716f;
      sa[183] = -6.3187414E-4f;
      sa[184] = 0.29098076f;
      sa[185] = 0.29711393f;
      sa[186] = -0.31797585f;
      sa[187] = 0.03411132f;
      sa[188] = -0.040325556f;
      sa[189] = 0.26432878f;
      sa[190] = 0.0021921943f;
      sa[191] = -0.08679928f;
      sa[192] = -0.10750979f;
      sa[193] = 0.18572639f;
      sa[194] = 0.08800958f;
      sa[195] = 0.38039452f;
      sa[196] = 0.41014498f;
      sa[197] = 0.046668902f;
      sa[198] = 0.09491502f;
      sa[199] = -0.010023114f;
      sa[200] = 0.03956172f;
      sa[201] = -0.5206955f;
      sa[202] = 0.1416169f;
      sa[203] = 0.0048531066f;
      sa[204] = -0.15601899f;
      sa[205] = 0.19568071f;
      sa[206] = 0.35659525f;
      sa[207] = 0.060502768f;
      sa[208] = -0.26782948f;
      sa[209] = -0.012152766f;
      sa[210] = -0.25583512f;
      sa[211] = -0.46703205f;
      sa[212] = 0.20856191f;
      sa[213] = -0.21621318f;
      sa[214] = -0.090967335f;
      sa[215] = 0.030901069f;
      sa[216] = 0.1360105f;
      sa[217] = 0.3282612f;
      sa[218] = -0.21000826f;
      sa[219] = 0.37252107f;
      sa[220] = -0.27896142f;
      sa[221] = 0.014547069f;
      sa[222] = 0.13337773f;
      sa[223] = 0.06351478f;
      sa[224] = 0.01790818f;
      sa[225] = 0.2821668f;
      sa[226] = -0.28901854f;
      sa[227] = 0.58006227f;
      sa[228] = 0.22549562f;
      sa[229] = 0.15979332f;
      sa[230] = 0.22711879f;
      sa[231] = -0.33908218f;
      sa[232] = -0.35409135f;
      sa[233] = 0.26713374f;
      sa[234] = 0.15899032f;
      sa[235] = -0.39311853f;
      sa[236] = -0.05518587f;
      sa[237] = -0.2583791f;
      sa[238] = -0.08888543f;
      sa[239] = 0.08156669f;
      sa[240] = 0.026993163f;
      sa[241] = -0.21708095f;
      sa[242] = -0.06255287f;
      sa[243] = -0.34461617f;
      sa[244] = -0.18593599f;
      sa[245] = -0.23717573f;
      sa[246] = -0.24383894f;
      sa[247] = -0.074890055f;
      sa[248] = 0.2811868f;
      sa[249] = 0.28488857f;
      sa[250] = -0.17506939f;
      sa[251] = -0.11210449f;
      sa[252] = -0.35141486f;
      sa[253] = -0.3814705f;
      sa[254] = -0.31430128f;
      sa[255] = 0.18511946f;
      sa[256] = -0.20567127f;
      sa[257] = 0.36086008f;
      sa[258] = -0.35184553f;
      sa[259] = -0.15536983f;
      sa[260] = -0.25486496f;
      sa[261] = -0.07559873f;
      sa[262] = -0.14427415f;
      sa[263] = 0.30538753f;
      sa[264] = -0.3901204f;
      sa[265] = 0.15814687f;
      sa[266] = -0.50815713f;
      sa[267] = -0.311544f;
      sa[268] = 0.111244984f;
      sa[269] = -0.08869237f;
      sa[270] = -0.1414947f;
      sa[271] = 0.3018334f;
      sa[272] = -0.2491882f;
      sa[273] = -0.12956291f;
      sa[274] = 0.15388082f;
      sa[275] = -0.016226824f;
      sa[276] = 0.15979122f;
      sa[277] = -0.27188385f;
      sa[278] = -0.11694955f;
      sa[279] = 0.052396655f;
      sa[280] = -0.2399611f;
      sa[281] = 0.28234437f;
      sa[282] = -0.08222872f;
      sa[283] = 2.0492825E-4f;
      sa[284] = 0.036861565f;
      sa[285] = -0.26024866f;
      sa[286] = 0.28402314f;
      sa[287] = 0.1610551f;
      sa[288] = -0.220075f;
      sa[289] = -0.1769451f;
      sa[290] = 0.08560796f;
      sa[291] = -0.04203631f;
      sa[292] = 0.023693316f;
      sa[293] = -0.090620354f;
      sa[294] = 0.12409016f;
      sa[295] = 0.08167924f;
      sa[296] = 0.31391612f;
      sa[297] = 0.22298631f;
      sa[298] = -0.06481941f;
      sa[299] = -0.0373687f;
      sa[300] = 0.0980552f;
      sa[301] = -0.015360449f;
      sa[302] = 0.17478731f;
      sa[303] = 0.073754914f;
      sa[304] = -0.23144248f;
      sa[305] = -0.22699334f;
      sa[306] = 0.23471363f;
      sa[307] = -0.16096304f;
      sa[308] = 0.108577214f;
      sa[309] = 0.06930932f;
      sa[310] = 0.4862803f;
      sa[311] = 0.20556346f;
      sa[312] = -0.2290354f;
      sa[313] = -0.28236532f;
      sa[314] = -0.24042003f;
      sa[315] = 0.029523917f;
      sa[316] = 0.052603576f;
      sa[317] = -0.1340795f;
      sa[318] = 0.23396052f;
      sa[319] = 0.3215441f;
      sa[320] = -0.091500886f;
      sa[321] = 0.15269855f;
      sa[322] = 0.3801355f;
      sa[323] = -0.37208256f;
      sa[324] = 0.1381214f;
      sa[325] = 0.3159186f;
      sa[326] = -0.3433935f;
      sa[327] = 0.01927907f;
      sa[328] = -0.13479862f;
      sa[329] = 0.085938595f;
      sa[330] = -0.138576f;
      sa[331] = 0.16425061f;
      sa[332] = -0.3011626f;
      sa[333] = 0.14864595f;
      sa[334] = -0.058609705f;
      sa[335] = 0.08974091f;
      sa[336] = -0.15267229f;
      sa[337] = -0.118178025f;
      sa[338] = 0.33983883f;
      sa[339] = -0.25602105f;
      sa[340] = -0.38877448f;
      sa[341] = -0.2360219f;
      sa[342] = 0.2426667f;
      sa[343] = -0.1165164f;
      sa[344] = -0.27725187f;
      sa[345] = 0.21097684f;
      sa[346] = 0.20242673f;
      sa[347] = -0.1874997f;
      sa[348] = 0.4463657f;
      sa[349] = -0.3901192f;
      sa[350] = -0.31337735f;
      sa[351] = 0.1349938f;
      sa[352] = 0.3146399f;
      sa[353] = 0.029482514f;
      sa[354] = -0.010192393f;
      sa[355] = -0.034037918f;
      sa[356] = -0.08726052f;
      sa[357] = 0.29493085f;
      sa[358] = -0.31755003f;
      sa[359] = 0.10421688f;
      sa[360] = 0.16026089f;
      sa[361] = -0.19297287f;
      sa[362] = -0.02367107f;
      sa[363] = 0.21305183f;
      sa[364] = -0.15555046f;
      sa[365] = -0.27059564f;
      sa[366] = -0.0065902295f;
      sa[367] = -0.1281879f;
      sa[368] = 0.0396671f;
      sa[369] = 0.21600887f;
      sa[370] = -0.3949089f;
      sa[371] = 0.15131822f;
      sa[372] = -0.044862833f;
      sa[373] = 0.27578413f;
      sa[374] = -0.13374712f;
      sa[375] = -0.35533825f;
      sa[376] = -0.420459f;
      sa[377] = -0.29141647f;
      sa[378] = 0.039682668f;
      sa[379] = 0.053414177f;
      sa[380] = -0.2209712f;
      sa[381] = -0.18932971f;
      sa[382] = -0.07858228f;
      sa[383] = 0.1242509f;
      sa[384] = -0.08715306f;
      sa[385] = 0.14763004f;
      sa[386] = -0.34284207f;
      sa[387] = -0.2591665f;
      sa[388] = -0.014774212f;
      sa[389] = 0.15390849f;
      sa[390] = 0.046146285f;
      sa[391] = 0.02153178f;
      sa[392] = 0.3503353f;
      sa[393] = 0.23266515f;
      sa[394] = -0.16209891f;
      sa[395] = 0.43095434f;
      sa[396] = 0.2263925f;
      sa[397] = -0.37910622f;
      sa[398] = 0.12092705f;
      sa[399] = 0.2228655f;
      sa[400] = 0.14012916f;
      sa[401] = 0.43634892f;
      sa[402] = -0.3352166f;
      sa[403] = 0.031095995f;
      sa[404] = 0.25316632f;
      sa[405] = -0.3400425f;
      sa[406] = 0.06775318f;
      sa[407] = 0.22939214f;
      sa[408] = -0.34468088f;
      sa[409] = -0.28758606f;
      sa[410] = -0.13125034f;
      sa[411] = 0.145221f;
      sa[412] = -0.29545146f;
      sa[413] = 0.1171817f;
      sa[414] = 0.13406028f;
      sa[415] = 0.3315763f;
    }
  }
}
// Neuron weights connecting Rectifier and Softmax layer
class h2o_nn_32x6_ReLU_01_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[160];
  static {
    h2o_nn_32x6_ReLU_01_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_01_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 1.3001262f;
      sa[1] = -0.21141581f;
      sa[2] = -0.45303825f;
      sa[3] = -0.8966812f;
      sa[4] = 0.09475512f;
      sa[5] = 0.17676285f;
      sa[6] = 1.2669681f;
      sa[7] = -0.6544409f;
      sa[8] = -1.0263287f;
      sa[9] = 0.58657026f;
      sa[10] = -0.17837612f;
      sa[11] = 0.4640551f;
      sa[12] = 0.15927993f;
      sa[13] = 0.119445056f;
      sa[14] = -1.2086693f;
      sa[15] = 1.1848934f;
      sa[16] = 1.6851776f;
      sa[17] = -0.5226058f;
      sa[18] = 0.026503406f;
      sa[19] = -0.48247474f;
      sa[20] = 1.2070049f;
      sa[21] = 0.027900431f;
      sa[22] = -1.001053f;
      sa[23] = 0.7529328f;
      sa[24] = -1.4801594f;
      sa[25] = 0.34113416f;
      sa[26] = -0.33097628f;
      sa[27] = 1.4999189f;
      sa[28] = -1.3792295f;
      sa[29] = -0.4173572f;
      sa[30] = 0.4797518f;
      sa[31] = -0.73732066f;
      sa[32] = -1.1121138f;
      sa[33] = -1.6100194f;
      sa[34] = -0.010207232f;
      sa[35] = 1.4183239f;
      sa[36] = -0.4279371f;
      sa[37] = -0.12312267f;
      sa[38] = 0.17413083f;
      sa[39] = 0.83475816f;
      sa[40] = -0.6802656f;
      sa[41] = -1.5292119f;
      sa[42] = -1.0380322f;
      sa[43] = -1.4477077f;
      sa[44] = 0.011701405f;
      sa[45] = -1.2912124f;
      sa[46] = 1.2349014f;
      sa[47] = 1.4434555f;
      sa[48] = -1.3196324f;
      sa[49] = -1.2091603f;
      sa[50] = 0.3916139f;
      sa[51] = -0.24236217f;
      sa[52] = 0.94142586f;
      sa[53] = 0.93052715f;
      sa[54] = -0.2513449f;
      sa[55] = 1.2123297f;
      sa[56] = -1.0866736f;
      sa[57] = -1.1004456f;
      sa[58] = 0.23932828f;
      sa[59] = -0.32358357f;
      sa[60] = 1.3990537f;
      sa[61] = -0.6143323f;
      sa[62] = -0.35861692f;
      sa[63] = 0.7795929f;
      sa[64] = 1.4446406f;
      sa[65] = -0.6172737f;
      sa[66] = 0.60735285f;
      sa[67] = 1.5785851f;
      sa[68] = 0.708146f;
      sa[69] = -0.9991681f;
      sa[70] = -1.5585622f;
      sa[71] = -0.44511393f;
      sa[72] = -1.273115f;
      sa[73] = 1.198513f;
      sa[74] = 1.3589798f;
      sa[75] = -0.09198558f;
      sa[76] = 0.9164513f;
      sa[77] = 0.48748907f;
      sa[78] = -1.3030411f;
      sa[79] = 0.9530282f;
      sa[80] = -0.7889429f;
      sa[81] = 0.29826233f;
      sa[82] = 0.80715024f;
      sa[83] = -0.6075177f;
      sa[84] = -1.8548498f;
      sa[85] = -1.3248198f;
      sa[86] = 0.25479132f;
      sa[87] = 1.2428054f;
      sa[88] = -1.5209166f;
      sa[89] = -0.49130994f;
      sa[90] = -1.0414765f;
      sa[91] = 1.5423064f;
      sa[92] = -0.33874893f;
      sa[93] = 0.27213225f;
      sa[94] = -1.5938466f;
      sa[95] = -0.18282102f;
      sa[96] = -1.1258012f;
      sa[97] = 0.25912526f;
      sa[98] = -0.09328679f;
      sa[99] = 0.23912004f;
      sa[100] = -1.2335341f;
      sa[101] = -0.38140324f;
      sa[102] = 1.2847747f;
      sa[103] = 0.03769682f;
      sa[104] = -0.1829785f;
      sa[105] = -0.5042001f;
      sa[106] = 1.4093759f;
      sa[107] = 1.1511987f;
      sa[108] = 0.74543774f;
      sa[109] = 0.053533763f;
      sa[110] = 0.6795917f;
      sa[111] = -0.88679177f;
      sa[112] = -1.1709635f;
      sa[113] = 0.19749245f;
      sa[114] = 0.23640828f;
      sa[115] = -0.39968127f;
      sa[116] = 1.1434774f;
      sa[117] = 0.06409229f;
      sa[118] = -1.3295686f;
      sa[119] = -0.81706476f;
      sa[120] = -0.28516334f;
      sa[121] = 1.1070268f;
      sa[122] = -0.106120765f;
      sa[123] = 0.37550414f;
      sa[124] = -0.73186654f;
      sa[125] = 0.7665942f;
      sa[126] = -0.9614244f;
      sa[127] = 0.3949917f;
      sa[128] = -0.9712188f;
      sa[129] = 0.73608387f;
      sa[130] = -0.44418615f;
      sa[131] = 0.9125581f;
      sa[132] = 0.4476455f;
      sa[133] = 0.61046875f;
      sa[134] = 0.6713015f;
      sa[135] = -1.1469656f;
      sa[136] = 0.53572816f;
      sa[137] = -0.6151313f;
      sa[138] = -0.12785876f;
      sa[139] = 1.173024f;
      sa[140] = -0.46437135f;
      sa[141] = -0.052836973f;
      sa[142] = 0.19306576f;
      sa[143] = -0.83313334f;
      sa[144] = 0.87243265f;
      sa[145] = -0.78335875f;
      sa[146] = 0.015681725f;
      sa[147] = -0.286875f;
      sa[148] = -1.0233805f;
      sa[149] = 1.0122027f;
      sa[150] = -1.069877f;
      sa[151] = -1.5656916f;
      sa[152] = 1.6167731f;
      sa[153] = -0.5586964f;
      sa[154] = 1.3492371f;
      sa[155] = -1.565658f;
      sa[156] = -1.4385695f;
      sa[157] = -1.235618f;
      sa[158] = -0.25229242f;
      sa[159] = -0.60185504f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_32x6_ReLU_01 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_32x6_ReLU_01_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_32x6_ReLU_01_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_01_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[5];
  static {
    h2o_nn_32x6_ReLU_01_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_01_ColInfo_13_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "1";
      sa[1] = "2";
      sa[2] = "4";
      sa[3] = "5";
      sa[4] = "6";
    }
  }
}

