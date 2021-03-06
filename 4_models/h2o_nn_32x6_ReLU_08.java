/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:42:36.311+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_32x6_ReLU_08

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_32x6_ReLU_08 > h2o_nn_32x6_ReLU_08.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_32x6_ReLU_08.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_32x6_ReLU_08", algorithm="deeplearning")
public class h2o_nn_32x6_ReLU_08 extends GenModel {
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
      /* Input */ h2o_nn_32x6_ReLU_08_Activation_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_08_Activation_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_08_Activation_2.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_32x6_ReLU_08_Bias_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_08_Bias_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_08_Bias_2.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_32x6_ReLU_08_Weight_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_08_Weight_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_08_Weight_2.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_32x6_ReLU_08.VALUES;
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
    /* Label */ h2o_nn_32x6_ReLU_08_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.24528301886792453,0.12264150943396226,0.12264150943396226,0.1320754716981132,0.24528301886792453,0.1320754716981132};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_32x6_ReLU_08() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(-7768151040173402056L); }

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
class h2o_nn_32x6_ReLU_08_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_32x6_ReLU_08_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_08_Activation_0_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_08_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_ReLU_08_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_08_Activation_1_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_08_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_32x6_ReLU_08_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_08_Activation_2_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_08_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Rectifier layer
class h2o_nn_32x6_ReLU_08_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_ReLU_08_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_08_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.4874186986506084;
      sa[1] = 0.5669937502262598;
      sa[2] = 0.3720244365146322;
      sa[3] = 0.5205925167219005;
      sa[4] = 0.47644290391459226;
      sa[5] = 0.5285677498035467;
      sa[6] = 0.5049272114901623;
      sa[7] = 0.5187971846441389;
      sa[8] = 0.46158178896953717;
      sa[9] = 0.5505748083844095;
      sa[10] = 0.5503149094183568;
      sa[11] = 0.5141420549696717;
      sa[12] = 0.4571912900370931;
      sa[13] = 0.46790663016459716;
      sa[14] = 0.4860281675162854;
      sa[15] = 0.5202924666039612;
      sa[16] = 0.5269476617109542;
      sa[17] = 0.5494949103913119;
      sa[18] = 0.5818217827326477;
      sa[19] = 0.5010198273712353;
      sa[20] = 0.5383228046111579;
      sa[21] = 0.5173686360316133;
      sa[22] = 0.45804212069825784;
      sa[23] = 0.5391692201321558;
      sa[24] = 0.5132285350274844;
      sa[25] = 0.5134422159481749;
      sa[26] = 0.5024821633455845;
      sa[27] = 0.2551282977469905;
      sa[28] = 0.44792954945918284;
      sa[29] = 0.5295193664565557;
      sa[30] = 0.5424254082494928;
      sa[31] = 0.5960822748271747;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_32x6_ReLU_08_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_32x6_ReLU_08_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_08_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.00210475112174911;
      sa[1] = -0.03354144059261626;
      sa[2] = -0.005309013939328626;
      sa[3] = 0.004428572279769714;
      sa[4] = -0.004271280666939827;
      sa[5] = -0.04694305343576539;
    }
  }
}
class h2o_nn_32x6_ReLU_08_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Rectifier layer
class h2o_nn_32x6_ReLU_08_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[416];
  static {
    h2o_nn_32x6_ReLU_08_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_08_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.08604428f;
      sa[1] = 0.23164256f;
      sa[2] = 0.18079263f;
      sa[3] = 0.086611114f;
      sa[4] = -0.26282933f;
      sa[5] = -0.14330842f;
      sa[6] = -0.029584974f;
      sa[7] = 0.2718094f;
      sa[8] = 0.15842661f;
      sa[9] = -0.14774793f;
      sa[10] = -0.14665411f;
      sa[11] = -0.10674463f;
      sa[12] = -0.045932733f;
      sa[13] = -0.3230413f;
      sa[14] = -0.115200974f;
      sa[15] = 0.03046516f;
      sa[16] = 0.057214066f;
      sa[17] = -0.0015510329f;
      sa[18] = -0.18965144f;
      sa[19] = 0.30102292f;
      sa[20] = 0.27215835f;
      sa[21] = -0.34873405f;
      sa[22] = 0.34089708f;
      sa[23] = -0.41606578f;
      sa[24] = 0.16279003f;
      sa[25] = 0.40154558f;
      sa[26] = -0.31803435f;
      sa[27] = 0.48110977f;
      sa[28] = 0.20772564f;
      sa[29] = -0.1797479f;
      sa[30] = -0.08822072f;
      sa[31] = 0.102605715f;
      sa[32] = 0.22853091f;
      sa[33] = -0.34524927f;
      sa[34] = 0.10553038f;
      sa[35] = -0.15438713f;
      sa[36] = 0.17111494f;
      sa[37] = 0.45827723f;
      sa[38] = -0.1655501f;
      sa[39] = 0.25613347f;
      sa[40] = 0.34918103f;
      sa[41] = -0.06795004f;
      sa[42] = -0.12386615f;
      sa[43] = 0.07460293f;
      sa[44] = 0.30471015f;
      sa[45] = -0.47855198f;
      sa[46] = 0.06432504f;
      sa[47] = 0.044284903f;
      sa[48] = 0.26911175f;
      sa[49] = 0.17589688f;
      sa[50] = 0.24227491f;
      sa[51] = -0.30597505f;
      sa[52] = -0.041277178f;
      sa[53] = 0.28534716f;
      sa[54] = 0.19826744f;
      sa[55] = -0.12767878f;
      sa[56] = -0.3869893f;
      sa[57] = -0.18131143f;
      sa[58] = 0.14439638f;
      sa[59] = -0.2350024f;
      sa[60] = -0.2980757f;
      sa[61] = 0.23982392f;
      sa[62] = 0.14599739f;
      sa[63] = 0.05259015f;
      sa[64] = 0.1656614f;
      sa[65] = 0.10302031f;
      sa[66] = -0.08763239f;
      sa[67] = 0.13504048f;
      sa[68] = -0.20283172f;
      sa[69] = -0.046870444f;
      sa[70] = -0.006806853f;
      sa[71] = 0.24608836f;
      sa[72] = -0.15836255f;
      sa[73] = 0.266398f;
      sa[74] = -0.12818474f;
      sa[75] = -0.0709494f;
      sa[76] = -0.34895316f;
      sa[77] = 0.13380204f;
      sa[78] = -0.10711475f;
      sa[79] = -0.09496933f;
      sa[80] = -0.2593889f;
      sa[81] = -0.13410205f;
      sa[82] = -0.0533761f;
      sa[83] = 0.079843484f;
      sa[84] = 0.091559425f;
      sa[85] = -0.20574619f;
      sa[86] = 0.17370068f;
      sa[87] = 0.2691257f;
      sa[88] = 0.36623183f;
      sa[89] = -0.00801213f;
      sa[90] = -0.052775882f;
      sa[91] = 0.13944508f;
      sa[92] = 0.23147005f;
      sa[93] = -0.18187918f;
      sa[94] = 0.022112926f;
      sa[95] = -0.05159551f;
      sa[96] = 0.029173324f;
      sa[97] = 0.02946358f;
      sa[98] = 0.2704567f;
      sa[99] = -0.15321574f;
      sa[100] = -0.0773196f;
      sa[101] = -0.2536834f;
      sa[102] = -0.30471894f;
      sa[103] = -0.2238866f;
      sa[104] = 0.035732087f;
      sa[105] = -0.29300946f;
      sa[106] = -0.36678538f;
      sa[107] = 0.1626086f;
      sa[108] = 0.23009709f;
      sa[109] = -0.2733866f;
      sa[110] = -0.15707284f;
      sa[111] = 0.19374137f;
      sa[112] = 0.20761847f;
      sa[113] = -0.041177966f;
      sa[114] = 0.04393111f;
      sa[115] = -0.22973067f;
      sa[116] = 0.025535261f;
      sa[117] = -0.03816509f;
      sa[118] = -0.15522772f;
      sa[119] = -0.16329627f;
      sa[120] = 0.17047821f;
      sa[121] = 0.15147881f;
      sa[122] = 0.4461776f;
      sa[123] = 0.31619033f;
      sa[124] = 0.29024774f;
      sa[125] = 0.31233078f;
      sa[126] = -0.20904498f;
      sa[127] = -0.5248859f;
      sa[128] = 0.14965081f;
      sa[129] = -0.02640176f;
      sa[130] = -0.13640043f;
      sa[131] = 0.5531622f;
      sa[132] = 0.24436624f;
      sa[133] = -0.10761091f;
      sa[134] = 0.28737172f;
      sa[135] = -0.3536133f;
      sa[136] = 0.04561895f;
      sa[137] = -0.026577517f;
      sa[138] = 0.1182593f;
      sa[139] = -0.18943991f;
      sa[140] = 0.031711847f;
      sa[141] = -0.20656346f;
      sa[142] = 0.154657f;
      sa[143] = -0.3607181f;
      sa[144] = 0.292232f;
      sa[145] = -0.15895212f;
      sa[146] = -0.31598568f;
      sa[147] = 0.06850348f;
      sa[148] = 0.236922f;
      sa[149] = 0.41512656f;
      sa[150] = 0.3351106f;
      sa[151] = -0.28978956f;
      sa[152] = 0.024935097f;
      sa[153] = 0.26793206f;
      sa[154] = 0.11591038f;
      sa[155] = -0.048847876f;
      sa[156] = 0.16897218f;
      sa[157] = -0.24311225f;
      sa[158] = 0.09756579f;
      sa[159] = 0.19256578f;
      sa[160] = 0.25942716f;
      sa[161] = 0.13874307f;
      sa[162] = 0.04563467f;
      sa[163] = -0.103237964f;
      sa[164] = 0.26138073f;
      sa[165] = -0.15087478f;
      sa[166] = 0.09518516f;
      sa[167] = -0.014487349f;
      sa[168] = 0.006929232f;
      sa[169] = 0.37564635f;
      sa[170] = -0.024298543f;
      sa[171] = -0.07615685f;
      sa[172] = -0.12897618f;
      sa[173] = -0.18448207f;
      sa[174] = -0.005143227f;
      sa[175] = -0.12853093f;
      sa[176] = -0.07737612f;
      sa[177] = -0.046772096f;
      sa[178] = -0.0537042f;
      sa[179] = 0.30786338f;
      sa[180] = 0.001810898f;
      sa[181] = -0.33927616f;
      sa[182] = -0.1604702f;
      sa[183] = 0.05263144f;
      sa[184] = 0.33434823f;
      sa[185] = 0.29738805f;
      sa[186] = -0.26207167f;
      sa[187] = 0.004568462f;
      sa[188] = -0.042757507f;
      sa[189] = 0.2798643f;
      sa[190] = 0.054106437f;
      sa[191] = -0.11655561f;
      sa[192] = -0.03233191f;
      sa[193] = 0.17006794f;
      sa[194] = 0.2670514f;
      sa[195] = 0.34168115f;
      sa[196] = 0.24207772f;
      sa[197] = -0.043042682f;
      sa[198] = 0.060279127f;
      sa[199] = -0.079969116f;
      sa[200] = 0.038377583f;
      sa[201] = -0.497897f;
      sa[202] = 0.14911325f;
      sa[203] = -0.03416367f;
      sa[204] = -0.13396782f;
      sa[205] = 0.28158388f;
      sa[206] = 0.44804338f;
      sa[207] = -0.063929476f;
      sa[208] = -0.34726417f;
      sa[209] = -0.18586466f;
      sa[210] = -0.3445363f;
      sa[211] = -0.46591154f;
      sa[212] = 0.06756316f;
      sa[213] = -0.18468791f;
      sa[214] = -0.10959318f;
      sa[215] = 0.07164538f;
      sa[216] = 0.12197762f;
      sa[217] = 0.2928532f;
      sa[218] = -0.44624487f;
      sa[219] = 0.24587795f;
      sa[220] = -0.27195904f;
      sa[221] = 0.05375676f;
      sa[222] = 0.3312757f;
      sa[223] = 0.12397811f;
      sa[224] = 0.23693384f;
      sa[225] = 0.47085616f;
      sa[226] = -0.30990416f;
      sa[227] = 0.60326946f;
      sa[228] = 0.14076872f;
      sa[229] = 0.08534838f;
      sa[230] = 0.1917138f;
      sa[231] = -0.20323808f;
      sa[232] = -0.2575661f;
      sa[233] = -0.11698407f;
      sa[234] = 0.17847842f;
      sa[235] = -0.047294095f;
      sa[236] = 0.061614536f;
      sa[237] = -0.26444617f;
      sa[238] = -0.30343333f;
      sa[239] = 0.24439724f;
      sa[240] = -0.08348665f;
      sa[241] = -0.23588184f;
      sa[242] = -0.09547289f;
      sa[243] = -0.19499886f;
      sa[244] = -0.53145814f;
      sa[245] = -0.16663899f;
      sa[246] = -0.46809933f;
      sa[247] = -0.06520412f;
      sa[248] = 0.21667187f;
      sa[249] = 0.30418706f;
      sa[250] = -0.18096244f;
      sa[251] = -0.051054604f;
      sa[252] = -0.42128107f;
      sa[253] = -0.29574516f;
      sa[254] = -0.27299696f;
      sa[255] = 0.23729166f;
      sa[256] = -0.2815719f;
      sa[257] = 0.46982828f;
      sa[258] = -0.39829633f;
      sa[259] = 0.08990357f;
      sa[260] = -0.34287125f;
      sa[261] = -0.32873666f;
      sa[262] = -0.106838144f;
      sa[263] = 0.32887527f;
      sa[264] = -0.20050998f;
      sa[265] = -0.007152606f;
      sa[266] = -0.18766493f;
      sa[267] = -0.2789891f;
      sa[268] = 0.08256689f;
      sa[269] = -0.20233798f;
      sa[270] = -0.080374405f;
      sa[271] = 0.1460728f;
      sa[272] = -0.18850905f;
      sa[273] = -0.173367f;
      sa[274] = 0.09745942f;
      sa[275] = 4.5113487E-5f;
      sa[276] = 0.088020004f;
      sa[277] = -0.35822266f;
      sa[278] = -0.082906924f;
      sa[279] = 0.057294585f;
      sa[280] = -0.18901086f;
      sa[281] = 0.34291855f;
      sa[282] = -0.103266165f;
      sa[283] = -0.1568557f;
      sa[284] = -0.06931816f;
      sa[285] = 0.034728713f;
      sa[286] = 0.34621415f;
      sa[287] = 0.1765923f;
      sa[288] = -0.2722842f;
      sa[289] = -0.17274155f;
      sa[290] = 0.069353975f;
      sa[291] = -0.009814686f;
      sa[292] = -0.028392535f;
      sa[293] = -0.1078154f;
      sa[294] = 0.11618871f;
      sa[295] = 0.122275166f;
      sa[296] = 0.42776513f;
      sa[297] = 0.38810351f;
      sa[298] = -0.12142604f;
      sa[299] = -0.018952958f;
      sa[300] = 0.02431765f;
      sa[301] = -0.02961828f;
      sa[302] = 0.15979837f;
      sa[303] = 0.0457532f;
      sa[304] = -0.24097647f;
      sa[305] = -0.17532988f;
      sa[306] = 0.24758194f;
      sa[307] = -0.20159088f;
      sa[308] = 0.18126272f;
      sa[309] = 0.10937076f;
      sa[310] = 0.5236595f;
      sa[311] = 0.0032239016f;
      sa[312] = -0.20146924f;
      sa[313] = -0.24370456f;
      sa[314] = -0.25025856f;
      sa[315] = -0.025549581f;
      sa[316] = -0.048137434f;
      sa[317] = -0.059394073f;
      sa[318] = 0.067109905f;
      sa[319] = 0.3478214f;
      sa[320] = 0.012791196f;
      sa[321] = 0.11442375f;
      sa[322] = 0.3887572f;
      sa[323] = -0.34697834f;
      sa[324] = 0.36227575f;
      sa[325] = 0.3201369f;
      sa[326] = -0.4897358f;
      sa[327] = 0.040460236f;
      sa[328] = -0.015685199f;
      sa[329] = 0.35151818f;
      sa[330] = -0.30704612f;
      sa[331] = 0.3197015f;
      sa[332] = -0.31828302f;
      sa[333] = 0.1278964f;
      sa[334] = -0.17591307f;
      sa[335] = 0.42181402f;
      sa[336] = -0.23626329f;
      sa[337] = -0.23206234f;
      sa[338] = 0.30692205f;
      sa[339] = -0.2526325f;
      sa[340] = -0.37644672f;
      sa[341] = -0.3235053f;
      sa[342] = 0.102324165f;
      sa[343] = -0.030617265f;
      sa[344] = -0.33685744f;
      sa[345] = 0.25126642f;
      sa[346] = 0.267601f;
      sa[347] = -0.18860053f;
      sa[348] = 0.2459839f;
      sa[349] = -0.46531573f;
      sa[350] = 0.06398503f;
      sa[351] = 0.060767565f;
      sa[352] = 0.31059435f;
      sa[353] = -0.0728618f;
      sa[354] = 0.1791943f;
      sa[355] = 0.216249f;
      sa[356] = -0.26470256f;
      sa[357] = 0.35043824f;
      sa[358] = -0.42708105f;
      sa[359] = -0.07438869f;
      sa[360] = 0.082823195f;
      sa[361] = -0.08714924f;
      sa[362] = -0.12863491f;
      sa[363] = 0.12382948f;
      sa[364] = -0.09909413f;
      sa[365] = -0.21018597f;
      sa[366] = 5.53603E-4f;
      sa[367] = -0.18060939f;
      sa[368] = -0.022700144f;
      sa[369] = 0.22683643f;
      sa[370] = -0.3596195f;
      sa[371] = 0.15332107f;
      sa[372] = -0.019339148f;
      sa[373] = 0.28689945f;
      sa[374] = 0.19915009f;
      sa[375] = -0.13125098f;
      sa[376] = -0.39804742f;
      sa[377] = -0.2597028f;
      sa[378] = 0.4600535f;
      sa[379] = 0.15327346f;
      sa[380] = 0.036806516f;
      sa[381] = 0.07506377f;
      sa[382] = -0.1465442f;
      sa[383] = 0.17030859f;
      sa[384] = -0.18371682f;
      sa[385] = 0.07957984f;
      sa[386] = -0.330567f;
      sa[387] = -0.25640446f;
      sa[388] = 0.07893364f;
      sa[389] = -0.14889088f;
      sa[390] = -0.052636f;
      sa[391] = -0.26171967f;
      sa[392] = 0.31404695f;
      sa[393] = 0.12679821f;
      sa[394] = -0.24753773f;
      sa[395] = 0.44518927f;
      sa[396] = 0.25884572f;
      sa[397] = -0.31286603f;
      sa[398] = 0.15238665f;
      sa[399] = 0.20797448f;
      sa[400] = -0.060185824f;
      sa[401] = 0.21465604f;
      sa[402] = -0.15900303f;
      sa[403] = 0.11072823f;
      sa[404] = 0.43289033f;
      sa[405] = -0.19648848f;
      sa[406] = 0.08912167f;
      sa[407] = 0.3041074f;
      sa[408] = -0.34833232f;
      sa[409] = -0.33937836f;
      sa[410] = -0.081131436f;
      sa[411] = 0.21215485f;
      sa[412] = -0.18702531f;
      sa[413] = 0.099703036f;
      sa[414] = 0.14618969f;
      sa[415] = 0.22569016f;
    }
  }
}
// Neuron weights connecting Rectifier and Softmax layer
class h2o_nn_32x6_ReLU_08_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[192];
  static {
    h2o_nn_32x6_ReLU_08_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_08_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 1.2393812f;
      sa[1] = -0.12752186f;
      sa[2] = -0.51975864f;
      sa[3] = -0.88261056f;
      sa[4] = 0.07318606f;
      sa[5] = 0.20410517f;
      sa[6] = 1.2030871f;
      sa[7] = -0.6393968f;
      sa[8] = -0.9273511f;
      sa[9] = 0.6695036f;
      sa[10] = -0.33035854f;
      sa[11] = 0.43244633f;
      sa[12] = 0.20024836f;
      sa[13] = 0.115474135f;
      sa[14] = -1.2215164f;
      sa[15] = 1.0661033f;
      sa[16] = 1.6264089f;
      sa[17] = -0.56134546f;
      sa[18] = 0.1013984f;
      sa[19] = -0.5332765f;
      sa[20] = 1.1874218f;
      sa[21] = 0.02588691f;
      sa[22] = -1.0118018f;
      sa[23] = 0.6967687f;
      sa[24] = -1.3598459f;
      sa[25] = 0.43851668f;
      sa[26] = -0.22364314f;
      sa[27] = 1.3703015f;
      sa[28] = -1.3488276f;
      sa[29] = -0.4654314f;
      sa[30] = 0.46499756f;
      sa[31] = -1.0657306f;
      sa[32] = -1.1925513f;
      sa[33] = -1.490565f;
      sa[34] = 0.0043030214f;
      sa[35] = 1.3688604f;
      sa[36] = -0.43887943f;
      sa[37] = -0.16918147f;
      sa[38] = 0.14852352f;
      sa[39] = 0.7327283f;
      sa[40] = -0.7106146f;
      sa[41] = -1.3869342f;
      sa[42] = -1.0244211f;
      sa[43] = -1.4077798f;
      sa[44] = 0.0183605f;
      sa[45] = -1.3173474f;
      sa[46] = 1.1818758f;
      sa[47] = 1.4048291f;
      sa[48] = -1.3268495f;
      sa[49] = -1.0491345f;
      sa[50] = 0.32793704f;
      sa[51] = -0.30486682f;
      sa[52] = 0.80343294f;
      sa[53] = 0.8089299f;
      sa[54] = -0.22903062f;
      sa[55] = 1.1972922f;
      sa[56] = -1.0549284f;
      sa[57] = -1.0686717f;
      sa[58] = 0.17914765f;
      sa[59] = -0.2362781f;
      sa[60] = 1.3188763f;
      sa[61] = -0.6218635f;
      sa[62] = -0.39752463f;
      sa[63] = 0.83779985f;
      sa[64] = 1.4373487f;
      sa[65] = -0.66147345f;
      sa[66] = 0.67186636f;
      sa[67] = 1.6175218f;
      sa[68] = 0.70401996f;
      sa[69] = -1.0880489f;
      sa[70] = -1.5043105f;
      sa[71] = -0.4127942f;
      sa[72] = -1.2952845f;
      sa[73] = 1.1410632f;
      sa[74] = 1.3206631f;
      sa[75] = -0.06617773f;
      sa[76] = 0.85694796f;
      sa[77] = 0.52262706f;
      sa[78] = -1.2601374f;
      sa[79] = 1.0159972f;
      sa[80] = -0.74068457f;
      sa[81] = 0.27390122f;
      sa[82] = 0.8217597f;
      sa[83] = -0.6323446f;
      sa[84] = -1.5592929f;
      sa[85] = -1.2657167f;
      sa[86] = 0.28643602f;
      sa[87] = 1.257404f;
      sa[88] = -1.6573193f;
      sa[89] = -0.6187205f;
      sa[90] = -1.1088197f;
      sa[91] = 1.4913802f;
      sa[92] = -0.30920827f;
      sa[93] = 0.3194119f;
      sa[94] = -1.4992423f;
      sa[95] = -0.19202198f;
      sa[96] = -1.0759242f;
      sa[97] = 0.19234365f;
      sa[98] = -0.14228643f;
      sa[99] = 0.1716082f;
      sa[100] = -1.2054023f;
      sa[101] = -0.33905926f;
      sa[102] = 1.2770745f;
      sa[103] = 0.03960748f;
      sa[104] = -0.11646278f;
      sa[105] = -0.54233575f;
      sa[106] = 1.4832206f;
      sa[107] = 1.1278355f;
      sa[108] = 0.7246682f;
      sa[109] = 0.06434276f;
      sa[110] = 0.6436029f;
      sa[111] = -0.91450536f;
      sa[112] = -1.2023563f;
      sa[113] = 0.23142022f;
      sa[114] = 0.16287328f;
      sa[115] = -0.23650709f;
      sa[116] = 1.0853833f;
      sa[117] = 0.08894458f;
      sa[118] = -1.3621454f;
      sa[119] = -0.8469438f;
      sa[120] = -0.28247568f;
      sa[121] = 1.143243f;
      sa[122] = -0.068912424f;
      sa[123] = 0.39362276f;
      sa[124] = -0.7425569f;
      sa[125] = 0.7794579f;
      sa[126] = -0.99023104f;
      sa[127] = 0.4592187f;
      sa[128] = -0.84122777f;
      sa[129] = 0.7982003f;
      sa[130] = -0.47734058f;
      sa[131] = 0.8255824f;
      sa[132] = 0.4764557f;
      sa[133] = 0.61086977f;
      sa[134] = 0.64442533f;
      sa[135] = -1.0327338f;
      sa[136] = 0.44084156f;
      sa[137] = -0.5627977f;
      sa[138] = -0.05444641f;
      sa[139] = 1.044778f;
      sa[140] = -0.44287077f;
      sa[141] = -0.10598899f;
      sa[142] = 0.3278785f;
      sa[143] = -0.79608047f;
      sa[144] = 0.81541574f;
      sa[145] = -0.78566146f;
      sa[146] = 0.06709262f;
      sa[147] = -0.29233995f;
      sa[148] = -0.91197073f;
      sa[149] = 1.1030594f;
      sa[150] = -1.1525036f;
      sa[151] = -1.6173493f;
      sa[152] = 1.5805361f;
      sa[153] = -0.7016828f;
      sa[154] = 1.2981255f;
      sa[155] = -1.5116843f;
      sa[156] = -1.3899586f;
      sa[157] = -1.1460665f;
      sa[158] = -0.2734344f;
      sa[159] = -0.571125f;
      sa[160] = 1.2480413f;
      sa[161] = -1.0956187f;
      sa[162] = 1.4959757f;
      sa[163] = -0.7610683f;
      sa[164] = 0.6961554f;
      sa[165] = -0.56701744f;
      sa[166] = 1.3697573f;
      sa[167] = -1.4866241f;
      sa[168] = 0.9995281f;
      sa[169] = -1.5256503f;
      sa[170] = 1.0947313f;
      sa[171] = 0.8976013f;
      sa[172] = 1.4885441f;
      sa[173] = 1.4491202f;
      sa[174] = -0.16844071f;
      sa[175] = -0.005361011f;
      sa[176] = -0.8630373f;
      sa[177] = 0.062210336f;
      sa[178] = -1.1233118f;
      sa[179] = -0.27528495f;
      sa[180] = -1.4322658f;
      sa[181] = -0.9864976f;
      sa[182] = 0.4061521f;
      sa[183] = -0.94660187f;
      sa[184] = 1.474826f;
      sa[185] = 1.268197f;
      sa[186] = -0.48879626f;
      sa[187] = -1.0353689f;
      sa[188] = -0.52298146f;
      sa[189] = -0.81189066f;
      sa[190] = -1.4925642f;
      sa[191] = -0.98268896f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_32x6_ReLU_08 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_32x6_ReLU_08_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_32x6_ReLU_08_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_08_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[6];
  static {
    h2o_nn_32x6_ReLU_08_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_08_ColInfo_13_0 implements java.io.Serializable {
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


